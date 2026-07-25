(** Draws one frame into a {!Framebuffer}, a column at a time, entirely in
    software.

    {1 Why software}

    The floor and ceiling are inclined {!Plane}s, so the surface a pixel shows —
    and how far away it is — differs from one pixel to the next even within a
    row. That cannot be expressed by blitting whole textures, so every pixel is
    computed by hand and the finished buffer is scaled up to the window by the
    GPU in a single copy.

    {1 A column}

    For each screen column we take the ray's horizontal direction and:

    - {b Cast the floor, and the ceiling or sky.} For every pixel below the
      horizon, {!Plane.view_distance} gives the distance to the floor, its
      {!Texture} sampled in world space and faded by fog. Above the horizon it
      is the ceiling the same way, or — where the level has no roof — the
      {!Sky}, whose colour depends only on the direction looked in.

    - {b Paint the opaque walls.} {!Ray.cast} returns every wall the ray
      crosses, farthest first. Each solid wall is drawn from its foot on the
      sloped floor up to its height (capped at the ceiling), its {!Texture}
      sampled per pixel and tinted by {!Palette}, with any {!Room.decal}s —
      paintings, posters — blended over it. Painting far-to-front lets a near
      wall cover the ones behind it while a tall wall still shows over a short
      one. Each wall records its distance into a per-pixel depth, so the passes
      that follow know exactly which pixels it covers — a short wall only the
      pixels of its own strip.

    {1 A frame}

    A column alone cannot place things that span columns or see past each other,
    so a frame is two phases: first the backgrounds and opaque walls above, per
    column; then everything translucent — the {b sprites} ({!Room.sprite}s
    drawn as billboards that always face the player) and the
    {b see-through walls} (grilles and windows, {!Texture.val-alpha}) —
    composited over them.

    The translucent things are drawn in one combined pass, farthest first, each
    still hidden — per pixel — by a nearer opaque wall, so a short wall in front
    hides only the lower part of a sprite or window behind it and the top still
    shows. They cannot each have their own pass: a sprite in front of a window
    has to cover it while a sprite behind one has to show through it, so sprites
    and see-through walls are sorted {e together} by depth.

    {1 Portals}

    What makes a doorway fit the painter's algorithm above is that {b it is just
    one more thing the ray meets}. A {!Room.type-threshold} joins the same far-to-near
    stream as the walls, so walls of this room beyond it are painted first and
    get covered, and walls nearer are painted after and cover it.

    A threshold with a leaf draws as a wall of the door's texture. An open one
    recurses: the neighbouring room is drawn in the same column, with the camera
    and the ray carried into its frame by the link's {!Transform} and the row
    clip narrowed to the opening. Above either, the {!Room.type-lintel} fills the
    strip of wall left standing over the gap — without it you would see over the
    top of a closed door.

    The rigid transform is horizontal, so eye height, projection and horizon are
    unchanged inside a portal and the {!Viewport} is not rebuilt; and because it
    preserves distance, a distance measured several rooms deep is directly
    comparable with everything else in the shared depth buffer. Rooms may form a
    cycle, so it is {!Config.max_portal_depth} and nothing else that ends the
    recursion — out of budget, the opening is filled with {!Palette.haze}.

    The sky belongs to the room. A room open to the sky takes its azimuth from
    the {e nested} direction, so it has its own sun: with per-room local
    coordinates there is no world compass to appeal to.

    Sprites and see-through walls still have to be composited together at the
    end, so a fragment records which room it was seen in, the pose it was seen
    from, and a {!type-mask} — the single column and row range of the doorway it
    was seen through. Because that is per column and not a bounding box, the clip
    is exact: a sprite behind a doorway is trimmed to the doorway's outline.

    One consequence of the mask being per column is that a room seen through a
    doorway contributes one fragment per column of that doorway. They are three
    words each and each draws a single column, so the cost is in the columns
    either way; but it does mean that if a chain of doorways ever loops back to a
    room already on it, that room's sprites are composited once for the near view
    and again for the far one. Both are honest views of the same objects at
    different distances, so they are left alone rather than suppressed.

    {1 Size}

    Casting per pixel costs one division per pixel, so the buffer is rendered no
    taller than {!Config.max_render_height} and the GPU scales it to the actual
    window. That keeps the cost — and so the frame rate — steady at any window
    size, and covers resizing and fullscreen with it. *)

open Tsdl
open Result_ext

(** The internal render size for a given window size: the window scaled down by
    a whole number until it is within the height cap, so its aspect ratio — and
    with it the {!Viewport} resize rules — is preserved. *)
let internal_size ~width ~height =
  let scale =
    Int.max 1
      ((height + Config.max_render_height - 1) / Config.max_render_height)
  in
  (Int.max 1 (width / scale), Int.max 1 (height / scale))

let clamp8 v = if v < 0 then 0 else if v > 255 then 255 else v

(** Clamp a sample index into a [0 .. n-1] range. *)
let clampi n v = Int.max 0 (Int.min (n - 1) v)

(** Fill one column's background: the floor below, and either a ceiling or the
    open {!Sky} above, depending on whether the level is roofed. Each pixel
    casts the relevant plane with {!Plane.view_distance} (one division), then
    shows its texture in world space, tinted and fogged; sky pixels come from
    {!Sky} and depend only on the direction looked in. *)
let draw_planes fb viewport world (player : Player.t) ~column ~dir ~first ~last =
  let open Viewport in
  let height = fb.Framebuffer.height in
  let eye_z = viewport.eye_z in
  let px = player.Player.pos.Vec.x and py = player.Player.pos.Vec.y in
  let dx = dir.Vec.x and dy = dir.Vec.y in
  let floor_base = Plane.elevation world.Room.floor player.Player.pos in
  let gf = Plane.gradient world.Room.floor dir in
  (* Paint one floor/ceiling pixel: the plane's texture at the world point it
     casts to, tinted by its colour and faded by fog. *)
  let surface y d (color : Color.t) pattern =
    let wx = px +. (d *. dx) and wy = py +. (d *. dy) in
    let texel = Palette.plane_texel pattern ~x:wx ~y:wy in
    let f = Palette.fog d in
    Framebuffer.set fb ~x:column ~y
      ~r:(clamp8 (int_of_float (float_of_int (color.r * texel / 255) *. f)))
      ~g:(clamp8 (int_of_float (float_of_int (color.g * texel / 255) *. f)))
      ~b:(clamp8 (int_of_float (float_of_int (color.b * texel / 255) *. f)))
  in
  match world.Room.ceiling with
  | Some ceiling ->
      let ceil_base = Plane.elevation ceiling player.Player.pos in
      let gc = Plane.gradient ceiling dir in
      for y = Int.max 0 first to Int.min (height - 1) last do
        let r = row_factor viewport ~row:y in
        (* A plane is only in view on its side of the horizon, so at most one of
           these is a positive distance for this pixel. *)
        let df =
          let dn = r +. gf in
          if dn > 1e-9 then (eye_z -. floor_base) /. dn else infinity
        and dc =
          let dn = r +. gc in
          if dn < -1e-9 then (eye_z -. ceil_base) /. dn else infinity
        in
        if Float.is_finite df && df <= dc then
          surface y df Palette.floor_color Palette.floor_pattern
        else if Float.is_finite dc then
          surface y dc Palette.ceiling_color Palette.ceiling_pattern
        else
          Framebuffer.set fb ~x:column ~y ~r:Palette.haze.Color.r
            ~g:Palette.haze.Color.g ~b:Palette.haze.Color.b
      done
  | None ->
      (* No roof: below the horizon is floor, above it is sky. The sky depends
         only on the column's azimuth and the pixel's elevation. *)
      let azimuth = Float.atan2 dy dx in
      for y = Int.max 0 first to Int.min (height - 1) last do
        let r = row_factor viewport ~row:y in
        let dn = r +. gf in
        if dn > 1e-9 then
          surface y
            ((eye_z -. floor_base) /. dn)
            Palette.floor_color Palette.floor_pattern
        else
          let s = Sky.color ~azimuth ~up:(-.r) in
          Framebuffer.set fb ~x:column ~y ~r:s.Color.r ~g:s.Color.g ~b:s.Color.b
      done

(** Paint one wall of a column over the background already there, and any decals
    hung on it over that.

    The wall stands on the floor at its hit point and rises to its height —
    capped at the ceiling, if the level has one, so it never draws over the
    ceiling in front of it. Its texture repeats every cell of height and is
    sampled per pixel; where that texel is not solid ({!Texture.val-alpha}) the
    wall is blended rather than written, so a grille or a window unveils what is
    behind. Decals — pictures placed by {!Room.along} and height — are blended
    over the wall's own texture, in the same light. *)
let draw_wall fb viewport world (player : Player.t) ~column ~dir ~occlude
    ~first:clip_first ~last:clip_last (hit : Ray.hit) =
  let open Viewport in
  let depth = fb.Framebuffer.depth and width = fb.Framebuffer.width in
  let d = hit.Ray.distance in
  let w = hit.Ray.wall in
  let hit_point =
    Vec.make
      (player.Player.pos.Vec.x +. (d *. dir.Vec.x))
      (player.Player.pos.Vec.y +. (d *. dir.Vec.y))
  in
  let floor_z = Plane.elevation world.Room.floor hit_point in
  (* The wall rises to its own height, but a roof caps it so it never draws over
     the ceiling in front of it; with open sky there is nothing to cap against. *)
  let top_z =
    match world.Room.ceiling with
    | Some ceiling ->
        Float.min
          (floor_z +. w.Room.height)
          (Plane.elevation ceiling hit_point)
    | None -> floor_z +. w.Room.height
  in
  if top_z > floor_z then begin
    let y_foot = project_height viewport ~z:floor_z ~distance:d in
    let y_top = project_height viewport ~z:top_z ~distance:d in
    let first =
      Int.max clip_first (Int.max 0 (int_of_float (Float.round y_top)))
    in
    let last =
      Int.min clip_last
        (Int.min (fb.Framebuffer.height - 1)
           (int_of_float (Float.round y_foot)))
    in
    (* One light factor — orientation and fog — dims both the wall and its
       decals, so the decals sit in the same light as the wall they are on. *)
    let light = Palette.face_shading w.Room.normal *. Palette.fog d in
    let tint = Color.shade (Palette.wall_color w.Room.texture) light in
    let pattern = Palette.pattern w.Room.texture in
    let u =
      Texture.column_of_offset (hit.Ray.along -. Float.floor hit.Ray.along)
    in
    (* The decals whose horizontal span this column falls in, with the texture
       column of each — both constant down the column. *)
    let decals =
      List.filter_map
        (fun (dec : Room.decal) ->
          let off =
            hit.Ray.along -. (dec.Room.along -. dec.Room.half_width)
          in
          if off >= 0. && off <= 2. *. dec.Room.half_width then
            let n = dec.Room.image.Image.size in
            let ui =
              clampi n
                (int_of_float
                   (off /. (2. *. dec.Room.half_width) *. float_of_int n))
            in
            Some (dec, ui)
          else None)
        w.Room.decals
    in
    for y = first to last do
      (* An opaque wall is painted unconditionally (nearer opaque walls are drawn
         after it) and records its distance; a see-through wall is only drawn
         where it stands nearer than the opaque wall already noted for this
         pixel, so a short wall in front hides only the part behind it. *)
      let index = (y * width) + column in
      if occlude || d < depth.(index) then begin
        (* World height this pixel looks at on the wall, then where that falls
           within the current one-cell tile of the texture (bottom of a cell at
           the texture's bottom row, top at its top). *)
        let z =
          viewport.eye_z
          -. ((float_of_int y -. viewport.horizon) *. d /. viewport.projection)
        in
        let above_foot = z -. floor_z in
        let tile = above_foot -. Float.floor above_foot in
        let v =
          clampi Texture.size
            (int_of_float ((1. -. tile) *. float_of_int (Texture.size - 1)))
        in
        let texel = Texture.sample pattern ~u ~v in
        let a = Texture.alpha pattern ~u ~v in
        let cr = tint.Color.r * texel / 255
        and cg = tint.Color.g * texel / 255
        and cb = tint.Color.b * texel / 255 in
        if a = 255 then begin
          Framebuffer.set fb ~x:column ~y ~r:cr ~g:cg ~b:cb;
          if occlude then depth.(index) <- d
        end
        else if a > 0 then
          Framebuffer.blend fb ~x:column ~y ~r:cr ~g:cg ~b:cb ~a;
        List.iter
          (fun ((dec : Room.decal), ui) ->
            (* A decal hangs a height above the {e floor} under the wall, not at
               an absolute elevation, so it is placed against [above_foot] — on
               a sloped floor it then rides with the wall instead of tilting
               across it. *)
            if
              above_foot >= dec.Room.z -. dec.Room.half_height
              && above_foot <= dec.Room.z +. dec.Room.half_height
            then begin
              let img = dec.Room.image in
              let n = img.Image.size in
              let vf =
                (dec.Room.z +. dec.Room.half_height -. above_foot)
                /. (2. *. dec.Room.half_height)
              in
              let vi = clampi n (int_of_float (vf *. float_of_int n)) in
              let idx = Image.index img ~u:ui ~v:vi in
              let da = img.Image.alpha.(idx) in
              if da > 0 then
                let c = img.Image.pixels.(idx) in
                Framebuffer.blend fb ~x:column ~y
                  ~r:(clamp8 (int_of_float (float_of_int c.Color.r *. light)))
                  ~g:(clamp8 (int_of_float (float_of_int c.Color.g *. light)))
                  ~b:(clamp8 (int_of_float (float_of_int c.Color.b *. light)))
                  ~a:da
            end)
          decals
      end
    done
  end

type mask = Full | Within of { column : int; first : int; last : int }
(** Which pixels a translucent thing seen through a chain of doorways is allowed
    to touch. Everything drawn inside a portal is drawn one column at a time, so
    a mask is never more than a single column and a row range within it — the
    rows the opening covered {e in that column}. That makes the clip exact
    rather than a bounding box: a sprite seen through a doorway is trimmed to
    the doorway's own outline, not to a rectangle around it. [Full] is the
    player's own room, clipped by nothing. *)

(** Draw one sprite as a billboard: a flat image that always faces the player,
    at perpendicular distance [depth_s]. It is placed by projecting its foot
    onto the sloped floor and its top a [size] above, and drawn per pixel only
    where it stands nearer than the opaque wall noted there, so a wall in front
    — even a short one — hides just the part of it behind. *)
let draw_sprite fb viewport world (player : Player.t) (s : Room.sprite)
    ~depth_s ~mask =
  let open Viewport in
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let depth = fb.Framebuffer.depth in
  let rel = Vec.sub s.Room.pos player.Player.pos in
  let lateral = Vec.dot rel player.Player.right in
  (* Same mapping {!ray_direction} inverts: a point at (depth, lateral) in camera
     space sits at this fraction across the screen. *)
  let camera_x = lateral /. (depth_s *. viewport.half_width) in
  let center = (camera_x +. 1.) *. float_of_int width /. 2. in
  let floor_z = Plane.elevation world.Room.floor s.Room.pos in
  let y_base = project_height viewport ~z:floor_z ~distance:depth_s in
  let y_top =
    project_height viewport ~z:(floor_z +. s.Room.size) ~distance:depth_s
  in
  (* The image is square, so its on-screen width equals its height. *)
  let half = (y_base -. y_top) /. 2. in
  let left = center -. half and rightx = center +. half in
  let span = rightx -. left and vspan = y_base -. y_top in
  let img = s.Room.image in
  let n = img.Image.size in
  let f = Palette.fog depth_s in
  let col0 = Int.max 0 (int_of_float (Float.round left)) in
  let col1 = Int.min (width - 1) (int_of_float (Float.round rightx)) in
  let row0 = Int.max 0 (int_of_float (Float.round y_top)) in
  let row1 = Int.min (height - 1) (int_of_float (Float.round y_base)) in
  (* A masked sprite reaches at most one column, so narrowing the span before
     the loop rather than discarding rows inside it is the difference between
     visiting the sprite's whole width and visiting the one column that can
     actually be drawn. *)
  let col0, col1, row0, row1 =
    match mask with
    | Full -> (col0, col1, row0, row1)
    | Within m ->
        ( Int.max col0 m.column,
          Int.min col1 m.column,
          Int.max row0 m.first,
          Int.min row1 m.last )
  in
  for col = col0 to col1 do
    let ui =
      clampi n
        (int_of_float ((float_of_int col -. left) /. span *. float_of_int n))
    in
    for y = row0 to row1 do
      if depth_s < depth.((y * width) + col) then begin
        let vi =
          clampi n
            (int_of_float
               ((float_of_int y -. y_top) /. vspan *. float_of_int n))
        in
        let idx = Image.index img ~u:ui ~v:vi in
        let a = img.Image.alpha.(idx) in
        if a > 0 then
          let c = img.Image.pixels.(idx) in
          Framebuffer.blend fb ~x:col ~y
            ~r:(clamp8 (int_of_float (float_of_int c.Color.r *. f)))
            ~g:(clamp8 (int_of_float (float_of_int c.Color.g *. f)))
            ~b:(clamp8 (int_of_float (float_of_int c.Color.b *. f)))
            ~a
      end
    done
  done

(** A translucent thing to composite after the opaque geometry: a sprite, or a
    strip of a see-through wall in one column. Both let what is behind show
    through, so they cannot each be given their own pass — a sprite in front of
    a window must cover it, one behind it must show through — they have to be
    sorted together and drawn farthest first. *)
type fragment = {
  distance : float;
  pose : Player.t;
  room : int;
  mask : mask;
  what : fragment_kind;
}

and fragment_kind =
  | Sprite of Room.sprite
  | See_through of int * Vec.t * Ray.hit

type step = Wall of Ray.hit | Opening of Ray.opening

(** Both lists arrive farthest-first, so one merge puts walls and thresholds
    into a single painter's-order stream without sorting either of them again. *)
let rec merge walls openings =
  match (walls, openings) with
  | [], rest | rest, [] -> rest
  | (dw, _) :: _, (((d, _) : float * step) as opening) :: rest when d > dw ->
      opening :: merge walls rest
  | wall :: rest, _ -> wall :: merge rest openings

(** Draw one column of one room, and — through any open doorway the column meets
    — of its neighbours behind it, clipped to [clip] rows.

    Everything the ray meets, wall or threshold, joins one stream ordered
    farthest first, and the painter's algorithm does the rest: a wall beyond a
    doorway is painted before it and gets covered, a wall nearer is painted
    after and covers it. A threshold with a leaf draws as a wall of that
    texture. An open one recurses, after carrying the camera and the ray into
    the neighbour's frame, with the clip narrowed to the rows the opening covers
    in this column.

    The {!Viewport} is deliberately not rebuilt for the nested room. A rigid
    motion is horizontal, so eye height, projection and horizon all carry over
    unchanged, and — because it also preserves distance — a distance measured
    three rooms deep is directly comparable with everything already in the
    shared depth buffer. Only [pos], [dir] and [right] move.

    Rooms may form a cycle, so it is [budget] and nothing else that ends the
    recursion; when it runs out the opening is filled with {!Palette.haze}, the
    same colour the planes already fade into. *)
let rec draw_room_column fb viewport world ~room ~pose ~column ~dir
    ~clip:(top, bottom) ~budget ~translucent =
  let current = World.room world room in
  let portals = World.portals world room in
  draw_planes fb viewport current pose ~column ~dir ~first:top ~last:bottom;
  (* One wall strip: painted straight over the column if it is opaque, held back
     for the translucent pass if you can see through it. *)
  let paint ~first ~last (hit : Ray.hit) =
    if (Palette.pattern hit.Ray.wall.Room.texture).Texture.opaque then
      draw_wall fb viewport current pose ~column ~dir ~occlude:true ~first ~last
        hit
    else
      translucent :=
        {
          distance = hit.Ray.distance;
          pose;
          room;
          mask = Within { column; first; last };
          what = See_through (column, dir, hit);
        }
        :: !translucent
  in
  (* A threshold drawn as though it were a wall — the leaf of a door, or the
     lintel above an opening, which is the strip of the surrounding wall left
     standing over the gap. *)
  let as_wall (threshold : Room.threshold) ~height ~texture ~distance ~along =
    let wall : Room.wall =
      {
        a = threshold.a;
        b = threshold.b;
        height;
        texture;
        decals = [];
        edge = threshold.edge;
        length = threshold.length;
        normal = threshold.normal;
      }
    in
    { Ray.distance; along; wall }
  in
  let walls =
    List.map
      (fun (hit : Ray.hit) -> (hit.Ray.distance, Wall hit))
      (Ray.cast current ~origin:pose.Player.pos ~direction:dir)
  and openings =
    List.map
      (fun (opening : Ray.opening) -> (opening.Ray.distance, Opening opening))
      (Ray.openings current ~origin:pose.Player.pos ~direction:dir)
  in
  List.iter
    (fun (_, step) ->
      match step with
      | Wall hit -> paint ~first:top ~last:bottom hit
      | Opening opening ->
          let portal = portals.(opening.Ray.index) in
          let threshold = portal.World.threshold in
          let distance = opening.Ray.distance in
          let hit_point = Vec.add pose.Player.pos (Vec.scale dir distance) in
          let floor_z = Plane.elevation current.Room.floor hit_point in
          let row z =
            int_of_float
              (Float.round (Viewport.project_height viewport ~z ~distance))
          in
          let head = Int.max top (row (floor_z +. threshold.height))
          and foot = Int.min bottom (row floor_z) in
          (* The wall above the opening. Without it the gap runs the full height
             of the wall it was cut into and you see over the door. *)
          Option.iter
            (fun (l : Room.lintel) ->
              paint ~first:top
                ~last:(Int.min bottom (head - 1))
                (as_wall threshold ~height:l.Room.top ~texture:l.Room.texture
                   ~distance ~along:opening.Ray.along))
            threshold.lintel;
          if head <= foot then
            match threshold.door with
            | Some texture ->
                paint ~first:head ~last:foot
                  (as_wall threshold ~height:threshold.height ~texture
                     ~distance ~along:opening.Ray.along)
            | None when budget > 0 ->
                let nested =
                  Player.through portal.World.onto ~room:portal.World.to_room
                    pose
                in
                let mask = Within { column; first = head; last = foot } in
                Array.iter
                  (fun sprite ->
                    let distance =
                      Vec.dot
                        (Vec.sub sprite.Room.pos nested.Player.pos)
                        nested.Player.dir
                    in
                    if distance > 0.1 then
                      translucent :=
                        {
                          distance;
                          pose = nested;
                          room = portal.World.to_room;
                          mask;
                          what = Sprite sprite;
                        }
                        :: !translucent)
                  (World.room world portal.World.to_room).Room.sprites;
                draw_room_column fb viewport world ~room:portal.World.to_room
                  ~pose:nested ~column
                  ~dir:(Transform.direction portal.World.onto dir)
                  ~clip:(head, foot) ~budget:(budget - 1) ~translucent
            | None ->
                for y = head to foot do
                  Framebuffer.set fb ~x:column ~y ~r:Palette.haze.Color.r
                    ~g:Palette.haze.Color.g ~b:Palette.haze.Color.b
                done)
    (merge walls openings)

(** Composite the sprites and see-through walls together, farthest first, each
    still hidden by a nearer opaque wall in its column. Interleaving them is
    what lets a sprite occlude a window behind it and show through one in front.
*)
let draw_translucent fb viewport world fragments =
  List.iter
    (fun fragment ->
      let current = World.room world fragment.room in
      match fragment.what with
      | Sprite s ->
          draw_sprite fb viewport current fragment.pose s
            ~depth_s:fragment.distance ~mask:fragment.mask
      | See_through (column, dir, hit) ->
          let first, last =
            match fragment.mask with
            | Full -> (0, fb.Framebuffer.height - 1)
            | Within m -> (m.first, m.last)
          in
          draw_wall fb viewport current fragment.pose ~column ~dir
            ~occlude:false ~first ~last hit)
    (List.stable_sort
       (fun a b -> Float.compare b.distance a.distance)
       fragments)

(** Fill the whole framebuffer from the player's point of view: the backgrounds
    and opaque walls per column, then the sprites and see-through walls
    composited over them together, farthest first. Pure array writes, no SDL
    calls — those are left to {!render}. *)
let draw_frame fb world (player : Player.t) =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let current = World.room world player.room in
  let eye_z =
    Plane.elevation current.Room.floor player.Player.pos +. Config.eye_height
  in
  let viewport =
    Viewport.create ~pitch:player.Player.pitch ~eye_z ~width ~height
  in
  Framebuffer.clear_depth fb;
  let translucent = ref [] in
  Array.iter
    (fun sprite ->
      let distance = Vec.dot (Vec.sub sprite.Room.pos player.pos) player.dir in
      if distance > 0.1 then
        translucent :=
          {
            distance;
            pose = player;
            room = player.room;
            mask = Full;
            what = Sprite sprite;
          }
          :: !translucent)
    current.Room.sprites;
  for column = 0 to width - 1 do
    let dir = Viewport.ray_direction viewport player ~column in
    draw_room_column fb viewport world ~room:player.room ~pose:player ~column
      ~dir ~clip:(0, height - 1) ~budget:Config.max_portal_depth ~translucent
  done;
  draw_translucent fb viewport world !translucent

(** (Re)size the framebuffer to match [width] x [height], reusing it when the
    size has not changed. *)
let ensure sdl fb_ref ~width ~height =
  let current = !fb_ref in
  if current.Framebuffer.width = width && current.Framebuffer.height = height
  then Ok ()
  else
    let+ next = Framebuffer.create sdl ~width ~height in
    Framebuffer.destroy current;
    fb_ref := next

(** Render a frame: fit the buffer to the current window, fill it, upload it and
    present. The window size is read fresh every frame, so resizing and
    fullscreen need no special handling. *)
let render sdl fb_ref world player =
  let* out_w, out_h = Sdl.get_renderer_output_size sdl in
  let width, height = internal_size ~width:out_w ~height:out_h in
  let* () = ensure sdl fb_ref ~width ~height in
  draw_frame !fb_ref world player;
  let+ () =
    Framebuffer.present sdl !fb_ref
      ~dst:(Sdl.Rect.create ~x:0 ~y:0 ~w:out_w ~h:out_h)
  in
  Sdl.render_present sdl
