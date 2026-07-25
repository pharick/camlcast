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
      sampled per pixel and tinted by {!Palette}, with any {!World.decal}s —
      paintings, posters — blended over it. Painting far-to-front lets a near
      wall cover the ones behind it while a tall wall still shows over a short
      one. Each wall records its distance into a per-pixel depth, so the passes
      that follow know exactly which pixels it covers — a short wall only the
      pixels of its own strip.

    {1 A frame}

    A column alone cannot place things that span columns or see past each other,
    so a frame is two phases: first the backgrounds and opaque walls above, per
    column; then everything translucent — the {b sprites} ({!World.sprite}s
    drawn as billboards that always face the player) and the
    {b see-through walls} (grilles and windows, {!Texture.val-alpha}) —
    composited over them.

    The translucent things are drawn in one combined pass, farthest first, each
    still hidden — per pixel — by a nearer opaque wall, so a short wall in front
    hides only the lower part of a sprite or window behind it and the top still
    shows. They cannot each have their own pass: a sprite in front of a window
    has to cover it while a sprite behind one has to show through it, so sprites
    and see-through walls are sorted {e together} by depth.

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
let draw_planes fb viewport world (player : Player.t) ~column ~dir =
  let open Viewport in
  let height = fb.Framebuffer.height in
  let eye_z = viewport.eye_z in
  let px = player.Player.pos.Vec.x and py = player.Player.pos.Vec.y in
  let dx = dir.Vec.x and dy = dir.Vec.y in
  let floor_base = Plane.elevation world.World.floor player.Player.pos in
  let gf = Plane.gradient world.World.floor dir in
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
  match world.World.ceiling with
  | Some ceiling ->
      let ceil_base = Plane.elevation ceiling player.Player.pos in
      let gc = Plane.gradient ceiling dir in
      for y = 0 to height - 1 do
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
      for y = 0 to height - 1 do
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
    behind. Decals — pictures placed by {!World.along} and height — are blended
    over the wall's own texture, in the same light. *)
let draw_wall fb viewport world (player : Player.t) ~column ~dir ~occlude
    (hit : Ray.hit) =
  let open Viewport in
  let depth = fb.Framebuffer.depth and width = fb.Framebuffer.width in
  let d = hit.Ray.distance in
  let w = hit.Ray.wall in
  let hit_point =
    Vec.make
      (player.Player.pos.Vec.x +. (d *. dir.Vec.x))
      (player.Player.pos.Vec.y +. (d *. dir.Vec.y))
  in
  let floor_z = Plane.elevation world.World.floor hit_point in
  (* The wall rises to its own height, but a roof caps it so it never draws over
     the ceiling in front of it; with open sky there is nothing to cap against. *)
  let top_z =
    match world.World.ceiling with
    | Some ceiling ->
        Float.min
          (floor_z +. w.World.height)
          (Plane.elevation ceiling hit_point)
    | None -> floor_z +. w.World.height
  in
  if top_z > floor_z then begin
    let y_foot = project_height viewport ~z:floor_z ~distance:d in
    let y_top = project_height viewport ~z:top_z ~distance:d in
    let first = Int.max 0 (int_of_float (Float.round y_top)) in
    let last =
      Int.min (fb.Framebuffer.height - 1) (int_of_float (Float.round y_foot))
    in
    (* One light factor — orientation and fog — dims both the wall and its
       decals, so the decals sit in the same light as the wall they are on. *)
    let light = Palette.face_shading w.World.normal *. Palette.fog d in
    let tint = Color.shade (Palette.wall_color w.World.texture) light in
    let pattern = Palette.pattern w.World.texture in
    let u =
      Texture.column_of_offset (hit.Ray.along -. Float.floor hit.Ray.along)
    in
    (* The decals whose horizontal span this column falls in, with the texture
       column of each — both constant down the column. *)
    let decals =
      List.filter_map
        (fun (dec : World.decal) ->
          let off =
            hit.Ray.along -. (dec.World.along -. dec.World.half_width)
          in
          if off >= 0. && off <= 2. *. dec.World.half_width then
            let n = dec.World.image.Image.size in
            let ui =
              clampi n
                (int_of_float
                   (off /. (2. *. dec.World.half_width) *. float_of_int n))
            in
            Some (dec, ui)
          else None)
        w.World.decals
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
          (fun ((dec : World.decal), ui) ->
            (* A decal hangs a height above the {e floor} under the wall, not at
               an absolute elevation, so it is placed against [above_foot] — on
               a sloped floor it then rides with the wall instead of tilting
               across it. *)
            if
              above_foot >= dec.World.z -. dec.World.half_height
              && above_foot <= dec.World.z +. dec.World.half_height
            then begin
              let img = dec.World.image in
              let n = img.Image.size in
              let vf =
                (dec.World.z +. dec.World.half_height -. above_foot)
                /. (2. *. dec.World.half_height)
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

(** Draw one column: its background, then the opaque walls it crosses (painting
    over each other far to near). Each opaque wall records its distance into the
    framebuffer's per-pixel depth, for sprite and see-through occlusion — per
    pixel, so a short wall only hides what is behind its own strip. See-through
    walls are deferred into [translucent] to be composited afterwards. *)
let draw_column fb viewport world (player : Player.t) ~column ~translucent =
  let dir = Viewport.ray_direction viewport player ~column in
  draw_planes fb viewport world player ~column ~dir;
  let hits = Ray.cast world ~origin:player.Player.pos ~direction:dir in
  List.iter
    (fun (hit : Ray.hit) ->
      if (Palette.pattern hit.Ray.wall.World.texture).Texture.opaque then
        draw_wall fb viewport world player ~column ~dir ~occlude:true hit
      else translucent := (hit.Ray.distance, column, dir, hit) :: !translucent)
    hits

(** Draw one sprite as a billboard: a flat image that always faces the player,
    at perpendicular distance [depth_s]. It is placed by projecting its foot
    onto the sloped floor and its top a [size] above, and drawn per pixel only
    where it stands nearer than the opaque wall noted there, so a wall in front
    — even a short one — hides just the part of it behind. *)
let draw_sprite fb viewport world (player : Player.t) (s : World.sprite)
    ~depth_s =
  let open Viewport in
  let width = viewport.width and height = fb.Framebuffer.height in
  let depth = fb.Framebuffer.depth in
  let rel = Vec.sub s.World.pos player.Player.pos in
  let lateral = Vec.dot rel player.Player.right in
  (* Same mapping {!ray_direction} inverts: a point at (depth, lateral) in camera
     space sits at this fraction across the screen. *)
  let camera_x = lateral /. (depth_s *. viewport.half_width) in
  let center = (camera_x +. 1.) *. float_of_int width /. 2. in
  let floor_z = Plane.elevation world.World.floor s.World.pos in
  let y_base = project_height viewport ~z:floor_z ~distance:depth_s in
  let y_top =
    project_height viewport ~z:(floor_z +. s.World.size) ~distance:depth_s
  in
  (* The image is square, so its on-screen width equals its height. *)
  let half = (y_base -. y_top) /. 2. in
  let left = center -. half and rightx = center +. half in
  let span = rightx -. left and vspan = y_base -. y_top in
  let img = s.World.image in
  let n = img.Image.size in
  let f = Palette.fog depth_s in
  let col0 = Int.max 0 (int_of_float (Float.round left)) in
  let col1 = Int.min (width - 1) (int_of_float (Float.round rightx)) in
  let row0 = Int.max 0 (int_of_float (Float.round y_top)) in
  let row1 = Int.min (height - 1) (int_of_float (Float.round y_base)) in
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
type fragment =
  | Sprite of World.sprite
  | See_through of int * Vec.t * Ray.hit  (** column, ray direction, hit *)

(** Composite the sprites and see-through walls together, farthest first, each
    still hidden by a nearer opaque wall in its column. Interleaving them is
    what lets a sprite occlude a window behind it and show through one in front.
*)
let draw_translucent fb viewport world (player : Player.t) ~sprites ~see_through
    =
  let sprite_frags =
    List.filter_map
      (fun (s : World.sprite) ->
        let d =
          Vec.dot (Vec.sub s.World.pos player.Player.pos) player.Player.dir
        in
        (* In front of the camera plane, and not right on top of it. *)
        if d > 0.1 then Some (d, Sprite s) else None)
      (Array.to_list sprites)
  in
  let wall_frags =
    List.map
      (fun (dist, column, dir, hit) -> (dist, See_through (column, dir, hit)))
      see_through
  in
  List.iter
    (fun (d, fragment) ->
      match fragment with
      | Sprite s -> draw_sprite fb viewport world player s ~depth_s:d
      | See_through (column, dir, hit) ->
          draw_wall fb viewport world player ~column ~dir ~occlude:false hit)
    (List.sort
       (fun (a, _) (b, _) -> Float.compare b a)
       (sprite_frags @ wall_frags))

(** Fill the whole framebuffer from the player's point of view: the backgrounds
    and opaque walls per column, then the sprites and see-through walls
    composited over them together, farthest first. Pure array writes, no SDL
    calls — those are left to {!render}. *)
let draw_frame fb world (player : Player.t) =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let eye_z =
    Plane.elevation world.World.floor player.Player.pos +. Config.eye_height
  in
  let viewport =
    Viewport.create ~pitch:player.Player.pitch ~eye_z ~width ~height
  in
  Framebuffer.clear_depth fb;
  let see_through = ref [] in
  for column = 0 to width - 1 do
    draw_column fb viewport world player ~column ~translucent:see_through
  done;
  draw_translucent fb viewport world player ~sprites:world.World.sprites
    ~see_through:!see_through

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
