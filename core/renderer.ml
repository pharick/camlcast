(* Implementation of {!Camlcast.Renderer}; the interface carries the prose. *)

open Tsdl
open Result_ext

let internal_size ~width ~height =
  let scale =
    Int.max 1
      ((height + Config.max_render_height - 1) / Config.max_render_height)
  in
  (Int.max 1 (width / scale), Int.max 1 (height / scale))

let clamp8 v = if v < 0 then 0 else if v > 255 then 255 else v

(* Clamp a sample index into a [0 .. n-1] range. *)
let clampi n v = Int.max 0 (Int.min (n - 1) v)

(* Fill one column's background: the floor below, and either a ceiling or the
    open {!Sky} above, depending on whether the level is roofed. Each pixel
    casts the relevant plane with {!Plane.view_distance} (one division), then
    shows its texture in world space, tinted by its colour and traded away for
    the haze as it recedes; sky pixels come from {!Sky} and depend only on the
    direction looked in.

    [near] is the doorway this room is being drawn through, and a plane cast
    nearer than it is not this room's to show. The case that needs it is the
    strip above an opening: a transom you can see through recurses for the rows
    over the doorway's head, and up there the neighbour's {e ceiling} runs back
    towards the eye and stands nearer than the doorway long before the top of
    the window. Without the clip its roof — or, over a room open to one, its sky
    — is painted overhead in the room the player is standing in. The floor wants
    the same rule for a narrower reason: inside the opening it is already beyond
    [near] wherever the two rooms' floors meet at the seam, so it only bites
    where they disagree, which is what {!World.seam_gap} measures.

    A pixel whose planes are all clipped away is left as it is rather than
    hazed: inside a portal the room in front has already painted every row of
    this band. The haze is for the pixel where neither plane is in view at all,
    which is a fact about the geometry and not about the doorway.

    Telling those two apart is a question of direction and not of distance. Only
    a plane in {e front} of the eye can be the one the doorway clipped, and that
    is what makes leaving its pixel alone safe — something nearer painted it. A
    plane behind the eye was never anybody's to paint: it casts to a negative
    distance, and a negative distance is not a near surface but an absent one, so
    the band is the haze's.

    At the top level [near] is zero and the floor is never clipped, the eye
    standing {!Config.eye_height} above it. A ceiling {e below} the eye is
    clipped, and is authorable however unlikely it reads: {!Plane.above} takes a
    negative height, {!Room.roof} has no opinion, and a roof that merely
    {e converges} on the floor gets there too. Above the horizon neither plane is
    then in view and the haze fills the band, which is the whole of what keeps
    the promise the colour buffer is never cleared on. *)
let draw_planes fb viewport ~air room (player : Player.t) ~column ~dir ~near
    ~first ~last =
  let open Viewport in
  let height = fb.Framebuffer.height in
  let eye_z = viewport.eye_z in
  let px = player.Player.pos.Vec.x and py = player.Player.pos.Vec.y in
  let dx = dir.Vec.x and dy = dir.Vec.y in
  let floor_plane = Room.floor_plane room
  and floor_material = Room.floor_material room in
  let haze = air.Atmosphere.haze in
  let floor_base = Plane.elevation floor_plane player.Player.pos in
  let gf = Plane.gradient floor_plane dir in
  (* Paint one floor/ceiling pixel: the surface's material at the world point it
     casts to, tinted by its colour, and with distance traded for the haze the
     air is full of — {!Atmosphere.fog} of the surface plus what is left of the
     haze, rather than the surface dimmed towards black.

     Written out rather than handed to {!Color.lerp} for the same reason the
     multiply before it was: this is the one fog site where the distance changes
     with every pixel, so there is nothing to hoist out of the loop, and a
     colour record per pixel of the background is a record the frame does not
     need. *)
  let surface y d (m : Material.t) =
    let wx = px +. (d *. dx) and wy = py +. (d *. dy) in
    let c = Material.plane_texel m ~x:wx ~y:wy in
    let f = Atmosphere.fog air d in
    let veil = 1. -. f in
    let mix v h =
      clamp8 (int_of_float ((float_of_int v *. f) +. (float_of_int h *. veil)))
    in
    Framebuffer.set fb ~x:column ~y
      ~r:(mix c.Color.r haze.Color.r)
      ~g:(mix c.Color.g haze.Color.g)
      ~b:(mix c.Color.b haze.Color.b)
  in
  (* In front of the doorway this room is being drawn through, and so out of
     the picture: taken out before the two planes are compared, so that a floor
     clipped away still lets the ceiling behind it be drawn. *)
  let beyond d = if d > near then d else infinity in
  (* In front of the eye, and so a surface somebody has to show — which is the
     only kind [beyond] can have taken out. A cast that is finite but negative is
     a plane behind the eye and no surface at all. *)
  let in_view d = Float.is_finite d && d > 0. in
  match Room.ceiling room with
  | Room.Roof ceiling ->
      let ceil_base = Plane.elevation ceiling.Room.plane player.Player.pos in
      let gc = Plane.gradient ceiling.Room.plane dir in
      for y = Int.max 0 first to Int.min (height - 1) last do
        let r = row_factor viewport ~row:y in
        (* A plane is only in view on its side of the horizon, so for parallel
           planes at most one of these is a positive distance. Two that converge
           along the ray can both be, and then the comparison below takes the
           nearer, which is what it would do anyway. *)
        let cast_f =
          let dn = r +. gf in
          if dn > 1e-9 then (eye_z -. floor_base) /. dn else infinity
        and cast_c =
          let dn = r +. gc in
          if dn < -1e-9 then (eye_z -. ceil_base) /. dn else infinity
        in
        let df = beyond cast_f and dc = beyond cast_c in
        if Float.is_finite df && df <= dc then surface y df floor_material
        else if Float.is_finite dc then surface y dc ceiling.Room.material
        else if not (in_view cast_f || in_view cast_c) then
          (* Neither plane is in view here, so there is no surface to keep any
             of: the band is the haze at full strength, which is where the two
             fades either side of it are heading. *)
          Framebuffer.set fb ~x:column ~y ~r:haze.Color.r ~g:haze.Color.g
            ~b:haze.Color.b
      done
  | Room.Open sky ->
      (* No roof: below the horizon is floor, above it is sky. The sky depends
         only on the column's azimuth and the pixel's elevation. It needs no
         clip of its own, being infinitely far and so beyond every doorway.

         Nothing here needs [in_view]. There is only one plane, and at the top
         level it is never clipped — the eye stands {!Config.eye_height} above
         its own floor, so the cast is positive wherever it is in view, and every
         row is either floor or sky. Inside a portal a cast the clip takes out is
         a floor the room in front has already painted, which is the case the
         pixel is left alone for. *)
      let azimuth = Float.atan2 dy dx in
      for y = Int.max 0 first to Int.min (height - 1) last do
        let r = row_factor viewport ~row:y in
        let dn = r +. gf in
        if dn > 1e-9 then begin
          let d = (eye_z -. floor_base) /. dn in
          if d > near then surface y d floor_material
        end
        else
          let s = Sky.color sky ~azimuth ~up:(-.r) in
          Framebuffer.set fb ~x:column ~y ~r:s.Color.r ~g:s.Color.g ~b:s.Color.b
      done

(* Paint one wall of a column over the background already there, and any decals
    hung on it over that.

    The wall stands on the floor at its hit point and rises to its height —
    capped at the ceiling, if the level has one, so it never draws over the
    ceiling in front of it. Its texture repeats every cell of height and is
    sampled per pixel; where that texel is not solid ({!Texture.alpha}) the
    wall is blended rather than written, so a grille or a window unveils what is
    behind. Decals — pictures placed by {!Room.along} and height — are blended
    over the wall's own texture, in the same light. *)
let draw_wall fb viewport ~air room (player : Player.t) ~column ~dir ~occlude
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
  let floor_z = Plane.elevation (Room.floor_plane room) hit_point in
  (* The wall rises to its own height, but a roof caps it so it never draws over
     the ceiling in front of it; with open sky there is nothing to cap against. *)
  let top_z =
    match Room.ceiling room with
    | Room.Roof ceiling ->
        Float.min (floor_z +. w.Room.height)
          (Plane.elevation ceiling.Room.plane hit_point)
    | Room.Open _ -> floor_z +. w.Room.height
  in
  if top_z > floor_z then begin
    let y_foot = project_height viewport ~z:floor_z ~distance:d in
    let y_top = project_height viewport ~z:top_z ~distance:d in
    let first =
      Int.max clip_first (Int.max 0 (int_of_float (Float.round y_top)))
    in
    let last =
      Int.min clip_last
        (Int.min
           (fb.Framebuffer.height - 1)
           (int_of_float (Float.round y_foot)))
    in
    (* One light factor — orientation and fog — dims both the wall and its
       decals, so a decal sits in the same light as the wall it is on unless it
       says otherwise; {!Room.decal_light} is where a decal that makes its own
       light lifts itself off this.

       The two halves of it are combined differently, and have to be.
       Orientation is a multiply, because a wall turned away from the light goes
       {e dark}. Distance is a blend towards {!Atmosphere.haze}, because a wall
       far away goes the colour of the air in front of it. So the haze arrives
       at [1. -. fog] — the fog alone, with no orientation in it. Folding the
       shading in there as well would make a wall facing away from the light
       fade into the distance faster than the wall beside it, and in air
       brighter than the wall it would come out {e brighter} for facing away. *)
    let fog = Atmosphere.fog air d in
    let light = Atmosphere.face_shading air w.Room.normal *. fog in
    (* Written out, the pixel is [texel * face_shading * fog + haze * (1 - fog)]
       — so the factor on the texel is exactly [light], and the rest is one
       colour that holds all the way down the column. Which is what keeps this
       loop in integers: the conversion out of floating point still belongs out
       here, and the whole of the haze is one addition per channel. *)
    let level = clamp8 (int_of_float (light *. 255.)) in
    let veil = Color.shade air.Atmosphere.haze (1. -. fog) in
    let pattern = w.Room.material.Material.pattern in
    let u =
      Texture.column_of_offset pattern
        (hit.Ray.along -. Float.floor hit.Ray.along)
    in
    (* The decals whose horizontal span this column falls in and which are on
       the face being looked at, with the texture column of each — all constant
       down the column. Which face that is comes from where the eye stands, not
       from where the ray landed, so a wall seen edge-on does not flicker
       between its two sides. *)
    let seen_from = Room.side_of w player.Player.pos in
    let decals =
      List.filter_map
        (fun (dec : Room.decal) ->
          Option.map
            (fun ui ->
              (* {!Room.decal_light} is a fraction raised towards [1.] by the
                 decal's [glow], and the fog factor is such a fraction — the
                 part of a surface the haze does not replace — so the same lift
                 gives the decal its own fog. That is what makes glow carry a
                 mark out of the haze as well as out of the dark: at [glow = 1.]
                 its own fog is [1.], the haze's share of it is nothing, and the
                 mark is drawn in the colours it was painted in however far down
                 the room it hangs. Lifting only the light would leave it at
                 full brightness under a coat of haze, which at the far end of a
                 long room is the haze and not the mark. *)
              let own_fog = Room.decal_light dec ~light:fog in
              ( dec,
                ui,
                Room.decal_light dec ~light,
                Color.shade air.Atmosphere.haze (1. -. own_fog) ))
            (Room.decal_column dec ~seen_from ~along:hit.Ray.along))
        w.Room.decals
    in
    for y = first to last do
      (* An opaque wall is painted unconditionally (nearer opaque walls are drawn
         after it) and records its distance; a see-through wall is only drawn
         where it stands nearer than the opaque wall already noted for this
         pixel, so a short wall in front hides only the part behind it. *)
      let index = (y * width) + column in
      if occlude || d < depth.(index) then begin
        (* World height this pixel looks at on the wall, which
           [Texture.row_of_height] turns into a texel row — it holds the tiling
           and the flip, so that {!Sight} can ask the same question of the same
           point and get the texel that was drawn here.

           [Viewport.row_factor] written out and multiplied by [d], because it
           is wanted once per pixel of the strip rather than once per row. The
           half is that function's, not a rounding: it is what makes this the
           row's centre, and dropping it would sample the floor and this wall
           half a pixel apart. *)
        let z =
          viewport.eye_z
          -. (float_of_int y +. 0.5 -. viewport.horizon)
             *. d /. viewport.projection
        in
        let above_foot = z -. floor_z in
        let v = Texture.row_of_height pattern above_foot in
        let texel = Texture.sample pattern ~u ~v in
        let a = Texture.alpha pattern ~u ~v in
        (* Clamped, which the multiply alone did not need to be: [level] and a
           texel are both at most 255, so their product was too. An atmosphere
           whose [ambient] and [directional] add to more than one can saturate
           that and still have haze to lay over it. *)
        let cr = clamp8 ((texel.Color.r * level / 255) + veil.Color.r)
        and cg = clamp8 ((texel.Color.g * level / 255) + veil.Color.g)
        and cb = clamp8 ((texel.Color.b * level / 255) + veil.Color.b) in
        if a = 255 then begin
          Framebuffer.set fb ~x:column ~y ~r:cr ~g:cg ~b:cb;
          if occlude then depth.(index) <- d
        end
        else if a > 0 then
          Framebuffer.blend fb ~x:column ~y ~r:cr ~g:cg ~b:cb ~alpha:a;
        List.iter
          (fun ((dec : Room.decal), ui, lit, veil) ->
            (* A decal hangs a height above the {e floor} under the wall, not at
               an absolute elevation, so it is placed against [above_foot] — on
               a sloped floor it then rides with the wall instead of tilting
               across it. *)
            match Room.decal_row dec ~above:above_foot with
            | None -> ()
            | Some vi ->
                let img = dec.Room.image in
                let idx = Image.index img ~u:ui ~v:vi in
                let da = img.Image.alpha.(idx) in
                if da > 0 then
                  let c = img.Image.pixels.(idx) in
                  Framebuffer.blend fb ~x:column ~y
                    ~r:
                      (clamp8
                         (int_of_float (float_of_int c.Color.r *. lit)
                         + veil.Color.r))
                    ~g:
                      (clamp8
                         (int_of_float (float_of_int c.Color.g *. lit)
                         + veil.Color.g))
                    ~b:
                      (clamp8
                         (int_of_float (float_of_int c.Color.b *. lit)
                         + veil.Color.b))
                    ~alpha:da)
          decals
      end
    done
  end

type mask =
  | Full
  | Within of { column : int; first : int; last : int }
      (** Which pixels a translucent thing seen through a chain of doorways is
          allowed to touch. Everything drawn inside a portal is drawn one column
          at a time, so a mask is never more than a single column and a row
          range within it — the rows the opening covered {e in that column}.
          That makes the clip exact rather than a bounding box: a sprite seen
          through a doorway is trimmed to the doorway's own outline, not to a
          rectangle around it. [Full] is the player's own room, clipped by
          nothing. *)

(* Draw one sprite as a billboard: a flat image that always faces the player,
    at perpendicular distance [depth_s]. {!Viewport.sprite_box} says where it
    lands — foot and head projected against the sloped floor under it, width
    from the picture's own shape — and it is drawn per pixel only where it
    stands nearer than the opaque wall noted there, so a wall in front — even a
    short one — hides just the part of it behind.

    The two loops below interpolate across that rectangle rather than asking
    {!Room.sprite_column} and {!Room.sprite_row} per pixel. That is the same
    rule read from the screen end: the rectangle came from those two, the
    mapping across it is linear in both directions, and doing it this way costs
    one divide per column instead of a dot product per pixel. *)
let draw_sprite fb viewport ~air room (player : Player.t) (s : Room.sprite)
    ~depth_s ~mask =
  let open Viewport in
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let depth = fb.Framebuffer.depth in
  let floor_z = Plane.elevation (Room.floor_plane room) s.Room.pos in
  let left, y_top, rightx, y_base =
    Viewport.sprite_box viewport player ~floor_z ~distance:depth_s s
  in
  let span = rightx -. left and vspan = y_base -. y_top in
  let img = s.Room.image in
  let nu = img.Image.width and nv = img.Image.height in
  let f = Atmosphere.fog air depth_s in
  (* A billboard stands at one distance, so the air in front of it is one colour
     for the whole picture: the haze's share of it, worked out here and added to
     what is left of each texel below. *)
  let veil = Color.shade air.Atmosphere.haze (1. -. f) in
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
    (* How far across the picture this column's centre falls. [left] and
       [y_top] are continuous edges, the columns and rows above are what
       [Float.round] made of them, and the half is what puts the two on the same
       footing — the same one every ray and every wall texel is sampled at. *)
    let ui =
      clampi nu
        (int_of_float
           ((float_of_int col +. 0.5 -. left) /. span *. float_of_int nu))
    in
    for y = row0 to row1 do
      if depth_s < depth.((y * width) + col) then begin
        let vi =
          clampi nv
            (int_of_float
               ((float_of_int y +. 0.5 -. y_top) /. vspan *. float_of_int nv))
        in
        let idx = Image.index img ~u:ui ~v:vi in
        let a = img.Image.alpha.(idx) in
        if a > 0 then
          let c = img.Image.pixels.(idx) in
          Framebuffer.blend fb ~x:col ~y
            ~r:
              (clamp8
                 (int_of_float (float_of_int c.Color.r *. f) + veil.Color.r))
            ~g:
              (clamp8
                 (int_of_float (float_of_int c.Color.g *. f) + veil.Color.g))
            ~b:
              (clamp8
                 (int_of_float (float_of_int c.Color.b *. f) + veil.Color.b))
            ~alpha:a
      end
    done
  done

type fragment = {
  distance : float;
  pose : Player.t;
  room : int;
  mask : mask;
  what : fragment_kind;
}
(* A translucent thing to composite after the opaque geometry: a sprite, or a
    strip of a see-through wall in one column. Both let what is behind show
    through, so they cannot each be given their own pass — a sprite in front of
    a window must cover it, one behind it must show through — they have to be
    sorted together and drawn farthest first. *)

and fragment_kind =
  | Sprite of Room.sprite
  | See_through of int * Vec.t * Ray.hit

(* Draw one column of one room, and — through any open doorway the column meets
    — of its neighbours behind it, clipped to [clip] rows.

    Everything the ray meets, wall or threshold, joins one stream ordered
    farthest first, and the painter's algorithm does the rest: a wall beyond a
    doorway is painted before it and gets covered, a wall nearer is painted
    after and covers it. A threshold with a leaf draws as a wall of that
    texture. An open one recurses, after carrying the camera and the ray into
    the neighbour's frame, with the clip narrowed to the rows the opening covers
    in this column.

    A leaf or a lintel you can see through recurses {e as well as} drawing: the
    neighbour fills those rows first and the translucent pass blends the leaf
    over it. That is what [behind] below is for — the recursion is reached from
    three places rather than one, and a threshold wearing see-through glass both
    above and across it pays for it twice in a column, which is the price of the
    two being separate surfaces.

    The {!Viewport} is deliberately not rebuilt for the nested room. A rigid
    motion is horizontal, so eye height, projection and horizon all carry over
    unchanged, and — because it also preserves distance — a distance measured
    three rooms deep is directly comparable with everything already in the
    shared depth buffer. Only [pos], [dir] and [right] move.

    [near] is how far away the doorway this room is being drawn through was met,
    and nothing at or nearer than it is drawn: not a wall, not another opening,
    not a sprite. It is not an approximation of the doorway but the whole of it.
    Along one ray, which side of the threshold's line a point falls on is affine
    in the distance and changes sign exactly where the ray crosses it — and the
    recursion is only reached when the ray crossed the opening itself — so past
    [near] is precisely what is beyond the doorway, and the wedge between the
    jambs collapses, per column, to one number.

    Which matters because the camera arrives {e behind} the neighbour's copy of
    the opening. Everything that room has standing on the near side of its own
    doorway is therefore in front of the ray, and in the room the player is
    actually in rather than in the picture the doorway shows. A convex room never
    has anything there; one that folds back on itself does, and without this
    drawing it would cover the room it was supposed to be a window onto.

    Nothing accumulates. A rigid motion preserves distance, so every room is on
    one scale — the same fact the shared depth buffer above rests on — and each
    recursion's [near] is an opening that already passed the previous one, so it
    only ever grows.

    [entered] is the threshold this room was reached through, which must be
    ignored. Stepping through a doorway lands the camera {e behind} the
    neighbour's own copy of it — that is what standing in a doorway looking in
    means — so the ray meets that opening again immediately, and without this
    the recursion would bounce straight back where it came from and spend its
    whole budget going nowhere. Kept as well as [near], which stands at exactly
    that threshold's distance: taking it out that way would rest on an equality
    between two floats that have been through a transform.

    Rooms may form a cycle, so it is [budget] and nothing else that ends the
    recursion; when it runs out the opening is filled with the world's
    {!Atmosphere.haze}, the same colour the planes already fade into. A doorway
    onto a room that has not been built yet takes the same fill, so a world
    still being grown renders rather than raising. *)
let rec draw_room_column fb viewport world ~room ~pose ~column ~dir
    ~clip:(top, bottom) ~near ~budget ~entered ~translucent =
  let current = World.room world room in
  let air = World.atmosphere world in
  draw_planes fb viewport ~air current pose ~column ~dir ~near ~first:top
    ~last:bottom;
  (* One wall strip: painted straight over the column if it is opaque, held back
     for the translucent pass if you can see through it. *)
  let paint ~first ~last (hit : Ray.hit) =
    if Material.opaque hit.Ray.wall.Room.material then
      draw_wall fb viewport ~air current pose ~column ~dir ~occlude:true ~first
        ~last hit
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
     standing over the gap. [index] is the threshold's own, since that is what
     this stands for; nothing on this path reads it, but a hit has to carry
     something and a wall index would be a lie. *)
  let as_wall threshold ~index ~height ~material ~distance ~along =
    let wall = Room.threshold_wall threshold ~height ~material in
    { Ray.distance; along; wall; index }
  in
  List.iter
    (fun step ->
      match step with
      (* In front of the doorway this room is being drawn through, and so not in
         this room's picture at all. Guarded rather than filtered out of the
         stream: it is ordered farthest first, so these are its tail, and a
         [List.filter] would allocate a copy of it every column. *)
      | Ray.Wall hit when hit.Ray.distance <= near -> ()
      | Ray.Wall hit -> paint ~first:top ~last:bottom hit
      (* The doorway we are already looking through. *)
      | Ray.Opening opening when entered = Some opening.Ray.index -> ()
      (* An opening in front of the doorway goes the same way, and the whole arm
         of it: no lintel, no leaf, no recursion — and no [nothing_behind]
         either, which would blank rows that belong to the room in front. *)
      | Ray.Opening opening when opening.Ray.distance <= near -> ()
      | Ray.Opening opening ->
          let threshold = Room.threshold_at current opening.Ray.index in
          let distance = opening.Ray.distance in
          let hit_point = Vec.add pose.Player.pos (Vec.scale dir distance) in
          let floor_z = Plane.elevation (Room.floor_plane current) hit_point in
          let row z =
            int_of_float
              (Float.round (Viewport.project_height viewport ~z ~distance))
          in
          let head = Int.max top (row (floor_z +. threshold.height))
          and foot = Int.min bottom (row floor_z) in
          (* A height capped at the roof over this point, the same way
             [draw_wall] caps a wall. What an opening shows is bounded by it as
             much as what stands beside one is: above the ceiling are rows
             [draw_planes] has already given this room's own roof, and the room
             beyond may no more paint over them than the strip may. *)
          let under_roof z =
            match Room.ceiling current with
            | Room.Roof ceiling ->
                Float.min z (Plane.elevation ceiling.Room.plane hit_point)
            | Room.Open _ -> z
          in
          (* Out of budget, or a doorway onto a room that has not been built
             yet: either way there is nothing behind it to draw, so those rows
             take the colour the distance fog already fades into. *)
          let nothing_behind ~first ~last =
            let h = air.Atmosphere.haze in
            for y = first to last do
              Framebuffer.set fb ~x:column ~y ~r:h.Color.r ~g:h.Color.g
                ~b:h.Color.b
            done
          in
          (* The neighbour, in the rows this much of the opening covers. Reached
             from three places: a threshold with nothing across it, and a leaf
             or a lintel you can see through, which have to have something drawn
             behind them or their clear texels show this room's own floor. *)
          let behind ~first ~last =
            match World.portal world ~room ~threshold:opening.Ray.index with
            | Some portal when budget > 0 ->
                let nested =
                  Player.through portal.World.onto ~room:portal.World.to_room
                    pose
                in
                let mask = Within { column; first; last } in
                let there = World.room world portal.World.to_room in
                for s = 0 to Room.sprite_count there - 1 do
                  let sprite = Room.sprite_at there s in
                  (* Named apart from [distance], which is this doorway's, and
                       is what a sprite of the far room standing in front of it
                       has to clear. The two are the same measurement: the ray's
                       direction is [dir + right * k] with [right] across the
                       view, so its parameter is already the distance along
                       [dir] that this dot product is. *)
                  let depth_s =
                    Vec.dot
                      (Vec.sub sprite.Room.pos nested.Player.pos)
                      nested.Player.dir
                  in
                  if depth_s > Config.sprite_near_clip && depth_s > distance
                  then
                    translucent :=
                      {
                        distance = depth_s;
                        pose = nested;
                        room = portal.World.to_room;
                        mask;
                        what = Sprite sprite;
                      }
                      :: !translucent
                done;
                draw_room_column fb viewport world ~room:portal.World.to_room
                  ~pose:nested ~column
                  ~dir:(Transform.direction portal.World.onto dir)
                  ~clip:(first, last) ~near:distance ~budget:(budget - 1)
                  ~entered:(Some portal.World.twin) ~translucent
            | Some _ | None -> nothing_behind ~first ~last
          in
          (* The wall above the opening — the strip that is what you meet over
             the top of a closed door. No lintel means no strip: the gap already
             runs the full height of the wall it was cut into, so there is
             nothing here to draw and the rows above it keep the ceiling
             [draw_planes] has already put there. A band with no rows in it is
             skipped rather than handed on: [paint] would draw nothing anyway,
             but [behind] would cast a whole ray to do it. *)
          Option.iter
            (fun (l : Room.lintel) ->
              let last = Int.min bottom (head - 1) in
              if top <= last then begin
                if not (Material.opaque l.Room.material) then begin
                  (* Where the strip itself begins, which is where [draw_wall]
                     will start painting the glass: over the top of it the strip
                     is not drawn, so there is nothing for the neighbour to show
                     through and the ceiling above stays. *)
                  let first =
                    Int.max top (row (under_roof (floor_z +. l.Room.top)))
                  in
                  if first <= last then behind ~first ~last
                end;
                paint ~first:top ~last
                  (as_wall threshold ~index:opening.Ray.index ~height:l.Room.top
                     ~material:l.Room.material ~distance
                     ~along:opening.Ray.along)
              end)
            threshold.lintel;
          if head <= foot then begin
            (* The opening's head, or the ceiling where that hangs below it. A
               gap in a wall is no way past the roof over it. *)
            let mouth =
              Int.max head (row (under_roof (floor_z +. threshold.height)))
            in
            match Room.leaf threshold with
            | Some material ->
                (* A leaf you can see through is a wall you can see through: the
                   room beyond has to be there for its clear texels to show, and
                   [paint] holds the leaf itself back for the translucent pass,
                   which blends it over what this just drew. *)
                if (not (Material.opaque material)) && mouth <= foot then
                  behind ~first:mouth ~last:foot;
                paint ~first:head ~last:foot
                  (as_wall threshold ~index:opening.Ray.index
                     ~height:threshold.height ~material ~distance
                     ~along:opening.Ray.along)
            | None -> if mouth <= foot then behind ~first:mouth ~last:foot
          end)
    (Ray.merge
       (Ray.cast current ~origin:pose.Player.pos ~direction:dir)
       (Ray.openings current ~origin:pose.Player.pos ~direction:dir))

(* Composite the sprites and see-through walls together, farthest first, each
    still hidden by a nearer opaque wall in its column. Interleaving them is
    what lets a sprite occlude a window behind it and show through one in front.
*)
let draw_translucent fb viewport world fragments =
  let air = World.atmosphere world in
  List.iter
    (fun fragment ->
      let current = World.room world fragment.room in
      match fragment.what with
      | Sprite s ->
          draw_sprite fb viewport ~air current fragment.pose s
            ~depth_s:fragment.distance ~mask:fragment.mask
      | See_through (column, dir, hit) ->
          let first, last =
            match fragment.mask with
            | Full -> (0, fb.Framebuffer.height - 1)
            | Within m -> (m.first, m.last)
          in
          draw_wall fb viewport ~air current fragment.pose ~column ~dir
            ~occlude:false ~first ~last hit)
    (List.stable_sort
       (fun a b -> Float.compare b.distance a.distance)
       fragments)

let draw_frame fb world (player : Player.t) =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let current = World.room world player.room in
  let eye_z =
    Plane.elevation (Room.floor_plane current) player.Player.pos
    +. Config.eye_height
  in
  let viewport =
    Viewport.make ~pitch:player.Player.pitch ~eye_z ~width ~height
  in
  Framebuffer.clear_depth fb;
  let translucent = ref [] in
  for s = 0 to Room.sprite_count current - 1 do
    let sprite = Room.sprite_at current s in
    let distance = Vec.dot (Vec.sub sprite.Room.pos player.pos) player.dir in
    if distance > Config.sprite_near_clip then
      translucent :=
        {
          distance;
          pose = player;
          room = player.room;
          mask = Full;
          what = Sprite sprite;
        }
        :: !translucent
  done;
  for column = 0 to width - 1 do
    let dir = Viewport.ray_direction viewport player ~column in
    draw_room_column fb viewport world ~room:player.room ~pose:player ~column
      ~dir
      ~clip:(0, height - 1)
      ~near:0. ~budget:Config.max_portal_depth ~entered:None ~translucent
  done;
  draw_translucent fb viewport world !translucent

(* (Re)size the framebuffer to match [width] x [height], reusing it when the
    size has not changed. *)
let ensure sdl fb_ref ~width ~height =
  let current = !fb_ref in
  if current.Framebuffer.width = width && current.Framebuffer.height = height
  then Ok ()
  else
    let+ next = Framebuffer.make sdl ~width ~height in
    Framebuffer.destroy current;
    fb_ref := next

let fit sdl fb_ref =
  let* out_w, out_h = Sdl.get_renderer_output_size sdl in
  let width, height = internal_size ~width:out_w ~height:out_h in
  ensure sdl fb_ref ~width ~height

let render sdl fb_ref ?(overlay = fun _ -> ()) world player =
  let* () = fit sdl fb_ref in
  (* Asked again rather than threaded out of {!fit}, which has no use for it:
     this is the destination rectangle and not the buffer's size, and one more
     query of a size SDL already has costs nothing beside a frame. *)
  let* out_w, out_h = Sdl.get_renderer_output_size sdl in
  draw_frame !fb_ref world player;
  overlay !fb_ref;
  let+ () =
    Framebuffer.present sdl !fb_ref
      ~dst:(Sdl.Rect.create ~x:0 ~y:0 ~w:out_w ~h:out_h)
  in
  Sdl.render_present sdl
