(** The level: a set of wall segments in the flat world, with a floor {!Plane}
    below them and, optionally, a ceiling {!Plane} above.

    A grid raycaster can only place walls on cell edges, so every wall faces
    north, south, east or west. Here a wall is an arbitrary line {e segment}
    instead, so a room may have any number of walls facing any direction — an
    octagon, a triangle, a wedge. Each wall also carries its own height, a
    texture id (some see-through, see {!Palette}) and any {!decal}s hung on it;
    the floor is an inclined plane so it need not be horizontal; the ceiling is
    an {e optional} inclined plane — [None] leaves the level open to the {!Sky};
    and {!sprite}s stand in the world as billboards facing the player. *)

type decal = {
  along : float;
  z : float;
  half_width : float;
  half_height : float;
  image : Image.t;
}
(** A decoration hung flat on a wall — a painting, a poster — drawn over the
    wall's own texture. It is placed by how far [along] the wall it sits and how
    high above the floor ([z]), and reaches [half_width] to each side and
    [half_height] up and down. *)

type wall = {
  a : Vec.t;  (** one endpoint *)
  b : Vec.t;  (** the other *)
  height : float;  (** how far the wall rises above the floor, in cells *)
  texture : int;  (** selects colour and pattern, see {!Palette} *)
  decals : decal list;  (** decorations over the texture *)
  edge : Vec.t;  (** [b - a], precomputed for intersection tests *)
  length : float;  (** [|b - a|] *)
  normal : Vec.t;  (** unit vector perpendicular to the wall, for shading *)
}

type sprite = { pos : Vec.t; size : float; image : Image.t }
(** An object or character standing in the world, drawn as a billboard — a flat
    {!Image} that always faces the player. It stands at [pos] on the floor and
    is [size] cells tall. *)

type t = {
  walls : wall array;
  spawn : Vec.t;
  floor : Plane.t;
  ceiling : Plane.t option;  (** [None] is open sky, see {!Sky} *)
  sprites : sprite array;
}

(** Build a wall between two points, precomputing the quantities the renderer
    and the ray caster would otherwise recompute every frame. *)
let wall ~height ~texture ?(decals = []) a b =
  let edge = Vec.sub b a in
  {
    a;
    b;
    height;
    texture;
    decals;
    edge;
    length = Vec.length edge;
    normal = Vec.normalize (Vec.perp edge);
  }

let make ~spawn ~floor ~ceiling ?(sprites = []) walls =
  {
    walls = Array.of_list walls;
    spawn;
    floor;
    ceiling;
    sprites = Array.of_list sprites;
  }

(** Shortest distance from a point to a wall segment: project the point onto the
    line, clamp to the segment's ends, and measure to that nearest point. *)
let distance_to_wall (w : wall) (p : Vec.t) =
  if w.length = 0. then Vec.length (Vec.sub p w.a)
  else
    let s = Vec.dot (Vec.sub p w.a) w.edge /. (w.length *. w.length) in
    let s = Float.max 0. (Float.min 1. s) in
    let foot = Vec.add w.a (Vec.scale w.edge s) in
    Vec.length (Vec.sub p foot)

(** Is [p] too close to any wall to stand there? The player is treated as a
    small disc of radius {!Config.collision_padding}, so it stops a little short
    of a wall rather than pressing its nose flat against it. *)
let blocked t (p : Vec.t) =
  Array.exists
    (fun w -> distance_to_wall w p < Config.collision_padding)
    t.walls

(** Do the two segments [a1..a2] and [b1..b2] cross? Solved with the same cross
    product as {!Ray.cast}: the crossing exists when both parameters land in
    [0, 1].

    That solution does not exist for parallel segments, but two of them can
    still lie on the same line and overlap — a step taken straight along a wall
    — which counts as a crossing just as much. Those are settled separately, by
    projecting [b1..b2] onto [a1..a2] and asking whether the two spans meet. *)
let segments_cross a1 a2 b1 b2 =
  let d1 = Vec.sub a2 a1 and d2 = Vec.sub b2 b1 in
  let denom = Vec.cross d1 d2 in
  let off = Vec.sub b1 a1 in
  if Float.abs denom < 1e-12 then
    let length = Vec.length d1 in
    (* Parallel, so only an overlap of collinear segments is left to find: the
       cross product below is the offset of [b1] from the line of [a1..a2],
       times that line's length. *)
    if length = 0. || Float.abs (Vec.cross off d1) > 1e-9 *. length then false
    else
      let project p = Vec.dot (Vec.sub p a1) d1 /. (length *. length) in
      let s = project b1 and e = project b2 in
      Float.max (Float.min s e) 0. <= Float.min (Float.max s e) 1.
  else
    let t = Vec.cross off d2 /. denom and u = Vec.cross off d1 /. denom in
    t >= 0. && t <= 1. && u >= 0. && u <= 1.

(** May the player step from [from] to [dest]? Refused if the destination is too
    close to a wall, or if the path crosses one outright — the latter matters
    because a single step can be longer than the padding and would otherwise
    tunnel straight through a thin wall. *)
let can_step t ~from ~dest =
  (not (blocked t dest))
  && not (Array.exists (fun w -> segments_cross from dest w.a w.b) t.walls)

(** Walls following a run of points; [closed] joins the last point back to the
    first, turning a polyline into a polygon. *)
let path ?(closed = false) ~height ~texture points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  let last = if closed then n - 1 else n - 2 in
  List.init
    (Int.max 0 (last + 1))
    (fun i -> wall ~height ~texture arr.(i) arr.((i + 1) mod n))

(** A regular polygon of [sides] walls, [radius] from [center], turned by
    [rotation]. A cheap way to draw rooms and pillars whose walls face every
    direction. *)
let regular_polygon ~center ~radius ~sides ~rotation ~height ~texture =
  path ~closed:true ~height ~texture
    (List.init sides (fun k ->
         let angle =
           rotation +. (float_of_int k *. 2. *. Float.pi /. float_of_int sides)
         in
         Vec.add center (Vec.make (radius *. cos angle) (radius *. sin angle))))

(** A demonstration level, built to exercise the whole engine at once: a bounded
    arena of tall outer walls, open to the {!Sky} rather than roofed, holding
    several distinct areas —

    - a plaza of pillars around the spawn, walls facing every way,
    - an enclosed hall to the east with a doorway you walk through,
    - a triangular nook to the north,
    - a garden of low walls to the west that you can see over,
    - a lone tall monolith to the south —

    with walls of every height and texture standing on a floor that tilts up
    towards +x. *)
let default =
  let outer =
    (* A twelve-sided wall ring, tall enough to close off the sky at the edges. *)
    regular_polygon ~center:(Vec.make 0. 0.) ~radius:17. ~sides:12 ~rotation:0.
      ~height:7. ~texture:3
  in
  let pillars =
    (* Six square pillars ringed around the spawn, each a different height and
       texture, so you weave between them and see over the low ones. *)
    List.concat
      (List.init 6 (fun k ->
           let angle = float_of_int k *. Float.pi /. 3. in
           let center = Vec.make (6. *. cos angle) (6. *. sin angle) in
           let height = [| 3.5; 0.6; 2.2; 4.5; 1.3; 2.8 |].(k) in
           regular_polygon ~center ~radius:0.6 ~sides:4 ~rotation:0.6 ~height
             ~texture:(1 + (k mod 4))))
  in
  let east_hall =
    (* A rectangular hall with a doorway in its west wall, a bench inside, and a
       tall pillar in the far corner. *)
    path ~height:4.5 ~texture:1
      [
        Vec.make 9. (-1.);
        Vec.make 9. (-5.);
        Vec.make 15. (-5.);
        Vec.make 15. 5.;
        Vec.make 9. 5.;
        Vec.make 9. 1.;
      ]
    @ [ wall ~height:0.5 ~texture:2 (Vec.make 12. (-4.)) (Vec.make 14. (-4.)) ]
    @ regular_polygon ~center:(Vec.make 13. 3.) ~radius:0.7 ~sides:4
        ~rotation:0.3 ~height:4.5 ~texture:4
  in
  let north_nook =
    (* Two angled walls, open along the base towards the plaza. *)
    path ~height:2.6 ~texture:4
      [ Vec.make (-3.) 9.; Vec.make 0. 14.; Vec.make 3. 9. ]
  in
  let west_garden =
    (* A winding low wall you look over into the arena and sky beyond. *)
    path ~height:0.5 ~texture:2
      [
        Vec.make (-14.) (-3.);
        Vec.make (-9.) (-2.);
        Vec.make (-11.) 2.;
        Vec.make (-8.) 4.5;
        Vec.make (-13.) 6.;
      ]
  in
  let monolith =
    [
      wall ~height:6. ~texture:1 (Vec.make (-4.) (-9.)) (Vec.make (-2.5) (-11.));
    ]
  in
  let gallery =
    (* A brick wall to the south hung with a painting and a poster. *)
    [
      wall ~height:3.2 ~texture:1 (Vec.make (-3.) (-4.)) (Vec.make 3. (-4.))
        ~decals:
          [
            {
              along = 2.;
              z = 1.6;
              half_width = 0.9;
              half_height = 0.9;
              image = Image.painting;
            };
            {
              along = 4.;
              z = 1.6;
              half_width = 0.7;
              half_height = 0.9;
              image = Image.poster;
            };
          ];
    ]
  in
  let see_through =
    (* A steel grille and a leaded window, each with something to look at behind
       it. *)
    [
      wall ~height:2. ~texture:5 (Vec.make (-4.) 4.) (Vec.make 1. 4.);
      wall ~height:2.6 ~texture:6 (Vec.make 3. 3.) (Vec.make 6. 3.);
    ]
  in
  let sprites =
    [
      { pos = Vec.make 2.6 0.7; size = 0.9; image = Image.barrel };
      { pos = Vec.make 2.6 (-0.8); size = 1.8; image = Image.figure };
      { pos = Vec.make (-1.5) 5.2; size = 1.8; image = Image.figure };
      { pos = Vec.make 4.5 4.3; size = 0.9; image = Image.barrel };
      { pos = Vec.make 12. (-2.); size = 0.9; image = Image.barrel };
    ]
  in
  make ~spawn:(Vec.make 0. 0.)
    ~floor:(Plane.make ~a:0.06 ~b:0.03 ~c:0.)
    ~ceiling:None ~sprites
    (List.concat
       [
         outer;
         pillars;
         east_hall;
         north_nook;
         west_garden;
         monolith;
         gallery;
         see_through;
       ])
