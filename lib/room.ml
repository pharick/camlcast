(** A room: a set of wall segments in its own flat coordinate frame, with a floor {!Plane}
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

type lintel = { top : float; texture : int }
(** The wall a doorway is cut into, so the renderer can fill the strip left
    above the opening: [top] is how far that wall rises above the floor and
    [texture] is its id. Without one the opening runs the full height of
    whatever it is cut into and there is nothing above it to draw. *)

type threshold = {
  name : string;  (** what a {!World} link refers to it by *)
  a : Vec.t;  (** one endpoint *)
  b : Vec.t;  (** the other *)
  height : float;  (** how tall the opening is above the floor *)
  door : int option;
      (** [Some texture] hangs a solid leaf across it, [None] leaves it open *)
  lintel : lintel option;  (** the wall above the opening, if any *)
  edge : Vec.t;  (** [b - a], precomputed exactly as on a {!type-wall} *)
  length : float;  (** [|b - a|] *)
  normal : Vec.t;  (** unit vector perpendicular to the opening *)
}
(** A doorway in the room boundary — the gap in the wall loop that a {!World}
    link joins to a doorway of another room.

    Its endpoints must be given in the {b same winding direction as the room's
    own boundary walls}, because {!Transform.between} pairs linked endpoints in
    reverse: the two rooms describe the same opening from opposite sides, so
    walking the boundary through it in one room runs the other way in the other.
    {!Transform.between}'s docstring carries the argument in full.

    An open threshold is a portal — the neighbour is drawn through it, and the
    player walks through. One with a [door] draws as a leaf of that texture
    instead; walking into it still crosses. *)

type t = {
  walls : wall array;
  thresholds : threshold array;
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

(** Build a doorway between two points, precomputing the same quantities as
    {!type-wall} — a threshold is intersected by exactly the same ray test. *)
let threshold ~name ~height ?door ?lintel a b =
  let edge = Vec.sub b a in
  {
    name;
    a;
    b;
    height;
    door;
    lintel;
    edge;
    length = Vec.length edge;
    normal = Vec.normalize (Vec.perp edge);
  }

let make ?(thresholds = []) ~floor ~ceiling ?(sprites = []) walls =
  {
    walls = Array.of_list walls;
    thresholds = Array.of_list thresholds;
    floor;
    ceiling;
    sprites = Array.of_list sprites;
  }

(** Shortest distance from a point to the segment [a..b]: project the point onto
    the line, clamp to the segment's ends, and measure to that nearest point. *)
let distance_to_segment (p : Vec.t) ~a ~b =
  let edge = Vec.sub b a in
  let length2 = Vec.dot edge edge in
  if length2 = 0. then Vec.length (Vec.sub p a)
  else
    let s =
      Float.max 0. (Float.min 1. (Vec.dot (Vec.sub p a) edge /. length2))
    in
    let foot = Vec.add a (Vec.scale edge s) in
    Vec.length (Vec.sub p foot)

let distance_to_wall (w : wall) (p : Vec.t) =
  distance_to_segment p ~a:w.a ~b:w.b

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

(** Shortest distance between the segments [a1..a2] and [b1..b2]. Segments that
    cross are no distance apart at all; for two that miss, the closest pair of
    points must include an endpoint of one of them — slide along either segment
    away from an interior closest point and the distance would keep falling — so
    the four endpoint-to-segment distances cover every remaining case. *)
let distance_between_segments a1 a2 b1 b2 =
  if segments_cross a1 a2 b1 b2 then 0.
  else
    let to_b p = distance_to_segment p ~a:b1 ~b:b2
    and to_a p = distance_to_segment p ~a:a1 ~b:a2 in
    Float.min (Float.min (to_b a1) (to_b a2)) (Float.min (to_a b1) (to_a b2))

(** May the player step from [from] to [dest]? The player is a disc of radius
    {!Config.collision_padding}, so the step sweeps that disc along the segment
    [from..dest] and is refused when the swept shape touches a wall — that is,
    when the step comes within the padding of a wall {e anywhere along the way}.

    Testing the whole path and not just the destination is what makes a step
    longer than the padding safe: it can neither tunnel through a thin wall nor
    clip past the end of one, both of which land clear of every wall and would
    pass a test taken at the destination alone. *)
let can_step t ~from ~dest =
  not
    (Array.exists
       (fun (w : wall) ->
         distance_between_segments from dest w.a w.b < Config.collision_padding)
    t.walls)

(** Walls following a run of points; [closed] joins the last point back to the
    first, turning a polyline into a polygon. *)
let path ?(closed = false) ~height ~texture points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  let last = if closed then n - 1 else n - 2 in
  List.init
    (Int.max 0 (last + 1))
    (fun i -> wall ~height ~texture arr.(i) arr.((i + 1) mod n))

(** Cut a doorway into the wall that would otherwise run from [a] to [b]: the
    two jambs left either side of a gap [width] wide in the middle, and the
    {!type-threshold} filling that gap, [opening] tall.

    The threshold comes out wound the same way as the wall it replaces, which is
    the winding rule {!Transform.between} depends on, and it takes the wall's
    own height and texture as its {!type-lintel}, so the strip left above the
    opening is still drawn. Cutting both sides of a doorway this way is what
    keeps a room's boundary and its thresholds honest about each other. *)
let doorway ~name ?door ~width ~opening ~height ~texture a b =
  let edge = Vec.sub b a in
  let half = Vec.scale edge (width /. (2. *. Vec.length edge)) in
  let middle = Vec.scale (Vec.add a b) 0.5 in
  let p = Vec.sub middle half and q = Vec.add middle half in
  ( [ wall ~height ~texture a p; wall ~height ~texture q b ],
    threshold ~name ?door ~height:opening ~lintel:{ top = height; texture } p q
  )

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
