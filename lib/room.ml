(** A room: a set of wall segments in its own flat coordinate frame, with a
    floor {!Plane} below them and either a roof or the open {!Sky} above.

    A grid raycaster can only place walls on cell edges, so every wall faces
    north, south, east or west. Here a wall is an arbitrary line {e segment}
    instead, so a room may have any number of walls facing any direction — an
    octagon, a triangle, a wedge. Each wall also carries its own height, the
    {!Material} it is made of (some see-through) and any {!decal}s hung on it;
    the floor is an inclined plane so it need not be horizontal; the ceiling is
    either an inclined plane of its own or the open sky; and {!type-sprite}s stand —
    or float — in the world as billboards facing the player.

    A room knows nothing of any other room, or of the {!World} it is in. Every
    coordinate here is its own, and the only thing that ever relates two rooms
    is a {!World} link between a {!type-threshold} of each. That is what lets
    rooms be authored — or generated — one at a time, in any order. *)

type surface = { plane : Plane.t; material : Material.t }
(** A floor or a ceiling: where it is, and what it is made of. *)

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

(** Where along a decal's width a point [along] the wall falls, as a column of
    its image — or [None] where the decal does not reach.

    This and {!decal_row} are the whole of "is this point on that decal". They
    are split in two because the renderer needs them split: the horizontal
    answer is constant down a screen column and is worked out once, the vertical
    one changes every pixel. {!Sight} asks both at once. Between them they are
    the only statement of the rule, so what can be picked stays exactly what is
    drawn. *)
let decal_column d ~along =
  let width = 2. *. d.half_width in
  let off = along -. (d.along -. d.half_width) in
  if off < 0. || off > width then None
  else
    let n = d.image.Image.width in
    Some (Int.max 0 (Int.min (n - 1) (int_of_float (off /. width *. float_of_int n))))

(** Where down a decal's height a point [above] the wall's foot falls, as a row
    of its image. A decal hangs a height above the {e floor} under the wall and
    not at an absolute elevation, so on a sloped floor it rides with the wall
    instead of tilting across it. *)
let decal_row d ~above =
  let height = 2. *. d.half_height in
  let off = d.z +. d.half_height -. above in
  if off < 0. || off > height then None
  else
    let n = d.image.Image.height in
    Some (Int.max 0 (Int.min (n - 1) (int_of_float (off /. height *. float_of_int n))))

type wall = {
  a : Vec.t;  (** one endpoint *)
  b : Vec.t;  (** the other *)
  height : float;  (** how far the wall rises above the floor, in cells *)
  material : Material.t;  (** what it is made of, and whether you see through *)
  decals : decal list;  (** decorations over the material *)
  edge : Vec.t;  (** [b - a], precomputed for intersection tests *)
  length : float;  (** [|b - a|] *)
  normal : Vec.t;  (** unit vector perpendicular to the wall, for shading *)
}

type sprite = { pos : Vec.t; base : float; size : float; image : Image.t }
(** An object or character standing in the world, drawn as a billboard — a flat
    {!Image} that always faces the player. It stands at [pos] and is [size]
    cells tall.

    [base] is how far its foot floats above the floor under it: [0.] for
    something resting on the ground, and anything else for a mote of dust, a
    lamp, a bird. Like a {!type-decal}'s [z] it is measured from the floor and
    not from an absolute height, so on a sloped floor a sprite rides with the
    floor rather than staying put while the ground falls away beneath it.

    Its width is not [size]. A sprite is as wide as its picture says it is —
    see {!sprite_half_width} — so a wide, short mote is drawn wide and short. *)

(** A sprite at [pos], [size] cells tall, made of [image]; [base] cells above
    the floor if you say so, and standing on it if you do not.

    A constructor rather than a bare record so that a floating sprite is the
    only kind anyone has to write down, exactly as {!val-wall} lets a wall
    without decals stay silent about them. *)
let sprite ?(base = 0.) ~size ~image pos = { pos; base; size; image }

(** Half a sprite's width, in cells.

    A billboard is as tall as its [size] and as wide as its picture's shape
    makes it: a 2:1 image is drawn twice as wide as it is tall. Taking the
    aspect from the picture rather than from a field of its own means the art
    cannot be stretched by an authoring mistake — there is nothing to disagree
    with — and a square picture comes out square, which is what every sprite was
    before this existed.

    This is the only place the aspect ratio appears. {!Viewport.sprite_box}
    scales the screen box by it and {!sprite_column} reads across it, so the
    rectangle a sprite is drawn in and the rectangle it is picked in are the
    same rectangle. *)
let sprite_half_width s =
  s.size
  *. float_of_int s.image.Image.width
  /. float_of_int s.image.Image.height
  /. 2.

(** The elevation of a sprite's foot, given the height of the floor under it. *)
let sprite_foot s ~floor_z = floor_z +. s.base

(** The elevation of a sprite's top. *)
let sprite_head s ~floor_z = sprite_foot s ~floor_z +. s.size

(** Where across a sprite's width a point [lateral] cells to one side of its
    centre falls, as a column of its image — or [None] where the sprite does not
    reach. [lateral] is measured along the viewer's [right], since a billboard
    faces the viewer and has no side of its own.

    This and {!sprite_row} are to a sprite what {!decal_column} and
    {!decal_row} are to a decal: between them the only statement of "is this
    point on that picture", so what can be picked stays exactly what is drawn.
    {!Sight} asks both at once; {!Renderer} inverts them once per sprite into a
    screen rectangle and interpolates across it, which is the same rule read
    from the other end. *)
let sprite_column s ~lateral =
  let half = sprite_half_width s in
  if Float.abs lateral > half then None
  else
    let n = s.image.Image.width in
    Some
      (Int.max 0
         (Int.min (n - 1)
            (int_of_float ((lateral +. half) /. (2. *. half) *. float_of_int n))))

(** Where down a sprite's height an elevation [z] falls, as a row of its image,
    over a floor at [floor_z] — or [None] above its head or below its foot. *)
let sprite_row s ~floor_z ~z =
  let head = sprite_head s ~floor_z in
  let off = head -. z in
  if off < 0. || off > s.size then None
  else
    let n = s.image.Image.height in
    Some
      (Int.max 0
         (Int.min (n - 1) (int_of_float (off /. s.size *. float_of_int n))))

type lintel = { top : float; material : Material.t }
(** The wall a doorway is cut into, so the renderer can fill the strip left
    above the opening: [top] is how far that wall rises above the floor and
    [material] is what it is made of. Without one the opening runs the full
    height of whatever it is cut into and there is nothing above it to draw. *)

type threshold = {
  name : string;  (** what a {!World} link refers to it by *)
  a : Vec.t;  (** one endpoint *)
  b : Vec.t;  (** the other *)
  height : float;  (** how tall the opening is above the floor *)
  door : Door.t option;
      (** the leaf hung across it, if any; [None] is a bare opening *)
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
    player walks through. A closed leaf takes its place: the neighbour is not
    drawn, and the step is refused. A door standing [Open] is neither drawn nor
    felt, so it behaves exactly as an opening with no door in it. *)

(** What is drawn across this opening, if anything — {!Door.leaf} of whatever
    hangs in it, and nothing at all where nothing does. *)
let leaf (t : threshold) = Option.bind t.door Door.leaf

(** Does this opening stop a step? Exactly when there is a leaf across it. *)
let shut (t : threshold) = Option.is_some (leaf t)

type ceiling =
  | Roof of surface  (** an inclined plane overhead, of some material *)
  | Open of Sky.t  (** nothing overhead, and which {!Sky} shows instead *)

type t = {
  walls : wall array;
  thresholds : threshold array;
  floor : surface;
  ceiling : ceiling;
  sprites : sprite array;
}

(** Build a wall between two points, precomputing the quantities the renderer
    and the ray caster would otherwise recompute every frame. *)
let wall ~height ~material ?(decals = []) a b =
  let edge = Vec.sub b a in
  {
    a;
    b;
    height;
    material;
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

(** The same room with other sprites in it.

    This is how something animates. A room is immutable, so a room that changes
    is a room that is built again — but a mote of dust drifting across it has
    not moved a wall, and rebuilding the walls to move the mote costs a
    {!val-wall} per wall per frame, each one normalizing a vector to arrive back
    at the number it already had. Here the walls, the thresholds and both planes
    are the ones that were already there; only the sprite array is new.

    {b The pictures must already exist.} Selecting among images made once at
    load is the whole of animating a sprite: hand back the same room with a
    different {!type-sprite} in it and give that to {!World.replace_room}. There
    is no frame counter and no timing here, because which frame it is at this
    moment is the game's to decide and there is nothing the engine could add to
    it — and because generating a picture inside a frame is the one thing this
    is meant to make unnecessary. *)
let with_sprites t sprites = { t with sprites = Array.of_list sprites }

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
let path ?(closed = false) ~height ~material points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  let last = if closed then n - 1 else n - 2 in
  List.init
    (Int.max 0 (last + 1))
    (fun i -> wall ~height ~material arr.(i) arr.((i + 1) mod n))

(** Cut a doorway into the wall that would otherwise run from [a] to [b]: the
    two jambs left either side of a gap [width] wide in the middle, and the
    {!type-threshold} filling that gap, [opening] tall.

    The threshold comes out wound the same way as the wall it replaces, which is
    the winding rule {!Transform.between} depends on, and it takes the wall's
    own height and material as its {!type-lintel}, so the strip left above the
    opening is still drawn. Cutting both sides of a doorway this way is what
    keeps a room's boundary and its thresholds honest about each other. *)
let doorway ~name ?door ~width ~opening ~height ~material a b =
  let edge = Vec.sub b a in
  let half = Vec.scale edge (width /. (2. *. Vec.length edge)) in
  let middle = Vec.scale (Vec.add a b) 0.5 in
  let p = Vec.sub middle half and q = Vec.add middle half in
  ( [ wall ~height ~material a p; wall ~height ~material q b ],
    threshold ~name ?door ~height:opening ~lintel:{ top = height; material } p q
  )

(** A regular polygon of [sides] walls, [radius] from [center], turned by
    [rotation]. A cheap way to draw rooms and pillars whose walls face every
    direction. *)
let regular_polygon ~center ~radius ~sides ~rotation ~height ~material =
  path ~closed:true ~height ~material
    (List.init sides (fun k ->
         let angle =
           rotation +. (float_of_int k *. 2. *. Float.pi /. float_of_int sides)
         in
         Vec.add center (Vec.make (radius *. cos angle) (radius *. sin angle))))
