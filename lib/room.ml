(* Implementation of {!Camlcast.Room}; the interface carries the prose. *)

type surface = { plane : Plane.t; material : Material.t }

let floor ~plane ~material = { plane; material }

type side = Front | Back

type decal = {
  along : float;
  z : float;
  half_width : float;
  half_height : float;
  image : Image.t;
  facing : side;
  glow : float;
}

let decal ?(facing = Front) ?(glow = 0.) ~along ~z ~half_width ~half_height
    image =
  (* Negated, so a nan is refused with the zeroes and the negatives. Both
     halves are divisors in decal_column and decal_row. *)
  if not (half_width > 0.) then
    invalid_arg "Room.decal: a decal has to have a width";
  if not (half_height > 0.) then
    invalid_arg "Room.decal: a decal has to have a height";
  if not (glow >= 0. && glow <= 1.) then
    invalid_arg "Room.decal: glow is a fraction from 0 to 1";
  { along; z; half_width; half_height; image; facing; glow }

let decal_light d ~light = light +. (d.glow *. (1. -. light))

let decal_column d ~seen_from ~along =
  if d.facing <> seen_from then None
  else
    let width = 2. *. d.half_width in
    let off = along -. (d.along -. d.half_width) in
    if off < 0. || off > width then None
    else
      let n = d.image.Image.width in
      Some
        (Int.max 0
           (Int.min (n - 1) (int_of_float (off /. width *. float_of_int n))))

let decal_row d ~above =
  let height = 2. *. d.half_height in
  let off = d.z +. d.half_height -. above in
  if off < 0. || off > height then None
  else
    let n = d.image.Image.height in
    Some
      (Int.max 0
         (Int.min (n - 1) (int_of_float (off /. height *. float_of_int n))))

type wall = {
  a : Vec.t;
  b : Vec.t;
  height : float;
  material : Material.t;
  decals : decal list;
  edge : Vec.t;
  length : float;
  normal : Vec.t;
}

let side_of (w : wall) point =
  if Vec.dot (Vec.sub point w.a) w.normal >= 0. then Front else Back

type sprite = { pos : Vec.t; base : float; size : float; image : Image.t }

let sprite ?(base = 0.) ~size ~image pos =
  (* Negated, so a nan is refused with it. size divides in sprite_row and in
     Viewport.sprite_box, and a sprite of no height would be a billboard of no
     width as well. *)
  if not (size > 0.) then invalid_arg "Room.sprite: a sprite has to have a size";
  { pos; base; size; image }

let sprite_half_width s =
  s.size
  *. float_of_int s.image.Image.width
  /. float_of_int s.image.Image.height
  /. 2.

let sprite_foot s ~floor_z = floor_z +. s.base
let sprite_head s ~floor_z = sprite_foot s ~floor_z +. s.size

let sprite_column s ~lateral =
  let half = sprite_half_width s in
  if Float.abs lateral > half then None
  else
    let n = s.image.Image.width in
    Some
      (Int.max 0
         (Int.min (n - 1)
            (int_of_float ((lateral +. half) /. (2. *. half) *. float_of_int n))))

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

type threshold = {
  name : string;
  a : Vec.t;
  b : Vec.t;
  height : float;
  door : Door.t option;
  lintel : lintel option;
  edge : Vec.t;
  length : float;
  normal : Vec.t;
}

let leaf (t : threshold) = Option.bind t.door Door.leaf
let shut (t : threshold) = Option.is_some (leaf t)
let with_door (t : threshold) door = { t with door }
let with_lintel (t : threshold) lintel = { t with lintel }

let across (a : threshold) (b : threshold) =
  Transform.between ~a1:a.a ~a2:a.b ~b1:b.a ~b2:b.b

type ceiling = Roof of surface | Open of Sky.t

let roof ~plane ~material = Roof { plane; material }
let open_sky sky = Open sky

type t = {
  walls : wall array;
  thresholds : threshold array;
  floor : surface;
  ceiling : ceiling;
  sprites : sprite array;
}

let floor_surface t = t.floor
let floor_plane t = t.floor.plane
let floor_material t = t.floor.material
let ceiling t = t.ceiling
let ceiling_surface t = match t.ceiling with Roof s -> Some s | Open _ -> None
let ceiling_plane t = Option.map (fun s -> s.plane) (ceiling_surface t)
let sky t = match t.ceiling with Open s -> Some s | Roof _ -> None

let threshold_wall (t : threshold) ~height ~material : wall =
  {
    a = t.a;
    b = t.b;
    height;
    material;
    decals = [];
    edge = t.edge;
    length = t.length;
    normal = t.normal;
  }

(* The length is enough to check on its own, the way Transform.between explains:
   Vec.length folds every bad coordinate into it, so a nan end gives a nan
   length and an infinite one a length whose reciprocal is 0. Negated, so both
   are refused with the coincident ends. What the check buys is the normal —
   Vec.normalize hands a zero vector back unchanged, so a wall of no length
   would carry one that is neither a unit vector nor perpendicular to anything,
   and Atmosphere.face_shading, side_of and every decal placed along it read
   exactly that. Nothing downstream refuses it a second time: Ray.segment finds
   no intersection with a zero edge and distance_to_segment degrades to a point,
   so it would stand there as an invisible collision blocker.

   The height buys the same thing at the other end of the wall.
   Renderer.draw_wall works out a top of floor_z +. height and draws nothing at
   all unless it clears the floor, while blocked and passable never read the
   height in the first place — so a wall that does not rise is the same
   invisible blocker by another route, and a nan one is too, that comparison
   being false as well. *)
let wall ~height ~material ?(decals = []) a b =
  let edge = Vec.sub b a in
  let length = Vec.length edge in
  if not (Float.is_finite length && length > 0.) then
    invalid_arg "Room.wall: the two ends have to be apart";
  if not (Float.is_finite height && height > 0.) then
    invalid_arg "Room.wall: the wall has to rise above the floor";
  {
    a;
    b;
    height;
    material;
    decals;
    edge;
    length;
    normal = Vec.normalize (Vec.perp edge);
  }

(* The same refusal on the same terms, and the stakes are higher: a threshold's
   normal is what Transform.between turns into a portal's frame change, and its
   length is the first thing World.pair measures. The height carries one stake
   of its own: World.passable is flat and never reads it, so an opening that
   does not rise is walked through all the same while the renderer draws it as a
   sliver or as nothing. And a lintel is by definition the strip above the
   opening — one hanging below it describes a wall that cannot be drawn. *)
let threshold ~name ~height ?door ?lintel a b =
  let edge = Vec.sub b a in
  let length = Vec.length edge in
  if not (Float.is_finite length && length > 0.) then
    invalid_arg ("Room.threshold: the two ends have to be apart: " ^ name);
  if not (Float.is_finite height && height > 0.) then
    invalid_arg
      ("Room.threshold: the opening has to rise above the floor: " ^ name);
  Option.iter
    (fun (l : lintel) ->
      if not (Float.is_finite l.top && l.top >= height) then
        invalid_arg
          ("Room.threshold: the lintel has to sit above the opening: " ^ name))
    lintel;
  {
    name;
    a;
    b;
    height;
    door;
    lintel;
    edge;
    length;
    normal = Vec.normalize (Vec.perp edge);
  }

let make ?(thresholds = []) ?(sprites = []) ~floor ~ceiling walls =
  {
    walls = Array.of_list walls;
    thresholds = Array.of_list thresholds;
    floor;
    ceiling;
    sprites = Array.of_list sprites;
  }

let wall_count t = Array.length t.walls
let wall_at t index = t.walls.(index)
let threshold_count t = Array.length t.thresholds
let threshold_at t index = t.thresholds.(index)
let sprite_count t = Array.length t.sprites
let sprite_at t index = t.sprites.(index)
let with_sprites t sprites = { t with sprites = Array.of_list sprites }
let with_thresholds t thresholds = { t with thresholds = Array.copy thresholds }

let add_decal t ~wall decal =
  let walls = Array.copy t.walls in
  let w = walls.(wall) in
  walls.(wall) <- { w with decals = w.decals @ [ decal ] };
  { t with walls }

(* Shortest distance from a point to the segment a..b: project the point onto
   the line, clamp to the segment's ends, and measure to that nearest point.
   Not in the interface — the two measurements built on it are what anything
   outside wants, and neither of them is this one under another name. *)
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

let blocked t (p : Vec.t) =
  Array.exists
    (fun w -> distance_to_wall w p < Config.collision_padding)
    t.walls

let segments_cross ~a1 ~a2 ~b1 ~b2 =
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

let distance_between_segments ~a1 ~a2 ~b1 ~b2 =
  if segments_cross ~a1 ~a2 ~b1 ~b2 then 0.
  else
    let to_b p = distance_to_segment p ~a:b1 ~b:b2
    and to_a p = distance_to_segment p ~a:a1 ~b:a2 in
    Float.min (Float.min (to_b a1) (to_b a2)) (Float.min (to_a b1) (to_a b2))

let passable t ~from ~dest =
  not
    (Array.exists
       (fun (w : wall) ->
         distance_between_segments ~a1:from ~a2:dest ~b1:w.a ~b2:w.b
         < Config.collision_padding)
       t.walls)

let path ?(closed = false) ~height ~material points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  if closed && n < 3 then
    invalid_arg "Room.path: a closed path has to have at least three points";
  if not (Float.is_finite height && height > 0.) then
    invalid_arg "Room.path: the walls have to rise above the floor";
  let last = if closed then n - 1 else n - 2 in
  (* Refused here rather than left to {!wall}, so that the habit a closed path
     invites — repeating the first point at the end to shut the loop — is
     refused under the name the caller wrote and not under one they never
     called. Negated, so a nan point is refused with the repeated ones. *)
  for i = 0 to last do
    let step = Vec.length (Vec.sub arr.((i + 1) mod n) arr.(i)) in
    if not (Float.is_finite step && step > 0.) then
      invalid_arg "Room.path: two points in a row are the same"
  done;
  List.init
    (Int.max 0 (last + 1))
    (fun i -> wall ~height ~material arr.(i) arr.((i + 1) mod n))

let doorway ~name ?door ~width ~opening ~height ~material a b =
  let edge = Vec.sub b a in
  let span = Vec.length edge in
  if not (Float.is_finite span && span > 0.) then
    invalid_arg ("Room.doorway: no wall to cut a doorway into: " ^ name);
  if not (Float.is_finite width && width > 0.) then
    invalid_arg ("Room.doorway: a doorway has to have a width: " ^ name);
  if not (width <= span) then
    invalid_arg ("Room.doorway: wider than the wall it is cut into: " ^ name);
  if not (Float.is_finite height && height > 0.) then
    invalid_arg ("Room.doorway: the wall has to rise above the floor: " ^ name);
  (* The opening becomes the threshold's height and the wall's becomes the
     lintel over it, so an opening taller than its wall is a lintel hanging
     below its own doorway. Refused here, in the words the caller wrote, rather
     than left to {!threshold} to refuse in words about a lintel they never
     mentioned. Negated, so a nan opening is refused with the too-tall ones;
     [height] is finite by the line above. *)
  if not (opening > 0. && opening <= height) then
    invalid_arg ("Room.doorway: the opening has to fit under the wall: " ^ name);
  let half = Vec.scale edge (width /. (2. *. span)) in
  let middle = Vec.scale (Vec.add a b) 0.5 in
  let p = Vec.sub middle half and q = Vec.add middle half in
  (* A doorway exactly as wide as its wall is allowed above, and leaves no jamb
     at either end. Those ends are dropped rather than built, because a wall of
     no length is the one thing {!wall} refuses. *)
  let jamb (x, y) =
    if Vec.length (Vec.sub y x) > 0. then Some (wall ~height ~material x y)
    else None
  in
  ( List.filter_map jamb [ (a, p); (q, b) ],
    threshold ~name ?door ~height:opening ~lintel:{ top = height; material } p q
  )

let rectangle ~height ~material c1 c2 =
  let x0 = Float.min c1.Vec.x c2.Vec.x
  and x1 = Float.max c1.Vec.x c2.Vec.x
  and y0 = Float.min c1.Vec.y c2.Vec.y
  and y1 = Float.max c1.Vec.y c2.Vec.y in
  (* Negated, so a nan corner is refused with the flat ones. *)
  if not (x0 < x1 && y0 < y1) then
    invalid_arg "Room.rectangle: the corners have to span an area";
  if not (Float.is_finite height && height > 0.) then
    invalid_arg "Room.rectangle: the walls have to rise above the floor";
  (* Wound per the Room winding rule so every wall's normal faces inward
     (on the screen map with y down, this loop appears clockwise). *)
  path ~closed:true ~height ~material
    [ Vec.make x0 y0; Vec.make x1 y0; Vec.make x1 y1; Vec.make x0 y1 ]

let nearest_threshold ?(within = infinity) ?(where = fun _ -> true) t
    (p : Vec.t) =
  let best = ref None in
  Array.iteri
    (fun i (th : threshold) ->
      if where th then
        let middle = Vec.scale (Vec.add th.a th.b) 0.5 in
        let away = Vec.length (Vec.sub middle p) in
        let nearer =
          match !best with None -> true | Some (d, _) -> away < d
        in
        if away <= within && nearer then best := Some (away, i))
    t.thresholds;
  Option.map snd !best

let regular_polygon ~center ~radius ~sides ~rotation ~height ~material =
  (* Refused here and not left to {!path}, which would name a function the
     caller never called and, for a negative count, not get that far: List.init
     raises under its own name first. Fewer than three sides is not a polygon —
     two are a pair of coincident walls wound against each other, one is a wall
     of no length, none is a room with no boundary at all. Negated, so a nan
     radius, rotation or center is refused with the flat ones. *)
  if not (sides >= 3) then
    invalid_arg
      "Room.regular_polygon: a polygon has to have at least three sides";
  if not (Float.is_finite radius && radius > 0.) then
    invalid_arg "Room.regular_polygon: a polygon has to have a radius";
  if not (Float.is_finite rotation) then
    invalid_arg "Room.regular_polygon: the rotation has to be a number";
  if not (Float.is_finite center.Vec.x && Float.is_finite center.Vec.y) then
    invalid_arg "Room.regular_polygon: the center has to be a point";
  if not (Float.is_finite height && height > 0.) then
    invalid_arg "Room.regular_polygon: the walls have to rise above the floor";
  path ~closed:true ~height ~material
    (List.init sides (fun k ->
         let angle =
           rotation +. (float_of_int k *. 2. *. Float.pi /. float_of_int sides)
         in
         Vec.add center (Vec.make (radius *. cos angle) (radius *. sin angle))))
