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

let floor_plane t = t.floor.plane
let floor_material t = t.floor.material

let ceiling_surface t =
  match t.ceiling with Roof s -> Some s | Open _ -> None

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

let make ?(thresholds = []) ?(sprites = []) ~floor ~ceiling walls =
  {
    walls = Array.of_list walls;
    thresholds = Array.of_list thresholds;
    floor;
    ceiling;
    sprites = Array.of_list sprites;
  }

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
  let last = if closed then n - 1 else n - 2 in
  List.init
    (Int.max 0 (last + 1))
    (fun i -> wall ~height ~material arr.(i) arr.((i + 1) mod n))

let doorway ~name ?door ~width ~opening ~height ~material a b =
  let edge = Vec.sub b a in
  let span = Vec.length edge in
  if not (span > 0.) then
    invalid_arg ("Room.doorway: no wall to cut a doorway into: " ^ name);
  if not (width > 0.) then
    invalid_arg ("Room.doorway: a doorway has to have a width: " ^ name);
  if not (width <= span) then
    invalid_arg ("Room.doorway: wider than the wall it is cut into: " ^ name);
  let half = Vec.scale edge (width /. (2. *. span)) in
  let middle = Vec.scale (Vec.add a b) 0.5 in
  let p = Vec.sub middle half and q = Vec.add middle half in
  ( [ wall ~height ~material a p; wall ~height ~material q b ],
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
  (* Counterclockwise with y down, so every wall's normal faces inward,
     whichever two opposite corners were given. *)
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
  path ~closed:true ~height ~material
    (List.init sides (fun k ->
         let angle =
           rotation +. (float_of_int k *. 2. *. Float.pi /. float_of_int sides)
         in
         Vec.add center (Vec.make (radius *. cos angle) (radius *. sin angle))))
