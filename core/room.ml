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
  (* Negated, so a nan is refused with the zeroes and the negatives, and finite
     with it, so an infinity goes the same way — the terms {!wall} and
     {!threshold} are already held to.

     All four numbers, and not only the two that divide. A decal is read by
     subtracting where it is from the point being asked about and seeing whether
     what is left falls inside its extent, and the two readers below are written
     to answer {e unless} it falls outside. A nan falls outside nothing: it is
     less than no bound and greater than none, so the test that should have
     rejected the point passes it, [int_of_float] takes the nan that follows to
     zero, and the decal answers for every point on its wall at texel column
     zero. That is not an invisible decal or a misplaced one. It is a smear
     across the whole wall, which the renderer draws and {!Sight} picks in front
     of whatever is really there.

     An infinite half-width arrives at the same place by a different road: the
     extent swallows the wall, and the offset divided by it is a nan again. An
     infinite [along] is the one unreal number the readers do refuse on their
     own, the offset coming out infinite rather than nan and failing the bound
     honestly — which is not a reason to let it in. *)
  if not (Float.is_finite along) then
    invalid_arg "Room.decal: a decal has to be somewhere along its wall";
  if not (Float.is_finite z) then
    invalid_arg "Room.decal: a decal has to be at some height";
  if not (Float.is_finite half_width && half_width > 0.) then
    invalid_arg "Room.decal: a decal has to have a width";
  if not (Float.is_finite half_height && half_height > 0.) then
    invalid_arg "Room.decal: a decal has to have a height";
  if not (glow >= 0. && glow <= 1.) then
    invalid_arg "Room.decal: glow is a fraction from 0 to 1";
  { along; z; half_width; half_height; image; facing; glow }

let decal_light d ~light = light +. (d.glow *. (1. -. light))

(* The bound tests below are negated for the reason the constructor's are, and
   they are the second half of the same guard rather than a repeat of it: the
   fields are finite by the time one is built, but the point being asked about
   arrives from the renderer and from {!Sight}, which work it out from a ray.
   Written the other way round — refuse when outside — a nan point is outside
   nothing and is answered for. Written this way it has to be shown inside, and
   a nan never is. *)
let decal_column d ~seen_from ~along =
  if d.facing <> seen_from then None
  else
    let width = 2. *. d.half_width in
    let off = along -. (d.along -. d.half_width) in
    if not (off >= 0. && off <= width) then None
    else
      let n = d.image.Image.width in
      let u =
        Int.max 0
          (Int.min (n - 1) (int_of_float (off /. width *. float_of_int n)))
      in
      (* [along] runs from the wall's [a] to its [b], and which way round that
         is on screen is the whole of what the winding rule above decides: the
         normal is [perp edge], so standing on the Front the walk from [a] to
         [b] goes left to right, and standing on the Back it goes right to left.
         The offset alone therefore names a column of the picture only from one
         side, and from the other it names its mirror.

         So the far face reads back to front. The extent is untouched — the
         decal covers the same stretch of wall from either side, because that is
         where the paint is — and only the picture within it is turned round,
         which is what makes [along] a place on the wall rather than a place in
         the image. Written here rather than in the renderer because {!Sight}
         reads this too, and a mark drawn mirrored and picked unmirrored would
         be a mark whose left half answered for its right. *)
      Some (match seen_from with Front -> u | Back -> n - 1 - u)

let decal_row d ~above =
  let height = 2. *. d.half_height in
  let off = d.z +. d.half_height -. above in
  if not (off >= 0. && off <= height) then None
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

type sprite = {
  pos : Vec.t;
  base : float;
  size : float;
  image : Image.t;
  glow : float;
}

let sprite ?(base = 0.) ?(glow = 0.) ~size ~image pos =
  (* Negated and finite, on the same terms as {!decal} above and for the same
     reasons. size divides in sprite_row and in Viewport.sprite_box, and a
     sprite of no height would be a billboard of no width as well; base is
     added to the floor to find the foot, so an unreal one makes every row of
     the picture answer for every height. The position is held to it too — it is
     what every distance to this sprite is measured from. *)
  if not (Float.is_finite pos.Vec.x && Float.is_finite pos.Vec.y) then
    invalid_arg "Room.sprite: a sprite has to stand somewhere";
  if not (Float.is_finite base) then
    invalid_arg "Room.sprite: a sprite has to stand at some height";
  if not (Float.is_finite size && size > 0.) then
    invalid_arg "Room.sprite: a sprite has to have a size";
  if not (glow >= 0. && glow <= 1.) then
    invalid_arg "Room.sprite: glow is a fraction from 0 to 1";
  { pos; base; size; image; glow }

let sprite_light s ~light = light +. (s.glow *. (1. -. light))

let sprite_half_width s =
  s.size
  *. float_of_int s.image.Image.width
  /. float_of_int s.image.Image.height
  /. 2.

let sprite_foot s ~floor_z = floor_z +. s.base
let sprite_head s ~floor_z = sprite_foot s ~floor_z +. s.size

let sprite_column s ~lateral =
  let half = sprite_half_width s in
  if not (Float.abs lateral <= half) then None
  else
    let n = s.image.Image.width in
    Some
      (Int.max 0
         (Int.min (n - 1)
            (int_of_float ((lateral +. half) /. (2. *. half) *. float_of_int n))))

let sprite_row s ~floor_z ~z =
  let head = sprite_head s ~floor_z in
  let off = head -. z in
  if not (off >= 0. && off <= s.size) then None
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

(* A lintel is the strip of wall above an opening, so one that does not reach the
   opening it stands over describes a wall that cannot be drawn. Written here
   rather than inside [threshold] because [with_lintel] hangs one on a threshold
   that has already been built and has to hold it to the same terms: a private
   type earns nothing from a check every route to it does not make. [who] is
   whichever of the two was called, so the message names it. *)
let check_lintel ~who ~name ~height lintel =
  Option.iter
    (fun (l : lintel) ->
      if not (Float.is_finite l.top && l.top >= height) then
        invalid_arg (who ^ ": the lintel has to sit above the opening: " ^ name))
    lintel

let leaf (t : threshold) = Option.bind t.door Door.leaf
let shut (t : threshold) = Option.is_some (leaf t)
let with_door (t : threshold) door = { t with door }

let with_lintel (t : threshold) lintel =
  check_lintel ~who:"Room.with_lintel" ~name:t.name ~height:t.height lintel;
  { t with lintel }

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
  (* The geometry is safe by construction — every derived field is copied from a
     threshold that already passed the same test [wall] would apply — but the
     height arrives from outside, so it is held to [wall]'s terms here. Without
     it this is the one route to a [wall] the checks below do not guard. *)
  if not (Float.is_finite height && height > 0.) then
    invalid_arg "Room.threshold_wall: the wall has to rise above the floor";
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
   being false as well.

   Vec.normalizable rather than is_finite && > 0., because those two are not the
   whole of what normalising needs: a length below about 5.6e-309 is both, and
   its reciprocal is still infinity. The normal then comes back (infinity, nan)
   — not a unit vector, not perpendicular, and read as it stands by the three
   things named above. side_of answers Back for every point, the dot product
   being a nan that fails the comparison. *)
let wall ~height ~material ?(decals = []) a b =
  let edge = Vec.sub b a in
  let length = Vec.length edge in
  if not (Vec.normalizable length) then
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
   sliver or as nothing. The lintel it may carry is held to check_lintel's
   terms, which with_lintel holds it to as well. *)
let threshold ~name ~height ?door ?lintel a b =
  let edge = Vec.sub b a in
  let length = Vec.length edge in
  if not (Vec.normalizable length) then
    invalid_arg ("Room.threshold: the two ends have to be apart: " ^ name);
  if not (Float.is_finite height && height > 0.) then
    invalid_arg
      ("Room.threshold: the opening has to rise above the floor: " ^ name);
  check_lintel ~who:"Room.threshold" ~name ~height lintel;
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

(* [parallel] is a sine, which is what is left of the cross product below once
   both lengths are divided out of it. Tested against a fixed figure instead, it
   would read as a parallel test and behave as a length one: the product is an
   area, so any pair short enough would fail it at whatever angle the two met
   at. The pair that gets short here is a step and a doorway — {!Player.slide}
   clips a leg where it crosses one and asks again about the remainder, which
   can be a whisker — and a remainder that came out parallel is a doorway
   {!World.crossing} does not report, a room the player never enters, and a walk
   on out through the wall it was cut into. The same reasoning and the same
   figure as {!Ray.segment}, which is the half of this arithmetic that was
   fixed first.

   Inclusive where Ray's is strict, so that a [b1..b2] of no length keeps the
   branch it has always taken: its [denom] is zero and so is its scaled
   tolerance, and a strict test would send it to the crossing branch to divide
   by that zero and come back false through a nan.

   [collinear] is not an area and is not scaled by both. Divided by [length],
   the cross product it is tested against is the offset of [b1] from the line of
   [a1..a2] — a distance in world units, which compares to one. *)
let parallel = 1e-12
let collinear = 1e-9

let segments_cross ~a1 ~a2 ~b1 ~b2 =
  let d1 = Vec.sub a2 a1 and d2 = Vec.sub b2 b1 in
  let denom = Vec.cross d1 d2 in
  let off = Vec.sub b1 a1 in
  let length = Vec.length d1 in
  if Float.abs denom <= parallel *. length *. Vec.length d2 then
    (* Parallel, so only an overlap of collinear segments is left to find: the
       cross product below is the offset of [b1] from the line of [a1..a2],
       times that line's length. *)
    if length = 0. || Float.abs (Vec.cross off d1) > collinear *. length then
      false
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

(* Which side of the line through [a..b] the point is on, as a sign. Unscaled,
   because only the sign is read. *)
let side ~a ~b p = Vec.cross (Vec.sub b a) (Vec.sub p a)

(* May a step be taken with this segment — a wall, or a doorway that stops
   one — as near as it is?

   The ordinary rule is the swept disc, and the first line is the whole of it:
   the player is a circle of {!Config.collision_padding}, so a step that brings
   that circle against the segment anywhere along its length is refused. Testing
   the sweep rather than the destination is what stops a long step tunnelling
   through a thin wall.

   The rest is the way out of a state that rule on its own has no exit from. A
   player can be inside the padding without having walked there — {!World.set_door}
   shuts a leaf and deliberately moves nobody, {!World.replace_room} can grow a
   wall beside them, and a description that reshapes a room around a pose it
   keeps does the same. Once there every step is refused, whichever way it
   points, because the swept segment starts where the player is standing: the
   step {e away} fails the same test as the step into it. The player is held
   there until the game undoes what it did, and a game closing a door behind
   someone who has just walked through it has no reason to think it did
   anything.

   So a step that does not decrease the separation is allowed. It cannot make
   the state worse, and it is the only thing that ends it — one step out and the
   ordinary rule has the player again.

   Passing through is still refused, and that test reads the {e sign} of the
   offset rather than whether the two segments touch. Touching is the case that
   matters here: a crossing leaves the player on the threshold line itself, an
   offset of exactly zero, and a test that refused a step from there would leave
   the commonest way into this state as the one way it could not be left. Zero
   is on neither side, so it passes, and only a step that starts one side and
   ends the other through the segment is turned back. *)
let clears_segment ~from ~dest ~a ~b =
  distance_between_segments ~a1:from ~a2:dest ~b1:a ~b2:b
  >= Config.collision_padding
  ||
  let d0 = distance_to_segment from ~a ~b
  and d1 = distance_to_segment dest ~a ~b in
  d0 < Config.collision_padding
  && d1 >= d0
  && not
       (side ~a ~b from *. side ~a ~b dest < 0.
       && segments_cross ~a1:from ~a2:dest ~b1:a ~b2:b)

let passable t ~from ~dest =
  not
    (Array.exists
       (fun (w : wall) -> not (clears_segment ~from ~dest ~a:w.a ~b:w.b))
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
    if not (Vec.normalizable step) then
      invalid_arg "Room.path: two points in a row are the same"
  done;
  List.init
    (Int.max 0 (last + 1))
    (fun i -> wall ~height ~material arr.(i) arr.((i + 1) mod n))

let doorway ~name ?door ~width ~opening ~height ~material a b =
  let edge = Vec.sub b a in
  let span = Vec.length edge in
  if not (Vec.normalizable span) then
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
  (* The cut points are measured {e in from the ends} rather than out from the
     middle, and the difference is only ever in the last bits — which is the
     whole of it. [inset] is the fraction of the wall each jamb takes, so at
     [width = span] it is a plain zero, scaling the edge to nothing and leaving
     [p] and [q] as the very floats [a] and [b] arrived as.

     Out from the middle they do not come back. [(a + b) / 2 - edge * (width /
     2 span)] is the same number on paper and a different one in binary, and at
     [width = span] — a whole side that is one opening, allowed above and
     documented to leave no jamb — the two disagreed by a few times [1e-17].
     That survives the test below, so the ends were dropped only when the
     coordinates happened to cancel exactly: [(0,0)-(4,0)] left nothing, and
     [(0.1,0.2)-(0.7,1.3)] left a wall [6.2e-17] long.

     Such a wall is the invisible blocker {!wall} exists to refuse. It is too
     short for a ray to hit, so nothing draws it and nothing shows it is there,
     and {!blocked} measures to the nearest point of a segment — so it is a
     {!Config.collision_padding} disc of solid nothing at the corner of an
     opening the player is meant to walk through. It also takes a wall index,
     and those are what {!Sight} reports and what {!add_decal} counts. *)
  let inset = Vec.scale edge ((span -. width) /. (2. *. span)) in
  let p = Vec.add a inset and q = Vec.sub b inset in
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
