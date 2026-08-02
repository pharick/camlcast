(* Implementation of {!Camlcast.World}; the interface carries the prose. *)

type portal = {
  threshold : Room.threshold;  (** the doorway, in this room's own frame *)
  to_room : int;  (** the room on the other side *)
  twin : int;
      (** which of [to_room]'s own thresholds is this same doorway, described
          from that side *)
  onto : Transform.t;  (** this room's frame onto that one's *)
}

type location = { room : int; pos : Vec.t }

type t = {
  rooms : Room.t array;
  names : string array;  (** [names.(i)] is what [rooms.(i)] was authored as *)
  portals : portal option array array;
      (** [portals.(i)] runs parallel to [rooms.(i).thresholds], so a ray that
          reports a threshold index can look up its portal directly. [None] is a
          doorway that leads nowhere yet — see {!open_doorway}. *)
  atmosphere : Atmosphere.t;  (** the air every room of it is seen through *)
  spawn : location;
}

let room t index = t.rooms.(index)
let room_count t = Array.length t.rooms
let name t index = t.names.(index)
let named t name = Array.find_index (String.equal name) t.names
let doorway_count t ~room = Array.length t.portals.(room)
let portal t ~room ~threshold = t.portals.(room).(threshold)
let atmosphere t = t.atmosphere
let with_atmosphere t atmosphere = { t with atmosphere }
let spawn t = t.spawn

(* Tolerance for the checks below. Lengths and heights are either written by
    hand or built from the same constants, so they should agree exactly; this
    only absorbs the last bit or two of a decimal literal. *)
let epsilon = 1e-6

(* What the two sides of a link have to agree about: whether a leaf hangs
    there, and what it is doing. [None] for a bare opening.

    Not what it is made of — see {!pair}. *)
let door_state (t : Room.threshold) =
  Option.map (fun (d : Door.t) -> d.Door.state) t.Room.door

(* The four questions {!pair} asks of a link, each written as what {e passes} so
    that a caller says [not (…)] to refuse. That shape is the nan handling and
    not a style: nan answers false to every ordered comparison, so a threshold of
    length nan fails [has_length] and fails [lengths_agree] with its twin, and is
    refused by both. Asserting the failure instead — [length <= epsilon] — would
    let it through each of them. The long note on {!pair} is about this.

    Public because {!Check} reads a description for the same mistakes before the
    world is built, and a checker that models the engine with its own copy of
    these numbers is a checker that disagrees with it as soon as either moves.
    It did: it refused a disagreement of 1e-9 that the engine accepts to 1e-6,
    and it compared whether a door was there rather than what it was doing. One
    implementation, asked twice. *)
let has_length (t : Room.threshold) = t.Room.length > epsilon

let lengths_agree (a : Room.threshold) (b : Room.threshold) =
  Float.abs (a.Room.length -. b.Room.length) <= epsilon

let heights_agree (a : Room.threshold) (b : Room.threshold) =
  Float.abs (a.Room.height -. b.Room.height) <= epsilon

let doors_agree a b = door_state a = door_state b

(* Refuse two thresholds of one room sharing a name: no link could tell them
    apart, and {!link} resolves by name. *)
let check_names ~who ~room name (r : Room.t) =
  let seen = Hashtbl.create (Room.threshold_count r) in
  for i = 0 to Room.threshold_count r - 1 do
    let (t : Room.threshold) = Room.threshold_at r i in
    if Hashtbl.mem seen t.Room.name then
      invalid_arg (who ^ ": two thresholds named " ^ name ^ "." ^ t.Room.name);
    Hashtbl.add seen t.Room.name ()
  done;
  ignore room

(* Refuse two rooms of one world sharing a name, for the same reason as the
    thresholds above and with a sharper edge: a duplicate name does not collide,
    it {e shadows}. Rooms are resolved with [Array.find_index], which answers
    with the first, so a link — or a spawn — written for the second of two rooms
    named [hall] is silently made against the first. The world is built, it
    renders, it walks, and it is not the one that was written down. *)
let check_room_names ~who names =
  let seen = Hashtbl.create (Array.length names) in
  Array.iter
    (fun name ->
      if Hashtbl.mem seen name then
        invalid_arg (who ^ ": two rooms named " ^ name);
      Hashtbl.add seen name ())
    names

(* The two {!portal}s a link makes, each the other's inverse, after refusing
    everything that would make the link meaningless: a threshold with no length
    (its transform would collapse the world to a point), two thresholds
    differing in length or height (the opening would not line up, so the seam
    would be visible from both sides), and two that disagree about a door (a
    leaf on one side and an opening on the other would be a door you could see
    through from behind).

    What is compared is whether a leaf hangs there and what it is doing, not
    what it is made of: the renderer draws the near side's, so a door that is
    oak from the hall and stone from the cellar is a choice and not a mistake.
    The state is another matter — a door open from one side and closed from the
    other is one the player could walk through in only one direction, which is
    not a door. {!set_door} is how it is changed, and it changes both sides at
    once.

    Shared by {!make} and {!link} so a world that grew is held to exactly the
    same standard as one that was written down.

    {b Each measurement is refused by negating what would pass}, rather than by
    asserting what would fail. The two read the same for an ordinary number and
    differently for [nan], which answers false to every ordered comparison it is
    given: written the other way round a threshold of length [nan] would be
    neither long enough to reject nor different enough from its twin to reject,
    and the world would be built out of a transform that was [nan] throughout.
    {!Room.doorway} refuses the degenerate wall such a threshold comes from and
    {!Room.threshold} refuses one built by hand, so what reaches here is always
    a positive finite number; what this adds is [epsilon], a length too small to
    be a doorway rather than no length at all. {!Transform.between} refuses it a
    third time, on its own account: a length of zero is the one thing that would
    let it hand back something that was not a rotation, so it does not rely on
    being called from here. What is only checked here is the {e agreement}
    between the two — lengths, heights, doors — which is a fact about a doorway
    rather than about a transform. *)
let pair ~who ~describe (ia, ja, (a : Room.threshold))
    (ib, jb, (b : Room.threshold)) =
  let length (t : Room.threshold) room =
    if not (has_length t) then
      invalid_arg (who ^ ": threshold has no length: " ^ describe room t)
  in
  length a ia;
  length b ib;
  let both = describe ia a ^ " and " ^ describe ib b in
  if not (lengths_agree a b) then
    invalid_arg (who ^ ": linked thresholds differ in length: " ^ both);
  if not (heights_agree a b) then
    invalid_arg (who ^ ": linked thresholds differ in height: " ^ both);
  if not (doors_agree a b) then
    invalid_arg (who ^ ": linked thresholds disagree about a door: " ^ both);
  let onto =
    Transform.between ~a1:a.Room.a ~a2:a.Room.b ~b1:b.Room.a ~b2:b.Room.b
  in
  ( { threshold = a; to_room = ib; twin = jb; onto },
    { threshold = b; to_room = ia; twin = ja; onto = Transform.inverse onto } )

let make ~rooms ~links ~atmosphere ~spawn =
  let names = Array.of_list (List.map fst rooms) in
  let values = Array.of_list (List.map snd rooms) in
  let describe room name = names.(room) ^ "." ^ name in
  let find_room name =
    match Array.find_index (String.equal name) names with
    | Some index -> index
    | None -> invalid_arg ("World.make: no room named " ^ name)
  in
  check_room_names ~who:"World.make" names;
  Array.iteri
    (fun room r -> check_names ~who:"World.make" ~room names.(room) r)
    values;
  let find_threshold room name =
    let r = values.(room) in
    let rec search index =
      if index >= Room.threshold_count r then
        invalid_arg ("World.make: no threshold " ^ describe room name)
      else
        let (t : Room.threshold) = Room.threshold_at r index in
        if String.equal t.Room.name name then (index, t) else search (index + 1)
    in
    search 0
  in
  (* One slot per threshold, filled as the links are read: a slot filled twice
     is a threshold linked twice, and one left empty is a threshold linked to
     nothing. *)
  let slots =
    Array.map
      (fun (r : Room.t) -> Array.make (Room.threshold_count r) None)
      values
  in
  let fill room index name portal =
    if Option.is_some slots.(room).(index) then
      invalid_arg ("World.make: threshold linked twice: " ^ describe room name);
    slots.(room).(index) <- Some portal
  in
  List.iter
    (fun ((room_a, name_a), (room_b, name_b)) ->
      let ia = find_room room_a and ib = find_room room_b in
      let ja, a = find_threshold ia name_a
      and jb, b = find_threshold ib name_b in
      let here, there =
        pair ~who:"World.make"
          ~describe:(fun room (t : Room.threshold) -> describe room t.Room.name)
          (ia, ja, a) (ib, jb, b)
      in
      fill ia ja name_a here;
      fill ib jb name_b there)
    links;
  Array.iteri
    (fun room ->
      Array.iteri (fun index -> function
        | Some _ -> ()
        | None ->
            let t = Room.threshold_at values.(room) index in
            invalid_arg
              ("World.make: nothing links threshold "
             ^ describe room t.Room.name)))
    slots;
  let spawn_room, spawn_pos = spawn in
  {
    rooms = values;
    names;
    portals = slots;
    atmosphere;
    spawn = { room = find_room spawn_room; pos = spawn_pos };
  }

(* Do two thresholds describe the same opening? Not physical equality, because
    a generator that rebuilds a room from its parts hands back thresholds that
    are equal without being the same value; and not full structural equality
    either, because a door may be hung in an opening that is already there. What
    has to hold is that the {e opening} is unmoved, since that is what a [twin]
    index and a link's {!Transform} were derived from. *)
let same_opening (x : Room.threshold) (y : Room.threshold) =
  String.equal x.Room.name y.Room.name
  && x.Room.a = y.Room.a && x.Room.b = y.Room.b
  && x.Room.height = y.Room.height

let open_doorway t ~room ~opened =
  let before = t.rooms.(room) in
  let n = Room.threshold_count before in
  let where = t.names.(room) in
  if Room.threshold_count opened <> n + 1 then
    invalid_arg
      (Printf.sprintf
         "World.open_doorway: %s must gain exactly one threshold, from %d to %d"
         where n
         (Room.threshold_count opened));
  for i = 0 to n - 1 do
    let (x : Room.threshold) = Room.threshold_at opened i in
    if not (same_opening x (Room.threshold_at before i)) then
      invalid_arg
        ("World.open_doorway: " ^ where ^ " moved its existing threshold "
       ^ x.Room.name)
  done;
  check_names ~who:"World.open_doorway" ~room where opened;
  let rooms = Array.copy t.rooms and portals = Array.copy t.portals in
  rooms.(room) <- opened;
  portals.(room) <- Array.append t.portals.(room) [| None |];
  { t with rooms; portals }

let add_room t ~name room =
  if Array.exists (String.equal name) t.names then
    invalid_arg ("World.add_room: a room is already named " ^ name);
  check_names ~who:"World.add_room" ~room:(Array.length t.rooms) name room;
  ( {
      t with
      rooms = Array.append t.rooms [| room |];
      names = Array.append t.names [| name |];
      portals =
        Array.append t.portals [| Array.make (Room.threshold_count room) None |];
    },
    Array.length t.rooms )

let link t (room_a, name_a) (room_b, name_b) =
  let find room name =
    let r = t.rooms.(room) in
    let rec search index =
      if index >= Room.threshold_count r then
        invalid_arg ("World.link: no threshold " ^ t.names.(room) ^ "." ^ name)
      else
        let (x : Room.threshold) = Room.threshold_at r index in
        if String.equal x.Room.name name then (index, x) else search (index + 1)
    in
    search 0
  in
  let describe room (x : Room.threshold) = t.names.(room) ^ "." ^ x.Room.name in
  let ja, a = find room_a name_a and jb, b = find room_b name_b in
  let free room j (x : Room.threshold) =
    if Option.is_some t.portals.(room).(j) then
      invalid_arg ("World.link: threshold linked twice: " ^ describe room x)
  in
  free room_a ja a;
  free room_b jb b;
  if room_a = room_b && ja = jb then
    invalid_arg
      ("World.link: a threshold cannot lead to itself: " ^ describe room_a a);
  let here, there =
    pair ~who:"World.link" ~describe (room_a, ja, a) (room_b, jb, b)
  in
  let portals = Array.copy t.portals in
  let fill room j portal =
    let row = Array.copy portals.(room) in
    row.(j) <- Some portal;
    portals.(room) <- row
  in
  fill room_a ja here;
  fill room_b jb there;
  { t with portals }

let replace_room t ~room ~replacement =
  let before = t.rooms.(room) in
  let n = Room.threshold_count before in
  let where = t.names.(room) in
  if Room.threshold_count replacement <> n then
    invalid_arg
      (Printf.sprintf
         "World.replace_room: %s has %d thresholds and its replacement has %d"
         where n
         (Room.threshold_count replacement));
  for i = 0 to n - 1 do
    let (x : Room.threshold) = Room.threshold_at replacement i in
    if not (same_opening x (Room.threshold_at before i)) then
      invalid_arg
        ("World.replace_room: " ^ where ^ " moved or reordered its threshold "
       ^ x.Room.name)
  done;
  let rooms = Array.copy t.rooms in
  rooms.(room) <- replacement;
  { t with rooms }

let set_door t ~room ~threshold state =
  let hang world ~room ~threshold =
    let before = world.rooms.(room) in
    if threshold < 0 || threshold >= Room.threshold_count before then
      invalid_arg
        (Printf.sprintf "World.set_door: %s has no threshold %d"
           world.names.(room) threshold);
    let x = Room.threshold_at before threshold in
    match x.Room.door with
    | None ->
        invalid_arg
          ("World.set_door: no door hangs in " ^ world.names.(room) ^ "."
         ^ x.Room.name)
    | Some door ->
        let thresholds =
          Array.init (Room.threshold_count before) (Room.threshold_at before)
        in
        thresholds.(threshold) <-
          Room.with_door x (Some (Door.set_state door state));
        (* Through {!replace_room}, so that a door changed here is held to the
           same invariants as a room rebuilt for any other reason. *)
        replace_room world ~room
          ~replacement:(Room.with_thresholds before thresholds)
  in
  let after = hang t ~room ~threshold in
  match t.portals.(room).(threshold) with
  | None -> after
  | Some portal -> hang after ~room:portal.to_room ~threshold:portal.twin

let check t =
  check_room_names ~who:"World.check" t.names;
  Array.iteri
    (fun room r -> check_names ~who:"World.check" ~room t.names.(room) r)
    t.rooms;
  Array.iteri
    (fun room row ->
      Array.iteri
        (fun index -> function
          | None ->
              let x = Room.threshold_at t.rooms.(room) index in
              invalid_arg
                ("World.check: nothing links threshold " ^ t.names.(room) ^ "."
               ^ x.Room.name)
          | Some portal ->
              let describe =
                t.names.(room) ^ "." ^ portal.threshold.Room.name
              in
              if Array.length t.portals.(portal.to_room) <= portal.twin then
                invalid_arg ("World.check: twin out of range: " ^ describe);
              (match t.portals.(portal.to_room).(portal.twin) with
              | Some back when back.to_room = room && back.twin = index -> ()
              | _ ->
                  invalid_arg
                    ("World.check: twin does not lead back: " ^ describe));
              let mine = Room.threshold_at t.rooms.(room) index in
              if
                door_state mine
                <> door_state
                     (Room.threshold_at t.rooms.(portal.to_room) portal.twin)
              then
                invalid_arg
                  ("World.check: linked thresholds disagree about a door: "
                 ^ describe);
              if not (same_opening portal.threshold mine) then
                invalid_arg
                  ("World.check: portal and threshold disagree: " ^ describe))
        row)
    t.portals

let passable t ~room:index ~from ~dest =
  let here = t.rooms.(index) in
  let near (threshold : Room.threshold) =
    Room.distance_between_segments ~a1:from ~a2:dest ~b1:threshold.Room.a
      ~b2:threshold.Room.b
    < Config.collision_padding
  in
  (* How far a point stands on this room's side of a threshold. The normal is
     unit and points into the room that owns the threshold, so this is a signed
     distance: positive inside, negative out through the opening. *)
  let depth (threshold : Room.threshold) p =
    Vec.dot (Vec.sub p threshold.Room.a) threshold.Room.normal
  in
  (* The part of the step that is the neighbour's business: what is left of it
     once it is cut where it crosses the threshold's plane. The cut is [limit]
     short of the plane rather than on it, because the player is a disc and a
     disc whose centre is still this side of an opening can already be touching
     something through it — and a link reverses the two thresholds, so a point
     [limit] this side of one is [limit] the far side of the other, which is
     exactly the reach {!Room.passable} then measures with.

     [depth] is affine along the step, so what survives is one sub-interval and
     never two. Both ends within it keep the step whole, which is also the
     parallel case; both beyond leave nothing to ask about, which is what a step
     running past a doorway without reaching it should cost. Only the two mixed
     arms divide, and there one end is above [limit] and the other is not, so the
     denominator cannot be zero and [u] lands in [0..1] without clamping — a
     clamp here would hide a sign error rather than guard against one. A [nan]
     coordinate falls through every ordered comparison into the last arm and
     comes back passable, which is what the whole function did with one before. *)
  let across (threshold : Room.threshold) =
    let limit = Config.collision_padding in
    let d0 = depth threshold from and d1 = depth threshold dest in
    let at u = Vec.add from (Vec.scale (Vec.sub dest from) u) in
    if d0 <= limit && d1 <= limit then Some (from, dest)
    else if d0 > limit && d1 > limit then None
    else
      let u = (d0 -. limit) /. (d0 -. d1) in
      if d0 > limit then Some (at u, dest) else Some (from, at u)
  in
  let beyond (portal : portal) threshold =
    match across threshold with
    | None -> true
    | Some (a, b) ->
        Room.passable t.rooms.(portal.to_room)
          ~from:(Transform.point portal.onto a)
          ~dest:(Transform.point portal.onto b)
  in
  let clear j (threshold : Room.threshold) =
    if Room.shut threshold then not (near threshold)
    else
      match t.portals.(index).(j) with
      | None -> not (near threshold)
      | Some portal -> (not (near threshold)) || beyond portal threshold
  in
  let rec every j =
    j >= Room.threshold_count here
    || (clear j (Room.threshold_at here j) && every (j + 1))
  in
  Room.passable here ~from ~dest && every 0

let crossing t ~room ~from ~dest =
  let row = t.portals.(room) in
  let side (threshold : Room.threshold) p =
    Vec.cross threshold.Room.edge (Vec.sub p threshold.Room.a)
  in
  (* Walked by index rather than folded over, because which doorway it was is
     half the answer: an index is what {!replace_room} leaves valid and what a
     [twin] is already expressed in, where a copy of the threshold would go
     stale the moment a leaf was hung in it. *)
  let rec nearest slot best =
    if slot >= Array.length row then best
    else
      let best =
        match row.(slot) with
        | Some (portal : portal)
          when Room.segments_cross ~a1:from ~a2:dest ~b1:portal.threshold.a
                 ~b2:portal.threshold.b -> (
            let entering = side portal.threshold from
            and leaving = side portal.threshold dest in
            if entering < 0. || leaving >= 0. then best
            else
              let here = entering /. (entering -. leaving) in
              match best with
              | Some (_, _, there) when there <= here -> best
              | _ -> Some (slot, portal, here))
        | _ -> best
      in
      nearest (slot + 1) best
  in
  nearest 0 None

let seam_gap t ~room:index portal =
  let here = t.rooms.(index) and there = t.rooms.(portal.to_room) in
  let difference p =
    Float.abs
      (Plane.elevation (Room.floor_plane here) p
      -. Plane.elevation (Room.floor_plane there)
           (Transform.point portal.onto p))
  in
  Float.max (difference portal.threshold.a) (difference portal.threshold.b)
