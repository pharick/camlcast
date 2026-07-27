open Raycaster
open Support

let links_resolve () =
  Alcotest.(check int) "two rooms" 2 (Array.length two_rooms.World.rooms);
  Alcotest.(check int)
    "one portal each way" 1
    (Array.length (World.portals two_rooms 0));
  Alcotest.(check int)
    "destination" 1 (portal two_rooms ~room:0 ~index:0).World.to_room;
  (* The renderer looks a portal up by the index a ray reports for the
     threshold, so the two arrays have to line up. *)
  Array.iteri
    (fun room (r : Room.t) ->
      Array.iteri
        (fun index (t : Room.threshold) ->
          Alcotest.(check string)
            "portals run parallel to thresholds" t.Room.name
            (portal two_rooms ~room ~index).World.threshold.Room.name)
        r.Room.thresholds)
    two_rooms.World.rooms

(* Each room is authored in its own coordinates, both here occupying the same
   0..4 square, so a crossing is only meaningful once the transform has carried
   the point into the neighbour's frame. *)
let crossing_changes_frame () =
  let slot, portal, at =
    Option.get
      (World.crossing two_rooms ~room:0 ~from:(Vec.make 3.8 2.)
         ~dest:(Vec.make 4.2 2.))
  in
  Alcotest.(check int) "through the room's only doorway" 0 slot;
  Alcotest.(check int) "crosses into second" 1 portal.to_room;
  (* How far along the step the opening was met. Movement clips the leg here and
     resolves the rest of it in the room on the other side, so this is not a
     detail of the answer but half of it. The doorway is at [x = 4] and the step
     runs from 3.8 to 4.2, so it is met exactly halfway. *)
  Alcotest.check close "met halfway along the step" 0.5 at;
  Alcotest.check vec "point lands inside neighbour" (Vec.make 0.2 2.)
    (Transform.point portal.onto (Vec.make 4.2 2.));
  Alcotest.(check bool)
    "a step that misses the doorway crosses nothing" true
    (World.crossing two_rooms ~room:0 ~from:(Vec.make 3.8 0.5)
       ~dest:(Vec.make 4.2 0.5)
    = None);
  Alcotest.(check bool)
    "nor does one that never reaches it" true
    (World.crossing two_rooms ~room:0 ~from:(Vec.make 1. 2.)
       ~dest:(Vec.make 2. 2.)
    = None)

(* Nothing of this room stops a step taken through its own doorway, so a step
   angled through the opening can be flush against something standing just
   inside the next room. The room alone says yes; the world has to say no. *)
let a_step_into_the_neighbour_is_refused () =
  let from = Vec.make 3.7 2.2 and dest = Vec.make 4.3 2.35 in
  Alcotest.(check bool)
    "this room sees nothing in the way" true
    (Room.can_step (World.room two_rooms 0) ~from ~dest);
  Alcotest.(check bool)
    "but the world sees the neighbour's wall" false
    (World.can_step two_rooms ~room:0 ~from ~dest);
  Alcotest.(check bool)
    "straight through the middle is still free" true
    (World.can_step two_rooms ~room:0 ~from:(Vec.make 3.5 2.)
       ~dest:(Vec.make 4.5 2.))

(* A leaf standing open changes none of that: it is a door swung aside, so the
   opening behaves as a bare one, and it is still the room behind it — as real
   as the room behind any opening — that refuses the step. *)
let a_step_through_a_door_is_refused_by_the_neighbour () =
  let ajar = two_rooms_with_a_door Door.Open in
  let from = Vec.make 3.7 2.2 and dest = Vec.make 4.3 2.35 in
  Alcotest.(check bool)
    "this room sees nothing in the way" true
    (Room.can_step (World.room ajar 0) ~from ~dest);
  Alcotest.(check bool)
    "but the world sees the neighbour's wall" false
    (World.can_step ajar ~room:0 ~from ~dest);
  Alcotest.(check bool)
    "and an open door is still one you can walk through" true
    (World.can_step ajar ~room:0 ~from:(Vec.make 3.5 2.) ~dest:(Vec.make 4.5 2.))

(* The straight step through the middle of the opening, in both states. This is
   the one the engine used to get wrong in either: a leaf was drawn and the
   player walked through it regardless. *)
let a_door_blocks_in_the_states_that_have_a_leaf () =
  let through world =
    World.can_step world ~room:0 ~from:(Vec.make 3.5 2.) ~dest:(Vec.make 4.5 2.)
  in
  Alcotest.(check bool)
    "an open door lets the step by" true
    (through (two_rooms_with_a_door Door.Open));
  Alcotest.(check bool)
    "a closed one does not" false
    (through (two_rooms_with_a_door Door.Closed));
  Alcotest.(check bool)
    "nor is there anything to cross into through a shut door" true
    (World.crossing two_rooms_closed ~room:0 ~from:(Vec.make 3.5 2.)
       ~dest:(Vec.make 4.5 2.)
    |> Option.is_some);
  (* [crossing] still reports the doorway — it answers "which opening is this
     step through", and [can_step] is what has already said no. Movement asks
     both, in that order. *)
  Alcotest.(check bool)
    "a step alongside a shut door is unaffected" true
    (World.can_step two_rooms_closed ~room:0 ~from:(Vec.make 2. 2.)
       ~dest:(Vec.make 2.5 2.));
  (* Walking is where the two meet: the step is refused, so the player stays. *)
  let start = Player.create ~room:0 ~pos:(Vec.make 3.5 2.) ~angle:0. in
  let moved = Player.traverse two_rooms_closed start ~forward:1. ~strafe:0. in
  Alcotest.(check int)
    "the player is still on this side of it" 0 moved.Player.player.Player.room;
  Alcotest.(check int)
    "and went through no doorway at all" 0
    (List.length moved.Player.crossings)

let square ?(thresholds = []) () =
  Room.make ~thresholds ~floor:flat_floor ~ceiling:flat_ceiling
    [ Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.) ]

let gate ?(name = "gate") ?(length = 1.) ?(height = 2.) ?door () =
  Room.threshold ~name ?door ~height (Vec.make 0. 0.) (Vec.make 0. length)

(* Every one of these is an authoring mistake with no sensible run-time
   behaviour, so make refuses the world outright rather than building one that
   renders wrongly. *)
let raises what message body =
  Alcotest.check_raises what (Invalid_argument message) body

let invalid_worlds_are_refused () =
  raises "unknown room" "World.make: no room named missing" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("only", square ()) ]
           ~links:[] ~atmosphere:air ~spawn:("missing", centre)));
  raises "unknown threshold" "World.make: no threshold a.nowhere" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate () ] ()) ]
           ~links:[ (("a", "nowhere"), ("a", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)));
  raises "duplicate room names" "World.make: two rooms named a" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ()); ("a", square ()) ]
           ~links:[] ~atmosphere:air ~spawn:("a", centre)));
  raises "duplicate names" "World.make: two thresholds named a.gate" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate (); gate () ] ()) ]
           ~links:[] ~atmosphere:air ~spawn:("a", centre)));
  raises "no length" "World.make: threshold has no length: a.gate" (fun () ->
      ignore
        (World.make
           ~rooms:
             [
               ("a", square ~thresholds:[ gate ~length:0. () ] ());
               ("b", square ~thresholds:[ gate ~length:0. () ] ());
             ]
           ~links:[ (("a", "gate"), ("b", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)));
  raises "mismatched length"
    "World.make: linked thresholds differ in length: a.gate and b.gate"
    (fun () ->
      ignore
        (World.make
           ~rooms:
             [
               ("a", square ~thresholds:[ gate () ] ());
               ("b", square ~thresholds:[ gate ~length:2. () ] ());
             ]
           ~links:[ (("a", "gate"), ("b", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)));
  raises "mismatched height"
    "World.make: linked thresholds differ in height: a.gate and b.gate"
    (fun () ->
      ignore
        (World.make
           ~rooms:
             [
               ("a", square ~thresholds:[ gate () ] ());
               ("b", square ~thresholds:[ gate ~height:3. () ] ());
             ]
           ~links:[ (("a", "gate"), ("b", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)));
  raises "mismatched door"
    "World.make: linked thresholds disagree about a door: a.gate and b.gate"
    (fun () ->
      ignore
        (World.make
           ~rooms:
             [
               ("a", square ~thresholds:[ gate ~door:(Door.make pale) () ] ());
               ("b", square ~thresholds:[ gate () ] ());
             ]
           ~links:[ (("a", "gate"), ("b", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)));
  raises "linked twice" "World.make: threshold linked twice: a.gate" (fun () ->
      ignore
        (World.make
           ~rooms:
             [
               ("a", square ~thresholds:[ gate () ] ());
               ("b", square ~thresholds:[ gate (); gate ~name:"other" () ] ());
             ]
           ~links:
             [ (("a", "gate"), ("b", "gate")); (("a", "gate"), ("b", "other")) ]
           ~atmosphere:air ~spawn:("a", centre)));
  raises "unlinked threshold" "World.make: nothing links threshold a.gate"
    (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate () ] ()) ]
           ~links:[] ~atmosphere:air ~spawn:("a", centre)))

(* {!Room.doorway} refuses the degenerate wall a [nan] threshold comes from, but
   a threshold can be built by hand, and a length of [nan] would slip past every
   check written the natural way round: it is not at or below the minimum, and
   not far enough from its twin's to differ, because an ordered comparison
   against [nan] is false however it is asked. The checks are written as the
   negation of what would pass, which is what makes this the length it does not
   have rather than a world whose every transform is [nan]. *)
let a_threshold_of_no_real_length_is_refused () =
  let nowhere = Vec.make Float.nan Float.nan in
  let broken () =
    square
      ~thresholds:[ Room.threshold ~name:"gate" ~height:2. nowhere nowhere ]
      ()
  in
  raises "a threshold measuring nan"
    "World.make: threshold has no length: a.gate" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", broken ()); ("b", broken ()) ]
           ~links:[ (("a", "gate"), ("b", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)))

(** {1 Growing a world} *)

(* A room with one solid wall along the x axis and nothing else, which the
   growth tests cut doorways into. *)
let cell ?(thresholds = []) ?(walls = []) () =
  Room.make ~thresholds ~floor:flat_floor ~ceiling:flat_ceiling
    (walls
    @ [ Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.) ]
    )

(* A world of one room and no doorways at all: the smallest thing a generator
   could be handed to start from. *)
let seed () =
  World.make
    ~rooms:[ ("start", cell ()) ]
    ~links:[] ~atmosphere:air ~spawn:("start", centre)

let cut ~name a b =
  Room.doorway ~name ~width:1. ~opening:2. ~height:3. ~material:pale a b

(* The whole cycle a generator runs: cut a doorway into a room that already
   exists, hang a new room off it, join the two. Every step leaves a world that
   still renders and still walks. *)
let a_world_can_grow () =
  let world = seed () in
  let jambs, north = cut ~name:"north" (Vec.make 4. 4.) (Vec.make 0. 4.) in
  let world =
    World.open_doorway world ~room:0
      ~opened:(cell ~thresholds:[ north ] ~walls:jambs ())
  in
  Alcotest.(check int)
    "the doorway is there" 1
    (Array.length (World.room world 0).Room.thresholds);
  Alcotest.(check bool)
    "and leads nowhere yet" true
    ((World.portals world 0).(0) = None);
  let jambs, south = cut ~name:"south" (Vec.make 0. 0.) (Vec.make 4. 0.) in
  let world, next =
    World.add_room world ~name:"next"
      (Room.make ~thresholds:[ south ] ~floor:flat_floor ~ceiling:flat_ceiling
         jambs)
  in
  Alcotest.(check int) "the new room landed at the end" 1 next;
  let world = World.link world (0, "north") (next, "south") in
  World.check world;
  let there = portal world ~room:0 ~index:0 in
  Alcotest.(check int) "the doorway leads to it now" next there.World.to_room;
  Alcotest.(check int)
    "and back again" 0
    (portal world ~room:next ~index:there.World.twin).World.to_room

(* Every index anything holds across a frame is a bare int, so growing must only
   ever append. If a room or a threshold were inserted anywhere but the end,
   every portal's twin past that point would silently mean a different doorway. *)
let growing_never_disturbs_an_index () =
  let before = two_rooms in
  let jambs, extra = cut ~name:"extra" (Vec.make 4. 4.) (Vec.make 0. 4.) in
  let opened =
    Room.make
      ~thresholds:
        (Array.to_list (World.room before 0).Room.thresholds @ [ extra ])
      ~floor:flat_floor ~ceiling:flat_ceiling
      (Array.to_list (World.room before 0).Room.walls @ jambs)
  in
  let after = World.open_doorway before ~room:0 ~opened in
  Alcotest.(check int)
    "the existing doorway is still index 0" 1
    (portal after ~room:0 ~index:0).World.to_room;
  Alcotest.(check string)
    "and still the same one" "east"
    (World.room after 0).Room.thresholds.(0).Room.name;
  Alcotest.(check bool)
    "the neighbour's twin still points at it" true
    ((portal after ~room:1 ~index:0).World.twin = 0);
  Alcotest.(check int)
    "the new one went on the end" 2
    (Array.length (World.room after 0).Room.thresholds)

(* Nothing forces a link to agree with the path that already runs between two
   rooms — there is no global frame for it to contradict. Joining a room back to
   one it already reaches produces a loop whose geometry is impossible, and that
   is a feature rather than something to guard against. *)
let a_loop_may_contradict_itself () =
  let jambs, back = cut ~name:"back" (Vec.make 4. 4.) (Vec.make 0. 4.) in
  let opened room name =
    Room.make
      ~thresholds:(Array.to_list room.Room.thresholds @ [ back ])
      ~floor:flat_floor ~ceiling:flat_ceiling
      (Array.to_list room.Room.walls @ jambs)
    |> fun r -> (r, name)
  in
  let first, _ = opened (World.room two_rooms 0) "first" in
  let world = World.open_doorway two_rooms ~room:0 ~opened:first in
  let second, _ = opened (World.room world 1) "second" in
  let world = World.open_doorway world ~room:1 ~opened:second in
  let world = World.link world (0, "back") (1, "back") in
  World.check world;
  Alcotest.(check int)
    "two ways from the first room to the second" 2
    (Array.length
       (Array.of_list
          (List.filter
             (fun p -> (Option.get p).World.to_room = 1)
             (Array.to_list (World.portals world 0)))));
  (* The two routes disagree about where the second room is, which is exactly
     what makes the house bigger on the inside. *)
  let through index = (portal world ~room:0 ~index).World.onto in
  let landing t = Transform.point t centre in
  Alcotest.(check bool)
    "and they arrive at different places" true
    (Vec.length (Vec.sub (landing (through 0)) (landing (through 1))) > 1e-6)

(* An unlinked doorway is a hole in the room's boundary with no room behind it,
   so it has to block: walking out through one would put the player in a room
   that does not exist. *)
let a_doorway_onto_nowhere_is_solid () =
  let jambs, east = cut ~name:"east" (Vec.make 4. 0.) (Vec.make 4. 4.) in
  let world =
    World.open_doorway (seed ()) ~room:0
      ~opened:(cell ~thresholds:[ east ] ~walls:jambs ())
  in
  let from = Vec.make 3.5 2. and dest = Vec.make 4.5 2. in
  Alcotest.(check bool)
    "the room alone sees the gap and lets you through" true
    (Room.can_step (World.room world 0) ~from ~dest);
  Alcotest.(check bool)
    "the world refuses it" false
    (World.can_step world ~room:0 ~from ~dest);
  Alcotest.(check bool)
    "and there is nothing to cross into" true
    (World.crossing world ~room:0 ~from ~dest = None);
  (* Walking past it, not through it, is still fine. *)
  Alcotest.(check bool)
    "a step alongside is unaffected" true
    (World.can_step world ~room:0 ~from:(Vec.make 2. 2.) ~dest:(Vec.make 2.5 2.))

let invalid_growth_is_refused () =
  let world = seed () in
  let jambs, north = cut ~name:"north" (Vec.make 4. 4.) (Vec.make 0. 4.) in
  let one = cell ~thresholds:[ north ] ~walls:jambs () in
  raises "no new threshold"
    "World.open_doorway: start must gain exactly one threshold, from 0 to 0"
    (fun () -> ignore (World.open_doorway world ~room:0 ~opened:(cell ())));
  raises "two at once"
    "World.open_doorway: start must gain exactly one threshold, from 0 to 2"
    (fun () ->
      ignore
        (World.open_doorway world ~room:0
           ~opened:
             (cell
                ~thresholds:
                  [
                    north;
                    Room.threshold ~name:"s" ~height:2. (Vec.make 0. 0.)
                      (Vec.make 1. 0.);
                  ]
                ())));
  (* An existing doorway that moved would leave every twin pointing at it
     meaning a different opening. *)
  let grown = World.open_doorway world ~room:0 ~opened:one in
  let _, moved = cut ~name:"north" (Vec.make 4. 3.) (Vec.make 0. 3.) in
  raises "an existing threshold moved"
    "World.open_doorway: start moved its existing threshold north" (fun () ->
      ignore
        (World.open_doorway grown ~room:0
           ~opened:
             (cell
                ~thresholds:
                  [
                    moved;
                    Room.threshold ~name:"s" ~height:2. (Vec.make 0. 0.)
                      (Vec.make 1. 0.);
                  ]
                ())));
  raises "linking a doorway twice"
    "World.link: threshold linked twice: first.east" (fun () ->
      ignore (World.link two_rooms (0, "east") (1, "west")));
  raises "linking a doorway to itself"
    "World.link: a threshold cannot lead to itself: start.north" (fun () ->
      ignore (World.link grown (0, "north") (0, "north")));
  raises "linking mismatched openings"
    "World.link: linked thresholds differ in height: start.north and next.south"
    (fun () ->
      let jambs, south =
        Room.doorway ~name:"south" ~width:1. ~opening:2.5 ~height:3.
          ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.)
      in
      let bigger, next =
        World.add_room grown ~name:"next"
          (Room.make ~thresholds:[ south ] ~floor:flat_floor
             ~ceiling:flat_ceiling jambs)
      in
      ignore (World.link bigger (0, "north") (next, "south")));
  raises "linking a door to an opening"
    "World.link: linked thresholds disagree about a door: start.north and \
     next.south" (fun () ->
      let jambs, south =
        Room.doorway ~name:"south" ~door:(Door.make pale) ~width:1. ~opening:2.
          ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.)
      in
      let bigger, next =
        World.add_room grown ~name:"next"
          (Room.make ~thresholds:[ south ] ~floor:flat_floor
             ~ceiling:flat_ceiling jambs)
      in
      ignore (World.link bigger (0, "north") (next, "south")));
  (* A leaf may be hung into an opening that is already linked, which is the one
     way the two sides of a link can come to disagree after make and link have
     both had their say — so check has to ask as well. Hanging one takes an
     open_doorway, which insists on a new threshold at the same time; the
     disagreement is at index 0, so it is what check reaches first. *)
  raises "a leaf hung on one side only"
    "World.check: linked thresholds disagree about a door: first.east"
    (fun () ->
      let first = World.room two_rooms 0 in
      let jambs, extra = cut ~name:"extra" (Vec.make 4. 4.) (Vec.make 0. 4.) in
      World.check
        (World.open_doorway two_rooms ~room:0
           ~opened:
             (Room.make
                ~thresholds:
                  [
                    {
                      (first.Room.thresholds.(0)) with
                      Room.door = Some (Door.make pale);
                    };
                    extra;
                  ]
                ~floor:flat_floor ~ceiling:flat_ceiling
                (Array.to_list first.Room.walls @ jambs))));
  raises "an unlinked doorway"
    "World.check: nothing links threshold start.north" (fun () ->
      World.check grown);
  (* A generator appends rooms, and a name it has used before does not collide
     with the one that has it — it shadows it, because a room is resolved by
     [Array.find_index], which answers with the first. A world where the second
     [next] could never be named again is one a later link would silently make
     against the wrong room. *)
  raises "a name another room already has"
    "World.add_room: a room is already named start" (fun () ->
      ignore (World.add_room grown ~name:"start" (cell ())))

(* Replacing a room is how a wall comes to have a chalk mark on it, a sign starts
   moving, and a room is lit differently on the way back than it was on the way
   out. Everything about the room may change but the openings, which are what
   the rest of the world is holding on to. *)
let a_room_can_be_replaced () =
  let before = World.room two_rooms 0 in
  let replacement =
    Room.make
      ~thresholds:(Array.to_list before.Room.thresholds)
      ~floor:{ Room.plane = Plane.horizontal 0.5; material = dim }
      ~ceiling:open_sky
      ~sprites:[ Room.sprite ~size:1. ~image:poster centre ]
      (Array.to_list before.Room.walls
      @ [
          Room.wall ~height:1. ~material:mesh (Vec.make 1. 1.) (Vec.make 2. 1.);
        ])
  in
  let after = World.replace_room two_rooms ~room:0 ~replacement in
  World.check after;
  let now = World.room after 0 in
  Alcotest.(check int)
    "the new wall is there"
    (Array.length before.Room.walls + 1)
    (Array.length now.Room.walls);
  Alcotest.(check int) "and the sprite" 1 (Array.length now.Room.sprites);
  Alcotest.(check bool)
    "the roof came off" true
    (match now.Room.ceiling with Room.Open _ -> true | Room.Roof _ -> false);
  Alcotest.check close "and the floor moved" 0.5
    (Plane.elevation now.Room.floor.Room.plane centre);
  Alcotest.(check int)
    "the room next door is untouched"
    (Array.length (World.room two_rooms 1).Room.walls)
    (Array.length (World.room after 1).Room.walls);
  (* The world is persistent, so the one it was made from still stands. *)
  Alcotest.(check int)
    "and so is the world it was made from"
    (Array.length before.Room.walls)
    (Array.length (World.room two_rooms 0).Room.walls)

(* What the rest of the world holds is a portal: a room index, a twin index and
   a transform, none of which is re-derived when a room is replaced. So they
   have to still mean what they meant — even when the room they describe has
   nothing left in common with the one they were derived from. *)
let replacing_never_disturbs_a_portal () =
  let there = portal two_rooms ~room:0 ~index:0 in
  let before = World.room two_rooms 0 in
  let stripped =
    (* Not one wall left, which the checks permit: nothing outside a room refers
       to its walls. *)
    Room.make
      ~thresholds:(Array.to_list before.Room.thresholds)
      ~floor:flat_floor ~ceiling:flat_ceiling []
  in
  let after = World.replace_room two_rooms ~room:0 ~replacement:stripped in
  World.check after;
  let now = portal after ~room:0 ~index:0 in
  Alcotest.(check int)
    "it leads to the same room" there.World.to_room now.World.to_room;
  Alcotest.(check int) "by the same twin" there.World.twin now.World.twin;
  Alcotest.check vec "and lands in the same place"
    (Transform.point there.World.onto centre)
    (Transform.point now.World.onto centre);
  Alcotest.(check int)
    "and the neighbour still points back at it" 0
    (portal after ~room:1 ~index:now.World.twin).World.to_room

(* A leaf hung through one side leaves the two halves of the link disagreeing,
   which is a world check refuses. Hanging one is still permitted — it is half of
   an operation, not a mistake — and the other half is the same call again on the
   room next door. *)
let a_door_takes_two_replacements () =
  (* Both fixture rooms have exactly the one doorway, so this hangs a leaf in
     the only opening the room has. *)
  let hang world ~room =
    let before = World.room world room in
    World.replace_room world ~room
      ~replacement:
        (Room.make
           ~thresholds:
             (List.map
                (fun (x : Room.threshold) ->
                  { x with Room.door = Some (Door.make pale) })
                (Array.to_list before.Room.thresholds))
           ~floor:before.Room.floor ~ceiling:before.Room.ceiling
           (Array.to_list before.Room.walls))
  in
  let half = hang two_rooms ~room:0 in
  raises "one side only"
    "World.check: linked thresholds disagree about a door: first.east"
    (fun () -> World.check half);
  let both = hang half ~room:1 in
  World.check both;
  Alcotest.(check bool)
    "a leaf hangs on both sides now" true
    ((World.room both 0).Room.thresholds.(0).Room.door <> None
    && (World.room both 1).Room.thresholds.(0).Room.door <> None)

let invalid_replacement_is_refused () =
  let first = World.room two_rooms 0 in
  let like thresholds =
    Room.make ~thresholds ~floor:flat_floor ~ceiling:flat_ceiling
      (Array.to_list first.Room.walls)
  in
  raises "a doorway dropped"
    "World.replace_room: first has 1 thresholds and its replacement has 0"
    (fun () ->
      ignore (World.replace_room two_rooms ~room:0 ~replacement:(like [])));
  (* An opening that moved would leave the twin pointing at it, and the
     transform derived from it, describing a doorway that is no longer there. *)
  let _, moved = cut ~name:"east" (Vec.make 4. 0.) (Vec.make 4. 3.) in
  raises "an opening moved"
    "World.replace_room: first moved or reordered its threshold east" (fun () ->
      ignore
        (World.replace_room two_rooms ~room:0 ~replacement:(like [ moved ])));
  (* Same opening, different height: the two sides would no longer line up. *)
  let _, taller =
    Room.doorway ~name:"east" ~width:1. ~opening:2.5 ~height:3. ~material:pale
      (Vec.make 4. 0.) (Vec.make 4. 4.)
  in
  raises "an opening that changed height"
    "World.replace_room: first moved or reordered its threshold east" (fun () ->
      ignore
        (World.replace_room two_rooms ~room:0 ~replacement:(like [ taller ])));
  (* Reordering is the same fault seen from another angle: a twin is a bare
     index, so the doorway at 0 has to still be the doorway at 0. *)
  let jambs, extra = cut ~name:"extra" (Vec.make 4. 4.) (Vec.make 0. 4.) in
  let two =
    World.open_doorway two_rooms ~room:0
      ~opened:
        (Room.make
           ~thresholds:(Array.to_list first.Room.thresholds @ [ extra ])
           ~floor:flat_floor ~ceiling:flat_ceiling
           (Array.to_list first.Room.walls @ jambs))
  in
  raises "the order changed"
    "World.replace_room: first moved or reordered its threshold extra"
    (fun () ->
      ignore
        (World.replace_room two ~room:0
           ~replacement:
             (Room.make
                ~thresholds:[ extra; first.Room.thresholds.(0) ]
                ~floor:flat_floor ~ceiling:flat_ceiling
                (Array.to_list first.Room.walls @ jambs))))

(* A door is one thing seen from two rooms, and the two have to agree about what
   it is doing — a door open from one side and locked from the other is one the
   player could walk through in one direction only. set_door is the only way to
   change one, precisely so that the disagreeing world never exists. *)
let state_of world ~room ~threshold =
  match (World.room world room).Room.thresholds.(threshold).Room.door with
  | Some d -> Some d.Door.state
  | None -> None

let setting_a_door_changes_both_sides () =
  let before = two_rooms_closed in
  let twin = (portal before ~room:0 ~index:0).World.twin in
  Alcotest.(check bool)
    "shut on both sides to begin with" true
    (state_of before ~room:0 ~threshold:0 = Some Door.Closed
    && state_of before ~room:1 ~threshold:twin = Some Door.Closed);
  let opened = World.set_door before ~room:0 ~threshold:0 Door.Open in
  World.check opened;
  Alcotest.(check bool)
    "opened from the room it was asked in" true
    (state_of opened ~room:0 ~threshold:0 = Some Door.Open);
  Alcotest.(check bool)
    "and from the room on the other side of it" true
    (state_of opened ~room:1 ~threshold:twin = Some Door.Open);
  (* Asking from the far side reaches back the same way. *)
  let shut = World.set_door opened ~room:1 ~threshold:twin Door.Closed in
  World.check shut;
  Alcotest.(check bool)
    "shut from both sides again" true
    (state_of shut ~room:0 ~threshold:0 = Some Door.Closed
    && state_of shut ~room:1 ~threshold:twin = Some Door.Closed);
  (* And the world it was changed from still stands, untouched. *)
  Alcotest.(check bool)
    "the world it came from is unchanged" true
    (state_of before ~room:0 ~threshold:0 = Some Door.Closed)

(* Doors are opened and shut over and over across a run, so the operation has to
   be one the world survives repeatedly — no drift in the openings it is
   carrying, no portal left describing the door it used to be. *)
let a_door_can_be_worked_repeatedly () =
  let twin = (portal two_rooms_closed ~room:0 ~index:0).World.twin in
  let world =
    List.fold_left
      (fun world state -> World.set_door world ~room:0 ~threshold:0 state)
      two_rooms_closed
      [
        Door.Open;
        Door.Closed;
        Door.Closed;
        Door.Open;
        Door.Open;
        Door.Closed;
        Door.Open;
      ]
  in
  World.check world;
  Alcotest.(check bool)
    "ends where the last change left it" true
    (state_of world ~room:0 ~threshold:0 = Some Door.Open
    && state_of world ~room:1 ~threshold:twin = Some Door.Open);
  Alcotest.(check bool)
    "the opening never moved" true
    (let now = (World.room world 0).Room.thresholds.(0)
     and then_ = (World.room two_rooms_closed 0).Room.thresholds.(0) in
     now.Room.a = then_.Room.a && now.Room.b = then_.Room.b
     && now.Room.height = then_.Room.height);
  Alcotest.(check int)
    "and the portal still leads where it did" 1
    (portal world ~room:0 ~index:0).World.to_room;
  Alcotest.(check bool)
    "so an opened door is walkable again" true
    (World.can_step world ~room:0 ~from:(Vec.make 3.5 2.)
       ~dest:(Vec.make 4.5 2.))

(* An unlinked doorway has only one side, and gets it. *)
let a_door_on_an_unlinked_doorway_has_one_side () =
  let jambs, north =
    Room.doorway ~name:"north" ~door:(Door.make pale) ~width:1. ~opening:2.
      ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.)
  in
  let world =
    World.open_doorway (seed ()) ~room:0
      ~opened:(cell ~thresholds:[ north ] ~walls:jambs ())
  in
  let opened = World.set_door world ~room:0 ~threshold:0 Door.Open in
  Alcotest.(check bool)
    "it opens" true
    (state_of opened ~room:0 ~threshold:0 = Some Door.Open);
  (* Opening it does not make a doorway onto nowhere passable: there is still
     no room to walk into. *)
  Alcotest.(check bool)
    "and still leads nowhere" false
    (World.can_step opened ~room:0 ~from:(Vec.make 2. 3.5)
       ~dest:(Vec.make 2. 4.5))

let working_a_door_that_is_not_there_is_refused () =
  raises "no door in the opening" "World.set_door: no door hangs in first.east"
    (fun () -> ignore (World.set_door two_rooms ~room:0 ~threshold:0 Door.Open));
  raises "no such threshold" "World.set_door: first has no threshold 3"
    (fun () ->
      ignore (World.set_door two_rooms_closed ~room:0 ~threshold:3 Door.Open))

let () =
  Alcotest.run "World"
    [
      ( "links",
        [
          case "resolve by name" links_resolve;
          case "crossing changes frame" crossing_changes_frame;
          case "a step into the neighbour is refused"
            a_step_into_the_neighbour_is_refused;
          case "a step through a door is refused by the neighbour"
            a_step_through_a_door_is_refused_by_the_neighbour;
          case "invalid worlds are refused" invalid_worlds_are_refused;
          case "a threshold of no real length is refused"
            a_threshold_of_no_real_length_is_refused;
        ] );
      ( "doors",
        [
          case "a door blocks in the states that have a leaf"
            a_door_blocks_in_the_states_that_have_a_leaf;
          case "setting a door changes both sides"
            setting_a_door_changes_both_sides;
          case "a door can be worked repeatedly" a_door_can_be_worked_repeatedly;
          case "a door on an unlinked doorway has one side"
            a_door_on_an_unlinked_doorway_has_one_side;
          case "working a door that is not there is refused"
            working_a_door_that_is_not_there_is_refused;
        ] );
      ( "growing",
        [
          case "a world can grow" a_world_can_grow;
          case "growing never disturbs an index" growing_never_disturbs_an_index;
          case "a loop may contradict itself" a_loop_may_contradict_itself;
          case "a doorway onto nowhere is solid" a_doorway_onto_nowhere_is_solid;
          case "invalid growth is refused" invalid_growth_is_refused;
        ] );
      ( "replacing",
        [
          case "a room can be replaced" a_room_can_be_replaced;
          case "replacing never disturbs a portal"
            replacing_never_disturbs_a_portal;
          case "a door takes two replacements" a_door_takes_two_replacements;
          case "invalid replacement is refused" invalid_replacement_is_refused;
        ] );
    ]
