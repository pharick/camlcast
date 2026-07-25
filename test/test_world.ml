open Raycaster
open Support

let links_resolve () =
  Alcotest.(check int) "two rooms" 2 (Array.length two_rooms.World.rooms);
  Alcotest.(check int) "one portal each way" 1
    (Array.length (World.portals two_rooms 0));
  Alcotest.(check int) "destination" 1
    (portal two_rooms ~room:0 ~index:0).World.to_room;
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
  let portal =
    Option.get
      (World.crossing two_rooms ~room:0 ~from:(Vec.make 3.8 2.)
         ~dest:(Vec.make 4.2 2.))
  in
  Alcotest.(check int) "crosses into second" 1 portal.to_room;
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

(* A leaf in the opening changes none of that. Walking into a door is how you go
   through it, so the door itself must not collide — but the room behind it is as
   real as the room behind an open doorway, and used to go unasked. *)
let a_step_through_a_door_is_refused_by_the_neighbour () =
  let from = Vec.make 3.7 2.2 and dest = Vec.make 4.3 2.35 in
  Alcotest.(check bool)
    "this room sees nothing in the way" true
    (Room.can_step (World.room two_rooms_with_a_door 0) ~from ~dest);
  Alcotest.(check bool)
    "but the world sees the neighbour's wall" false
    (World.can_step two_rooms_with_a_door ~room:0 ~from ~dest);
  Alcotest.(check bool)
    "and the door is still one you can walk through" true
    (World.can_step two_rooms_with_a_door ~room:0 ~from:(Vec.make 3.5 2.)
       ~dest:(Vec.make 4.5 2.))

let square ?(thresholds = []) () =
  Room.make ~thresholds ~floor:flat_floor ~ceiling:flat_ceiling
    [ Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.) ]

let gate ?(name = "gate") ?(length = 1.) ?(height = 2.) ?door () =
  Room.threshold ~name ?door ~height (Vec.make 0. 0.) (Vec.make 0. length)

(* Every one of these is an authoring mistake with no sensible run-time
   behaviour, so make refuses the world outright rather than building one that
   renders wrongly. *)
let raises what message body = Alcotest.check_raises what (Invalid_argument message) body

let invalid_worlds_are_refused () =
  raises "unknown room" "World.make: no room named missing" (fun () ->
      ignore
        (World.make ~rooms:[ ("only", square ()) ] ~links:[]
           ~atmosphere:air ~spawn:("missing", centre)));
  raises "unknown threshold" "World.make: no threshold a.nowhere" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate () ] ()) ]
           ~links:[ (("a", "nowhere"), ("a", "gate")) ]
           ~atmosphere:air ~spawn:("a", centre)));
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
               ("a", square ~thresholds:[ gate ~door:pale () ] ());
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

(** {1 Growing a world} *)

(* A room with one solid wall along the x axis and nothing else, which the
   growth tests cut doorways into. *)
let cell ?(thresholds = []) ?(walls = []) () =
  Room.make ~thresholds ~floor:flat_floor ~ceiling:flat_ceiling
    (walls
    @ [ Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.) ])

(* A world of one room and no doorways at all: the smallest thing a generator
   could be handed to start from. *)
let seed () =
  World.make ~rooms:[ ("start", cell ()) ] ~links:[] ~atmosphere:air
    ~spawn:("start", centre)

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
  Alcotest.(check int) "and back again" 0
    (portal world ~room:next ~index:there.World.twin).World.to_room

(* Every index anything holds across a frame is a bare int, so growing must only
   ever append. If a room or a threshold were inserted anywhere but the end,
   every portal's twin past that point would silently mean a different doorway. *)
let growing_never_disturbs_an_index () =
  let before = two_rooms in
  let jambs, extra = cut ~name:"extra" (Vec.make 4. 4.) (Vec.make 0. 4.) in
  let opened =
    Room.make
      ~thresholds:(Array.to_list (World.room before 0).Room.thresholds @ [ extra ])
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
                  [ north; Room.threshold ~name:"s" ~height:2. (Vec.make 0. 0.)
                             (Vec.make 1. 0.) ]
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
                  [ moved;
                    Room.threshold ~name:"s" ~height:2. (Vec.make 0. 0.)
                      (Vec.make 1. 0.) ]
                ())));
  raises "linking a doorway twice" "World.link: threshold linked twice: first.east"
    (fun () -> ignore (World.link two_rooms (0, "east") (1, "west")));
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
    "World.link: linked thresholds disagree about a door: start.north and next.south"
    (fun () ->
      let jambs, south =
        Room.doorway ~name:"south" ~door:pale ~width:1. ~opening:2. ~height:3.
          ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.)
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
    "World.check: linked thresholds disagree about a door: first.east" (fun () ->
      let first = World.room two_rooms 0 in
      let jambs, extra = cut ~name:"extra" (Vec.make 4. 4.) (Vec.make 0. 4.) in
      World.check
        (World.open_doorway two_rooms ~room:0
           ~opened:
             (Room.make
                ~thresholds:
                  [
                    { (first.Room.thresholds.(0)) with Room.door = Some pale };
                    extra;
                  ]
                ~floor:flat_floor ~ceiling:flat_ceiling
                (Array.to_list first.Room.walls @ jambs))));
  raises "an unlinked doorway" "World.check: nothing links threshold start.north"
    (fun () -> World.check grown)

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
        ] );
      ( "growing",
        [
          case "a world can grow" a_world_can_grow;
          case "growing never disturbs an index" growing_never_disturbs_an_index;
          case "a loop may contradict itself" a_loop_may_contradict_itself;
          case "a doorway onto nowhere is solid" a_doorway_onto_nowhere_is_solid;
          case "invalid growth is refused" invalid_growth_is_refused;
        ] );
    ]
