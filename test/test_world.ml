open Raycaster
open Support

let links_resolve () =
  Alcotest.(check int) "two rooms" 2 (Array.length two_rooms.World.rooms);
  Alcotest.(check int) "one portal each way" 1
    (Array.length (World.portals two_rooms 0));
  Alcotest.(check int) "destination" 1
    (World.portals two_rooms 0).(0).World.to_room;
  (* The renderer looks a portal up by the index a ray reports for the
     threshold, so the two arrays have to line up. *)
  Array.iteri
    (fun room (r : Room.t) ->
      Array.iteri
        (fun index (t : Room.threshold) ->
          Alcotest.(check string)
            "portals run parallel to thresholds" t.Room.name
            (World.portals two_rooms room).(index).World.threshold.Room.name)
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

let square ?(thresholds = []) () =
  Room.make ~thresholds ~floor:flat_floor ~ceiling:flat_ceiling
    [ Room.wall ~height:3. ~texture:1 (Vec.make 0. 0.) (Vec.make 4. 0.) ]

let gate ?(name = "gate") ?(length = 1.) ?(height = 2.) () =
  Room.threshold ~name ~height (Vec.make 0. 0.) (Vec.make 0. length)

(* Every one of these is an authoring mistake with no sensible run-time
   behaviour, so make refuses the world outright rather than building one that
   renders wrongly. *)
let raises what message body = Alcotest.check_raises what (Invalid_argument message) body

let invalid_worlds_are_refused () =
  raises "unknown room" "World.make: no room named missing" (fun () ->
      ignore
        (World.make ~rooms:[ ("only", square ()) ] ~links:[]
           ~spawn:("missing", centre)));
  raises "unknown threshold" "World.make: no threshold a.nowhere" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate () ] ()) ]
           ~links:[ (("a", "nowhere"), ("a", "gate")) ]
           ~spawn:("a", centre)));
  raises "duplicate names" "World.make: two thresholds named a.gate" (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate (); gate () ] ()) ]
           ~links:[] ~spawn:("a", centre)));
  raises "no length" "World.make: threshold has no length: a.gate" (fun () ->
      ignore
        (World.make
           ~rooms:
             [
               ("a", square ~thresholds:[ gate ~length:0. () ] ());
               ("b", square ~thresholds:[ gate ~length:0. () ] ());
             ]
           ~links:[ (("a", "gate"), ("b", "gate")) ]
           ~spawn:("a", centre)));
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
           ~spawn:("a", centre)));
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
           ~spawn:("a", centre)));
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
           ~spawn:("a", centre)));
  raises "unlinked threshold" "World.make: nothing links threshold a.gate"
    (fun () ->
      ignore
        (World.make
           ~rooms:[ ("a", square ~thresholds:[ gate () ] ()) ]
           ~links:[] ~spawn:("a", centre)))

(* The demo world is the only thing that exercises all of this at once, so it is
   checked for the properties the rest of the engine assumes of it. *)
let the_default_world_is_playable () =
  let spawn = World.default.World.spawn in
  Alcotest.(check bool)
    "the spawn point is not inside a wall" false
    (Room.blocked (World.room World.default spawn.room) spawn.pos);
  (* Every room encloses itself. Split into rooms, each one has to close its own
     boundary or its floor and sky run to the horizon through the gap. *)
  Array.iteri
    (fun i (r : Room.t) ->
      Alcotest.(check bool)
        (World.default.World.names.(i) ^ " is walled all round")
        true
        (Array.length r.Room.walls >= 4))
    World.default.World.rooms;
  Alcotest.(check int)
    "the plaza's ring, pillars and furniture" 42
    (Array.length (World.room World.default 0).Room.walls)

let the_default_world_is_connected () =
  let seen = Array.make (Array.length World.default.World.rooms) false in
  let rec visit i =
    if not seen.(i) then begin
      seen.(i) <- true;
      Array.iter
        (fun (p : World.portal) -> visit p.World.to_room)
        (World.portals World.default i)
    end
  in
  visit World.default.World.spawn.room;
  Alcotest.(check bool)
    "every room is reachable from the spawn" true
    (Array.for_all Fun.id seen)

let the_default_world_is_varied () =
  let rooms = World.default.World.rooms in
  Alcotest.(check bool)
    "some room is open to the sky" true
    (Array.exists (fun (r : Room.t) -> r.Room.ceiling = None) rooms);
  Alcotest.(check bool)
    "some room is roofed" true
    (Array.exists (fun (r : Room.t) -> r.Room.ceiling <> None) rooms);
  Alcotest.(check bool)
    "some threshold is a solid door" true
    (Array.exists
       (Array.exists (fun (p : World.portal) ->
            p.World.threshold.Room.door <> None))
       World.default.World.portals);
  Alcotest.(check bool)
    "every doorway knows the wall above it" true
    (Array.for_all
       (fun (r : Room.t) ->
         Array.for_all
           (fun (t : Room.threshold) -> t.Room.lintel <> None)
           r.Room.thresholds)
       rooms);
  (* Doorways cut into three different sides of the plaza's ring, so the links
     are genuine rotations and not merely translations. *)
  Alcotest.(check bool)
    "some link turns as well as moves" true
    (Array.exists
       (Array.exists (fun (p : World.portal) ->
            Float.abs p.World.onto.Transform.sin > 1e-6))
       World.default.World.portals);
  Alcotest.(check bool)
    "some room's floor is inclined" true
    (Array.exists
       (fun (r : Room.t) ->
         Plane.elevation r.Room.floor (Vec.make 0. 0.)
         <> Plane.elevation r.Room.floor (Vec.make 1. 0.))
       rooms)

(* A floor mismatch across a doorway is a visible step you walk into, so every
   room's floor is built from its neighbour's with Plane.through and the gap
   should be zero to the last bit. *)
let the_default_world_has_no_seams () =
  Array.iteri
    (fun room portals ->
      Array.iter
        (fun (portal : World.portal) ->
          Alcotest.check close
            (World.default.World.names.(room)
            ^ "." ^ portal.World.threshold.Room.name)
            0.
            (World.seam_gap World.default ~room portal))
        portals)
    World.default.World.portals

let () =
  Alcotest.run "World"
    [
      ( "links",
        [
          case "resolve by name" links_resolve;
          case "crossing changes frame" crossing_changes_frame;
          case "a step into the neighbour is refused"
            a_step_into_the_neighbour_is_refused;
          case "invalid worlds are refused" invalid_worlds_are_refused;
        ] );
      ( "the default world",
        [
          case "is playable" the_default_world_is_playable;
          case "is connected" the_default_world_is_connected;
          case "is varied" the_default_world_is_varied;
          case "has no seams" the_default_world_has_no_seams;
        ] );
    ]
