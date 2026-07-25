open Raycaster
open Camlcast_demo
open Support

let link = portal
let ceiling_is_open (r : Room.t) =
  match r.Room.ceiling with Room.Open _ -> true | Room.Roof _ -> false

(* Stepping through a doorway lands you behind the neighbour's own copy of it,
   so anything that then looks or walks forward meets that opening again
   immediately. [twin] is how it knows which one not to go back through; if it
   pointed anywhere else the renderer would bounce between the two rooms until
   it ran out of budget, and the doorway would fill with haze. *)
let a_portal_knows_its_twin () =
  Array.iteri
    (fun room portals ->
      Array.iter
        (fun portal ->
          let portal : World.portal = Option.get portal in
          let back = link Level.default ~room:portal.to_room ~index:portal.twin in
          Alcotest.(check int) "the twin leads back here" room back.World.to_room;
          Alcotest.(check int)
            "and its own twin is where we started" portal.World.twin
            (link Level.default ~room ~index:back.World.twin).World.twin;
          (* The twin really is the same doorway seen from the other side: this
             room's opening maps exactly onto it. *)
          Alcotest.check vec "endpoints meet, reversed"
            back.World.threshold.Room.b
            (Transform.point portal.onto portal.threshold.Room.a);
          Alcotest.check vec "and the other way about"
            back.World.threshold.Room.a
            (Transform.point portal.onto portal.threshold.Room.b))
        portals)
    Level.default.World.portals

(* The showcase level is the only thing that exercises all of this at once, so
   it is checked for the properties the rest of the engine assumes of it. *)
let the_default_world_is_playable () =
  let spawn = Level.default.World.spawn in
  Alcotest.(check bool)
    "the spawn point is not inside a wall" false
    (Room.blocked (World.room Level.default spawn.room) spawn.pos);
  (* Every room encloses itself. Split into rooms, each one has to close its own
     boundary or its floor and sky run to the horizon through the gap. *)
  Array.iteri
    (fun i (r : Room.t) ->
      Alcotest.(check bool)
        (Level.default.World.names.(i) ^ " is walled all round")
        true
        (Array.length r.Room.walls >= 4))
    Level.default.World.rooms;
  Alcotest.(check int)
    "the plaza's ring, pillars and furniture" 42
    (Array.length (World.room Level.default 0).Room.walls)

let the_default_world_is_connected () =
  let seen = Array.make (Array.length Level.default.World.rooms) false in
  let rec visit i =
    if not seen.(i) then begin
      seen.(i) <- true;
      Array.iter
        (fun p -> visit (Option.get p).World.to_room)
        (World.portals Level.default i)
    end
  in
  visit Level.default.World.spawn.room;
  Alcotest.(check bool)
    "every room is reachable from the spawn" true
    (Array.for_all Fun.id seen)

let the_default_world_is_varied () =
  let rooms = Level.default.World.rooms in
  Alcotest.(check bool)
    "some room is open to the sky" true
    (Array.exists (fun (r : Room.t) -> ceiling_is_open r) rooms);
  Alcotest.(check bool)
    "some room is roofed" true
    (Array.exists (fun (r : Room.t) -> not (ceiling_is_open r)) rooms);
  Alcotest.(check bool)
    "some threshold is a solid door" true
    (Array.exists
       (Array.exists (fun p ->
            (Option.get p).World.threshold.Room.door <> None))
       Level.default.World.portals);
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
       (Array.exists (fun p ->
            Float.abs (Option.get p).World.onto.Transform.sin > 1e-6))
       Level.default.World.portals);
  Alcotest.(check bool)
    "some room's floor is inclined" true
    (Array.exists
       (fun (r : Room.t) ->
         Plane.elevation r.Room.floor.Room.plane (Vec.make 0. 0.)
         <> Plane.elevation r.Room.floor.Room.plane (Vec.make 1. 0.))
       rooms)

(* A floor mismatch across a doorway is a visible step you walk into, so every
   room's floor is built from its neighbour's with Plane.through and the gap
   should be zero to the last bit. *)
let the_default_world_has_no_seams () =
  Array.iteri
    (fun room portals ->
      Array.iter
        (fun portal ->
          let portal : World.portal = Option.get portal in
          Alcotest.check close
            (Level.default.World.names.(room)
            ^ "." ^ portal.World.threshold.Room.name)
            0.
            (World.seam_gap Level.default ~room portal))
        portals)
    Level.default.World.portals

let () =
  Alcotest.run "Level"
    [
      ( "links",
        [ case "a portal knows its twin" a_portal_knows_its_twin ] );
      ( "the showcase level",
        [
          case "is playable" the_default_world_is_playable;
          case "is connected" the_default_world_is_connected;
          case "is varied" the_default_world_is_varied;
          case "has no seams" the_default_world_has_no_seams;
        ] );
    ]
