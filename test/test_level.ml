open Camlcast
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
          let back =
            link Level.default ~room:portal.to_room ~index:portal.twin
          in
          Alcotest.(check int)
            "the twin leads back here" room back.World.to_room;
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
  (* The cellar doorway carries an oak leaf. It stands open, so the level is
     walkable end to end; a shut one is the Doors demo's business. *)
  Alcotest.(check bool)
    "some threshold carries a door" true
    (Array.exists
       (Array.exists (fun p -> (Option.get p).World.threshold.Room.door <> None))
       Level.default.World.portals);
  Alcotest.(check bool)
    "and every door in the level stands open, so nothing is sealed off" true
    (Array.for_all
       (fun (r : Room.t) ->
         Array.for_all
           (fun (t : Room.threshold) -> not (Room.shut t))
           r.Room.thresholds)
       Level.default.World.rooms);
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

(* The hall's cellar door is the one solid leaf in the level, and a leaf has to
   be walked into to be gone through. Collision carries a step near any doorway
   into the room on the far side and asks again there, a doored one included, so
   this is what says that question has not made the one door in the level
   impassable — and that a step through it is checked against the room it arrives
   in rather than the one it left. *)
let the_cellar_door_can_be_walked_through () =
  let hall = 1 in
  let room = World.room Level.default hall in
  let door = room.Room.thresholds.(1) in
  Alcotest.(check string)
    "the second doorway of the hall" "cellar" door.Room.name;
  Alcotest.(check bool) "has a leaf hanging in it" true (door.Room.door <> None);
  (* A threshold is wound with the boundary it is cut into, so its normal points
     into the room that owns it — this one at the hall. A step along the normal
     therefore starts inside the hall, and one taken against it walks at the
     door. Standing there is asserted rather than assumed: were the sign the
     other way about, the walk below would begin in the cellar's own space while
     still calling itself the hall, and going through would mean walking back
     into the room it was already standing outside. *)
  let middle = Vec.scale (Vec.add door.Room.a door.Room.b) 0.5 in
  let inward = Vec.scale door.Room.normal 0.3 in
  let start =
    Player.create ~room:hall ~pos:(Vec.add middle inward)
      ~angle:(Float.atan2 (-.inward.Vec.y) (-.inward.Vec.x))
  in
  Alcotest.(check bool)
    "the near side of the door is somewhere you can stand" false
    (Room.blocked room start.Player.pos);
  let through =
    (Player.traverse Level.default start ~forward:0.6 ~strafe:0.).Player.player
  in
  Alcotest.(check int)
    "walking into the door goes through it"
    (link Level.default ~room:hall ~index:1).World.to_room through.Player.room;
  Alcotest.(check bool)
    "and not into the cellar's wall" false
    (Room.blocked
       (World.room Level.default through.Player.room)
       through.Player.pos);
  let back =
    (Player.traverse Level.default through ~forward:(-0.6) ~strafe:0.)
      .Player.player
  in
  Alcotest.(check int)
    "and it opens from the other side too" hall back.Player.room;
  Alcotest.check vec "landing where it set out from" start.Player.pos
    back.Player.pos

(* A step is resolved one axis at a time, and the leg that has not been taken yet
   is measured along the axes of the room the first leg may just have left. The
   plaza's doorways are cut into three different sides of its ring, so the links
   through them turn as well as move, and a leg carried through one has to turn
   with it.

   What says it did is that the whole step, carried back into the room it began
   in, is the plain L it would have been had there been no doorway there at all:
   a link is a rigid motion, and going through one cannot change the shape of a
   step. Leave the remaining leg in the old room's axes and it comes back bent by
   the angle of the link. *)
let a_diagonal_through_a_turning_doorway_keeps_its_shape () =
  let hall = 1 in
  let west = link Level.default ~room:hall ~index:0 in
  Alcotest.(check string)
    "the hall's own way out" "west" west.World.threshold.Room.name;
  Alcotest.(check bool)
    "through a link that turns and does not merely move" true
    (Float.abs west.World.onto.Transform.sin > 1e-6);
  (* Facing the doorway from a short step inside the hall, then a diagonal: the
     first leg crosses, the second is taken in the plaza. *)
  let start = Player.create ~room:hall ~pos:(Vec.make 0.3 0.) ~angle:Float.pi in
  let moved =
    (Player.traverse Level.default start ~forward:0.5 ~strafe:0.3).Player.player
  in
  Alcotest.(check int)
    "the step went through" west.World.to_room moved.Player.room;
  let delta =
    Vec.scale (Vec.make (-0.5) (-0.3)) (0.5 /. Vec.length (Vec.make 0.5 0.3))
  in
  Alcotest.check vec "and kept its shape across the seam"
    (Vec.add start.Player.pos delta)
    (Transform.point (Transform.inverse west.World.onto) moved.Player.pos)

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
      ("links", [ case "a portal knows its twin" a_portal_knows_its_twin ]);
      ( "the showcase level",
        [
          case "is playable" the_default_world_is_playable;
          case "is connected" the_default_world_is_connected;
          case "is varied" the_default_world_is_varied;
          case "has no seams" the_default_world_has_no_seams;
          case "the cellar door can be walked through"
            the_cellar_door_can_be_walked_through;
          case "a diagonal through a turning doorway keeps its shape"
            a_diagonal_through_a_turning_doorway_keeps_its_shape;
        ] );
    ]
