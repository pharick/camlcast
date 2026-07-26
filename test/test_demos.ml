(** The demos are content, but they are content the engine's invariants are
    asserted against: a world that does not close its rooms or match its floors
    across a threshold renders wrongly rather than failing, and nobody notices
    until they walk into it.

    Every world in the {!Camlcast_demo.Catalogue} is checked here, so adding a
    demo to that list is also adding it to this suite. The showcase level has
    its own suite besides this one, for the things only it does. *)

open Raycaster
open Camlcast_demo
open Support

let each name check =
  List.map
    (fun (demo : Catalogue.t) ->
      case (demo.Catalogue.name ^ ": " ^ name) (fun () -> check demo))
    Catalogue.demos

(* World.make raises on a world it cannot join up, and these values are built
   when the module is loaded, so merely reaching this suite has already run
   every one of them. What is checked here is what make does not: that the
   result is somewhere you can stand and look. *)
let is_walkable (demo : Catalogue.t) =
  let world = demo.Catalogue.world in
  let spawn = world.World.spawn in
  Alcotest.(check bool)
    "the spawn point is not inside a wall" false
    (Room.blocked (World.room world spawn.World.room) spawn.World.pos);
  Array.iteri
    (fun i (room : Room.t) ->
      (* A room that does not close its own boundary leaks its floor and sky to
         the horizon through the gap. *)
      Alcotest.(check bool)
        (world.World.names.(i) ^ " is walled all round")
        true
        (Array.length room.Room.walls >= 3))
    world.World.rooms

let is_consistent (demo : Catalogue.t) = World.check demo.Catalogue.world

(* A floor mismatch across a doorway is a visible step you walk into. Every one
   of these worlds either has flat floors on both sides or derives the second
   from the first with Plane.through, so the gap should be zero to the last bit
   in all of them. *)
let has_no_seams (demo : Catalogue.t) =
  let world = demo.Catalogue.world in
  Array.iteri
    (fun room portals ->
      Array.iter
        (fun portal ->
          let portal : World.portal = Option.get portal in
          Alcotest.check close
            (world.World.names.(room) ^ "." ^ portal.World.threshold.Room.name)
            0.
            (World.seam_gap world ~room portal))
        portals)
    world.World.portals

(* Every room is reachable from the spawn, so nothing in a demo is content
   nobody can get to. *)
let is_connected (demo : Catalogue.t) =
  let world = demo.Catalogue.world in
  let seen = Array.make (Array.length world.World.rooms) false in
  let rec visit i =
    if not seen.(i) then begin
      seen.(i) <- true;
      Array.iter
        (fun portal -> visit (Option.get portal).World.to_room)
        (World.portals world i)
    end
  in
  visit world.World.spawn.World.room;
  Alcotest.(check bool)
    "every room is reachable from the spawn" true
    (Array.for_all Fun.id seen)

let names_are_distinct () =
  let names = List.map (fun (d : Catalogue.t) -> d.Catalogue.name) Catalogue.demos in
  Alcotest.(check int)
    "no two demos answer to the same name" (List.length names)
    (List.length (List.sort_uniq String.compare names));
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " can be found by name") true
        (Catalogue.find name <> None))
    names;
  Alcotest.(check bool)
    "and nothing else can" true
    (Catalogue.find "no-such-demo" = None)

(* Growing is where a world is most easily broken, because open_doorway and
   link are checking invariants the generator has to keep by hand. Walk the
   corridor by growing from the far end over and over, and the result has to
   still be a world every time. *)
let growing_leaves_a_world_that_still_works () =
  let world = ref Endless.world in
  let rooms () = Array.length !world.World.rooms in
  let before = rooms () in
  for _ = 1 to 8 do
    let far = rooms () - 1 in
    world :=
      Endless.grow !world
        (Player.create ~room:far ~pos:(Vec.make 0. 2.) ~angle:0.);
    World.check !world;
    Array.iteri
      (fun room portals ->
        Array.iter
          (fun portal ->
            let portal : World.portal = Option.get portal in
            Alcotest.check close "no seam appeared" 0.
              (World.seam_gap !world ~room portal))
          portals)
      !world.World.portals
  done;
  Alcotest.(check bool)
    "the corridor is longer than it was" true
    (rooms () > before);
  (* Every room the player has walked through has a way back and a way on, and
     no threshold anywhere is left leading nowhere. *)
  Array.iteri
    (fun room portals ->
      Array.iteri
        (fun index portal ->
          Alcotest.(check bool)
            (Printf.sprintf "room %d threshold %d leads somewhere" room index)
            true (portal <> None))
        portals)
    !world.World.portals

let () =
  Alcotest.run "Demos"
    [
      ("walkable", each "is walkable" is_walkable);
      ("consistent", each "is consistent" is_consistent);
      ("seams", each "has no seams" has_no_seams);
      ("connected", each "is connected" is_connected);
      ( "the catalogue",
        [
          case "names are distinct" names_are_distinct;
          case "growing leaves a world that still works"
            growing_leaves_a_world_that_still_works;
        ] );
    ]
