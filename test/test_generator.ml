open Raycaster
open House
open Support

(* Walk the house the way a player exploring it would: through a doorway, and
   never straight back out the one just come in by unless it is the only one.
   The generator catches up after each step, exactly where the engine would ask
   it to, so nothing here can accidentally build the whole house up front and
   hide a bug that only shows at the edge.

   Pushing on rather than pacing is the case that matters. A walk that turns
   back half the time never leaves the first few rooms, and would pass whatever
   it was pointed at. *)
let wander ~seed ~steps =
  let rng = Rng.make (seed * 31) in
  let house = ref (Generator.start ~seed) in
  let player = ref (Player.spawn !house.Generator.world) in
  house := Generator.horizon !house !player;
  let path = ref [ !player.Player.room ] in
  let came_in_by = ref (-1) in
  for _ = 1 to steps do
    let all =
      Array.to_list (World.portals !house.Generator.world !player.Player.room)
    in
    let onward =
      List.filteri (fun i p -> i <> !came_in_by && Option.is_some p) all
      |> List.filter_map Fun.id
    in
    match
      if onward = [] then List.filter_map Fun.id all else onward
    with
    | [] -> ()
    | list ->
        let portal = Rng.pick rng (Array.of_list list) in
        (* Straight through the middle of the opening, which is what the
           threshold's own endpoints average to. *)
        let middle =
          Vec.scale
            (Vec.add portal.World.threshold.Room.a portal.World.threshold.Room.b)
            0.5
        in
        came_in_by := portal.World.twin;
        player :=
          Player.through portal.World.onto ~room:portal.World.to_room
            { !player with Player.pos = middle };
        house := Generator.horizon !house !player;
        path := !player.Player.room :: !path
  done;
  (!house, List.rev !path)

let rooms house = Array.length house.Generator.world.World.rooms

(* How many seeds the aggregate checks below run over. One house is a sample of
   one: the room counts a forty-step walk produces range from about ten to about
   a hundred, so anything asserted against a single seed is really asserting
   that seed. What every seed must satisfy is checked per seed; what the
   generator should do on average is checked on the average. *)
let spread = List.init 24 (fun i -> (i * 7717) + 11)

let median l =
  let a = Array.of_list (List.sort compare l) in
  a.(Array.length a / 2)

(* Everything World.make guarantees about a level someone wrote down, asserted
   over two dozen that built themselves: every doorway named once, every doorway
   linked, every twin the same doorway from the other side. *)
let the_house_holds_together () =
  List.iter
    (fun seed ->
      let house, _ = wander ~seed ~steps:40 in
      World.check house.Generator.world;
      Alcotest.(check bool)
        (Printf.sprintf "seed %d built a house (%d rooms)" seed (rooms house))
        true
        (rooms house > 5))
    spread

(* The walk has to keep arriving somewhere rather than pacing a pocket. Loops
   and dead ends are supposed to send you back over ground you have covered, so
   this is nowhere near forty; what it is watching for is the walk circling four
   or five rooms forever, which is what a sealed house looks like from inside. *)
let walking_it_keeps_finding_rooms () =
  let counts =
    List.map
      (fun seed ->
        let _, path = wander ~seed ~steps:40 in
        List.length (List.sort_uniq compare path))
      spread
  in
  Alcotest.(check bool)
    (Printf.sprintf "the median walk of forty found %d different rooms"
       (median counts))
    true
    (median counts >= 15);
  Alcotest.(check bool)
    (Printf.sprintf "and the worst of them found %d"
       (List.fold_left Int.min 999 counts))
    true
    (List.fold_left Int.min 999 counts >= 6)

(* A run is a value, not an event. Without this nothing else here could be
   asserted at all, and a bug found by walking into it could not be walked into
   again. *)
let the_same_seed_builds_the_same_house () =
  let a, path_a = wander ~seed:4242 ~steps:30 in
  let b, path_b = wander ~seed:4242 ~steps:30 in
  Alcotest.(check (list int)) "the same walk" path_a path_b;
  Alcotest.(check int) "the same room count" (rooms a) (rooms b);
  Alcotest.(check (list string))
    "the same rooms in the same order"
    (Array.to_list a.Generator.world.World.names)
    (Array.to_list b.Generator.world.World.names);
  let c, path_c = wander ~seed:4243 ~steps:30 in
  Alcotest.(check bool)
    "and a different seed does not" true
    (path_a <> path_c || rooms a <> rooms c)

(* The renderer looks three doorways deep, so every room within three of the
   player has to be finished — every wall that will ever be a doorway already
   is one. A room finished one step late is a wall the player watches turn into
   a door as they walk towards it. *)
let the_horizon_is_finished () =
  let house, _ = wander ~seed:31337 ~steps:25 in
  let world = house.Generator.world in
  let player_room = List.hd (List.rev (snd (wander ~seed:31337 ~steps:25))) in
  let seen = Hashtbl.create 64 in
  let rec sweep layer depth =
    if depth > Config.max_portal_depth then ()
    else
      match List.filter (fun i -> not (Hashtbl.mem seen i)) layer with
      | [] -> ()
      | layer ->
          List.iter (fun i -> Hashtbl.replace seen i ()) layer;
          List.iter
            (fun room ->
              Array.iteri
                (fun index -> function
                  | Some _ -> ()
                  | None ->
                      Alcotest.failf
                        "room %s is %d doorways away and its %s leads nowhere"
                        world.World.names.(room) depth
                        (World.room world room).Room.thresholds.(index).Room.name)
                (World.portals world room))
            layer;
          sweep
            (List.concat_map
               (fun i ->
                 Array.to_list (World.portals world i)
                 |> List.filter_map
                      (Option.map (fun (p : World.portal) -> p.World.to_room)))
               layer)
            (depth + 1)
  in
  sweep [ player_room ] 0

(* Every floor in the house is level, so the two rooms either side of a doorway
   agree about where the floor is exactly rather than nearly. This is not a
   detail: the elevation around a loop need not come back to itself, so a house
   that closes loops between rooms reached by different routes could not have
   sloped floors without a step appearing in some doorway. *)
let no_doorway_has_a_step_in_it () =
  let house, _ = wander ~seed:5150 ~steps:30 in
  let world = house.Generator.world in
  Array.iteri
    (fun room portals ->
      Array.iter
        (function
          | None -> ()
          | Some portal ->
              Alcotest.(check (float 0.))
                (world.World.names.(room) ^ "."
               ^ portal.World.threshold.Room.name)
                0.
                (World.seam_gap world ~room portal))
        portals)
    world.World.portals

(* Loops are the point. A doorway that leads back into the house produces a
   route that contradicts the one that got there, and there is no global frame
   for that to be wrong against. If the chance were somehow zero the house would
   be a tree and every walk would be reversible, which is a different game. *)
let the_house_closes_loops () =
  let house, _ = wander ~seed:2718 ~steps:60 in
  let world = house.Generator.world in
  (* A loop shows up as a room reachable from another by two different
     doorways, or from itself. *)
  let loops = ref 0 in
  Array.iteri
    (fun room portals ->
      let seen = Hashtbl.create 8 in
      Array.iter
        (function
          | None -> ()
          | Some (p : World.portal) ->
              if p.World.to_room = room || Hashtbl.mem seen p.World.to_room then
                incr loops;
              Hashtbl.replace seen p.World.to_room ())
        portals)
    world.World.portals;
  Alcotest.(check bool)
    (Printf.sprintf "sixty rooms in, something has doubled back (%d)" !loops)
    true (!loops > 0)

(* A closed door is a room you have to walk into to find out about: you cannot
   see through a leaf, only through an opening. Both sides of a doorway have to
   agree about whether it has one, or it would be a door from one room and an
   opening from the other. *)
let the_house_hangs_doors () =
  let house, _ = wander ~seed:6006 ~steps:40 in
  let world = house.Generator.world in
  let shut = ref 0 and total = ref 0 in
  Array.iter
    (fun (r : Room.t) ->
      Array.iter
        (fun (t : Room.threshold) ->
          incr total;
          if t.Room.door <> None then incr shut)
        r.Room.thresholds)
    world.World.rooms;
  Alcotest.(check bool)
    (Printf.sprintf "%d of %d doorways are shut" !shut !total)
    true
    (!shut > 0 && !shut * 2 < !total);
  (* The two sides of every doorway agree. *)
  Array.iteri
    (fun room portals ->
      Array.iteri
        (fun index -> function
          | None -> ()
          | Some (p : World.portal) ->
              let back =
                (World.room world p.World.to_room).Room.thresholds.(p.World.twin)
              in
              Alcotest.(check bool)
                (Printf.sprintf "%s.%s is the same from both sides"
                   world.World.names.(room)
                   (World.room world room).Room.thresholds.(index).Room.name)
                true
                (Option.is_some back.Room.door
                = Option.is_some p.World.threshold.Room.door))
        portals)
    world.World.portals

(* The house is infinite only for as long as some wall in it could still become
   a doorway, and that can run out: a loop spends two openings and returns none,
   a dead end spends the one it was entered by. Left to chance it runs out
   almost immediately — with two openings in the world, one loop joins them to
   each other and the house is sealed at two rooms.

   So the generator counts them and refuses to spend the last. This walks a
   long way on a lot of seeds to say that the count never gets near zero, and
   that the house is still growing at the end of it. *)
let the_house_never_seals_itself () =
  List.iter
    (fun seed ->
      let house, _ = wander ~seed ~steps:60 in
      Alcotest.(check bool)
        (Printf.sprintf "seed %d still has %d walls to open" seed
           house.Generator.frontier)
        true
        (house.Generator.frontier >= 1);
      (* The running count has to be the truth, since every decision the
         generator makes about looping is taken against it. *)
      let counted =
        List.length
          (List.concat_map
             (fun room -> Generator.unopened house room)
             (List.init (rooms house) Fun.id))
      in
      Alcotest.(check int)
        "the running count agrees with the house it describes" counted
        house.Generator.frontier;
      (* Meanwhile every doorway that has been cut leads somewhere: the frontier
         is walls still standing, not holes left open. *)
      Alcotest.(check int)
        "and no doorway was left leading nowhere" 0
        (Array.to_list house.Generator.world.World.portals
        |> List.concat_map Array.to_list
        |> List.filter Option.is_none |> List.length))
    spread

(* The one way this generator could run away in the other direction. Every
   prototype but the closet offers more ways on than it has ways in, so if the
   weights drifted the house would branch faster than anyone could walk it. *)
let the_house_does_not_run_away () =
  let counts = List.map (fun seed -> rooms (fst (wander ~seed ~steps:40))) spread in
  let per_step = float_of_int (median counts) /. 40. in
  Alcotest.(check bool)
    (Printf.sprintf "%.1f rooms built per room walked, at the median" per_step)
    true
    (per_step > 0.5 && per_step < 6.);
  Alcotest.(check bool)
    (Printf.sprintf "and %.1f at the worst"
       (float_of_int (List.fold_left Int.max 0 counts) /. 40.))
    true
    (List.fold_left Int.max 0 counts < 40 * 8)

(* The player has to be able to stand where they start and walk out of it. *)
let the_spawn_is_walkable () =
  List.iter
    (fun seed ->
      let house = Generator.start ~seed in
      let world = house.Generator.world in
      let spawn = world.World.spawn in
      Alcotest.(check bool)
        (Printf.sprintf "seed %d spawns clear of everything" seed)
        false
        (Room.blocked (World.room world spawn.World.room) spawn.World.pos))
    (List.init 12 (fun i -> (i * 977) + 3))

(* Nothing inside a room may stand in a doorway. A fitting that did would wall
   the player into the room it was in, and with the house generated ahead of
   them there would be no way to tell that had happened until they were stuck. *)
let nothing_stands_in_a_doorway () =
  let house, _ = wander ~seed:8675309 ~steps:40 in
  let world = house.Generator.world in
  Array.iteri
    (fun room (r : Room.t) ->
      Array.iter
        (fun (t : Room.threshold) ->
          (* Step straight through the middle of the opening, from a little
             inside to a little outside. *)
          let middle = Vec.scale (Vec.add t.Room.a t.Room.b) 0.5 in
          let out = Vec.scale t.Room.normal 0.6 in
          let from = Vec.sub middle out and dest = Vec.add middle out in
          Alcotest.(check bool)
            (Printf.sprintf "%s.%s is clear" world.World.names.(room)
               t.Room.name)
            true
            (Room.can_step r ~from ~dest))
        r.Room.thresholds)
    world.World.rooms

let () =
  Alcotest.run "Generator"
    [
      ( "the house",
        [
          case "holds together" the_house_holds_together;
          case "the same seed builds the same house"
            the_same_seed_builds_the_same_house;
          case "the horizon is finished" the_horizon_is_finished;
          case "no doorway has a step in it" no_doorway_has_a_step_in_it;
          case "closes loops" the_house_closes_loops;
          case "hangs doors on some of it" the_house_hangs_doors;
          case "never seals itself" the_house_never_seals_itself;
          case "does not run away" the_house_does_not_run_away;
        ] );
      ( "walking it",
        [
          case "keeps finding rooms" walking_it_keeps_finding_rooms;
          case "the spawn is walkable" the_spawn_is_walkable;
          case "nothing stands in a doorway" nothing_stands_in_a_doorway;
        ] );
    ]
