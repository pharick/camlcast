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
  let world = Lazy.force demo.Catalogue.world in
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

let is_consistent (demo : Catalogue.t) =
  World.check (Lazy.force demo.Catalogue.world)

(* A floor mismatch across a doorway is a visible step you walk into. Every one
   of these worlds either has flat floors on both sides or derives the second
   from the first with Plane.through, so the gap should be zero to the last bit
   in all of them. *)
let has_no_seams (demo : Catalogue.t) =
  let world = Lazy.force demo.Catalogue.world in
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
  let world = Lazy.force demo.Catalogue.world in
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

(* The trail demo builds a return route out of the crossings each frame reports,
   pushing one unless it undoes the one on top. Walking to the far end of the
   corridor and back again has to leave that route exactly as it was found —
   which is the whole point of a traversal trace, asserted over a few hundred
   frames of walking rather than a single step. *)
let the_trail_demo_unwinds_its_own_route () =
  let frame state forward =
    Trail.update state ~dt:(1. /. 60.)
      ~motion:{ Input.still with Input.forward }
      ~actions:Input.untouched
  in
  let walk state ~forward ~frames =
    List.fold_left (fun s _ -> frame s forward) state (List.init frames Fun.id)
  in
  Alcotest.(check int)
    "nothing behind you to begin with" 0
    (List.length Trail.start.Trail.stack);
  let out = walk Trail.start ~forward:0.15 ~frames:320 in
  Alcotest.(check int)
    "walking east reaches the far chamber" 4
    out.Trail.player.Player.room;
  Alcotest.(check int)
    "with four doorways on the way home" 4
    (List.length out.Trail.stack);
  (* Backwards down the same corridor, still facing the same way. *)
  let home = walk out ~forward:(-0.15) ~frames:320 in
  Alcotest.(check int) "and back where it started" 0
    home.Trail.player.Player.room;
  Alcotest.(check int)
    "with the route unwound to nothing" 0
    (List.length home.Trail.stack)

(* The loading demo is the one whose world is not a value in a source file: it
   is read off the disk when something forces it, through Asset and the two
   loaders. Forcing it at all is most of the test — a missing file, a path
   resolved against the wrong root or a picture that would not decode all raise
   here rather than returning a world.

   The rest is what a screenshot would show and a walkability check would not:
   that the pictures reached the room rather than merely parsing. Every number
   below is a property of a file in assets/, so a world quietly built from the
   generated art instead would fail on all of them. *)
let the_loading_demo_reads_its_art () =
  let world = Lazy.force Loading.world in
  let room = World.room world 0 in
  let pattern (w : Room.wall) = w.Room.material.Material.pattern in
  let sized n =
    Array.exists (fun w -> (pattern w).Texture.size = n) room.Room.walls
  in
  Alcotest.(check bool)
    "a 128-texel pattern came off the disk" true (sized 128);
  Alcotest.(check bool)
    "beside a 64-texel generated one" true (sized Texture.size);
  (* grille.png has square holes cut out of it, and nothing told Material so:
     the alpha came out of the file and Material.opaque read it. *)
  Alcotest.(check bool)
    "and a loaded pattern carries the alpha it was drawn with" true
    (Array.exists
       (fun (w : Room.wall) ->
         (pattern w).Texture.size = 64
         && (not (Material.opaque w.Room.material))
         && w.Room.height = 2.6)
       room.Room.walls);
  let decals =
    Array.to_list room.Room.walls |> List.concat_map (fun w -> w.Room.decals)
  in
  Alcotest.(check bool)
    "the poster is 96 x 64, and stayed that shape" true
    (List.exists
       (fun (d : Room.decal) ->
         d.Room.image.Image.width = 96 && d.Room.image.Image.height = 64)
       decals);
  Alcotest.(check bool)
    "and the loaded sprite kept its shape too" true
    (List.exists
       (fun (s : Room.sprite) ->
         s.Room.image.Image.width = 96 && s.Room.image.Image.height = 96)
       (Array.to_list room.Room.sprites))

(* The floating demo is where a sprite leaves the floor and a billboard stops
   being square. Both are properties of the world it is built from, so both are
   asserted here rather than by looking at it. *)
let the_floating_demo_lifts_its_sprites () =
  let world = Floating.world in
  let sprites =
    Array.to_list (World.room world 0).Room.sprites
    @ Array.to_list (World.room world 1).Room.sprites
  in
  Alcotest.(check bool)
    "some of them float" true
    (List.exists (fun (s : Room.sprite) -> s.Room.base > 0.) sprites);
  Alcotest.(check bool)
    "and some are still on the ground, as every other demo's are" true
    (List.exists (fun (s : Room.sprite) -> s.Room.base = 0.) sprites);
  (* Three times as wide as it is tall, and drawn that way: half a width is not
     half a size. *)
  Alcotest.(check bool)
    "one picture is not square" true
    (List.exists
       (fun (s : Room.sprite) ->
         s.Room.image.Image.width = 3 * s.Room.image.Image.height
         && Float.abs (Room.sprite_half_width s -. (1.5 *. s.Room.size)) < 1e-9)
       sprites);
  (* And the frames it animates through exist before anything is drawn. *)
  Alcotest.(check bool)
    "the frame strip has more than one frame in it" true
    (Array.length Pictures.motes > 1);
  Alcotest.(check bool)
    "and every frame is the same shape" true
    (Array.for_all
       (fun (i : Image.t) ->
         i.Image.width = Pictures.motes.(0).Image.width
         && i.Image.height = Pictures.motes.(0).Image.height)
       Pictures.motes)

(* The dust demo is the frames half at the scale a game wants it: seventy motes,
   every one of them somewhere else each frame. Two things have to be true of
   that, and neither is visible in a screenshot.

   The first is §15's rule, and it is asserted by {e physical} equality: every
   mote's picture must be one of the images Pictures built when it loaded, not
   an equal one made during the frame. A version that generated a picture per
   mote per frame would draw exactly the same thing and fail here.

   The second is what makes it cheap: a moving room shares the room it moved
   from. The walls and both planes have to be the very same values, or seventy
   motes would be dragging four walls behind them sixty times a second. *)
let the_dust_demo_moves_without_making_anything () =
  let at t = fst (Dust.view { Dust.elapsed = t; player = Player.spawn Dust.world }) in
  let early = at 0.4 and late = at 3.1 in
  let sprites world = Array.to_list (World.room world 0).Room.sprites in
  Alcotest.(check int) "every mote is there" 70 (List.length (sprites early));
  Alcotest.(check bool)
    "each one's picture came out of the precomputed strip" true
    (List.for_all
       (fun (s : Room.sprite) ->
         Array.exists (fun im -> im == s.Room.image) Pictures.motes)
       (sprites early @ sprites late));
  (* And they are genuinely moving: not one of them is where it was. *)
  Alcotest.(check bool)
    "all seventy have moved" true
    (List.for_all2
       (fun (a : Room.sprite) (b : Room.sprite) ->
         a.Room.base <> b.Room.base || a.Room.pos <> b.Room.pos)
       (sprites early) (sprites late));
  Alcotest.(check bool)
    "and they are spread through the room, not stacked at one height" true
    (List.exists (fun (s : Room.sprite) -> s.Room.base < 1.) (sprites early)
    && List.exists (fun (s : Room.sprite) -> s.Room.base > 3.) (sprites early));
  (* The geometry is shared, not rebuilt. *)
  let authored = World.room Dust.world 0 and moved = World.room late 0 in
  Alcotest.(check bool)
    "the walls are the very same array" true
    (moved.Room.walls == authored.Room.walls);
  Alcotest.(check bool)
    "and both planes the very same values" true
    (moved.Room.floor == authored.Room.floor
    && moved.Room.ceiling == authored.Room.ceiling)

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
          case "the trail demo unwinds its own route"
            the_trail_demo_unwinds_its_own_route;
          case "the loading demo reads its art" the_loading_demo_reads_its_art;
          case "the floating demo lifts its sprites"
            the_floating_demo_lifts_its_sprites;
          case "the dust demo moves without making anything"
            the_dust_demo_moves_without_making_anything;
        ] );
    ]
