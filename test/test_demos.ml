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
  let names =
    List.map (fun (d : Catalogue.t) -> d.Catalogue.name) Catalogue.demos
  in
  Alcotest.(check int)
    "no two demos answer to the same name" (List.length names)
    (List.length (List.sort_uniq String.compare names));
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " can be found by name")
        true
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
    "walking east reaches the far chamber" 4 out.Trail.player.Player.room;
  Alcotest.(check int)
    "with four doorways on the way home" 4
    (List.length out.Trail.stack);
  (* Backwards down the same corridor, still facing the same way. *)
  let home = walk out ~forward:(-0.15) ~frames:320 in
  Alcotest.(check int)
    "and back where it started" 0 home.Trail.player.Player.room;
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
  Alcotest.(check bool) "a 128-texel pattern came off the disk" true (sized 128);
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
  let at t =
    fst (Dust.view { Dust.elapsed = t; player = Player.spawn Dust.world })
  in
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

(* The chalk demo, driven the way a player drives it: stand somewhere, aim, and
   call the same {!Chalk.place} the C key calls. Every claim below is one §13.5
   asks for, and none of them is visible in a screenshot.

   The partition across the hall runs from (-1.5, 1) to (2.5, 1) and is the one
   wall here with two faces you can stand at, so it is what the side cases use.
   Facing it from the south is looking north, at +y, and standing a cell and a
   half back — inside {!Chalk.reach}, since a wall further off than that is
   named but not markable. *)
let facing_the_partition ?(from = Vec.make 0.5 (-0.5)) (state : Chalk.t) =
  {
    state with
    Chalk.player = Player.create ~room:0 ~pos:from ~angle:(Float.pi /. 2.);
  }

let decal_under (state : Chalk.t) =
  match Sight.cast (Chalk.dressed state) state.Chalk.player with
  | Some { Sight.kind = Sight.Wall w; _ } -> w.decal
  | _ -> None

let the_chalk_demo_marks_what_the_crosshair_is_on () =
  let aimed = facing_the_partition Chalk.start in
  Alcotest.(check (option int))
    "bare wall to start with" None (decal_under aimed);
  let marked = Chalk.place aimed in
  Alcotest.(check int) "one mark placed" 1 (List.length marked.Chalk.marks);
  Alcotest.(check int) "and a stroke spent" 7 marked.Chalk.left;
  (* Placement coordinates: the mark is where the crosshair was, so aiming again
     from the same spot finds it. *)
  Alcotest.(check (option int))
    "and it is under the crosshair" (Some 0) (decal_under marked);
  (* Side specificity: from the far side of the same partition there is nothing
     on it. Standing north of it, looking south. *)
  let behind =
    {
      marked with
      Chalk.player =
        Player.create ~room:0 ~pos:(Vec.make 0.5 2.5) ~angle:(-.Float.pi /. 2.);
    }
  in
  Alcotest.(check (option int))
    "and nothing on its back" None (decal_under behind);
  Alcotest.(check bool)
    "though the partition is still what is being looked at" true
    (match Sight.cast (Chalk.dressed behind) behind.Chalk.player with
    | Some { Sight.kind = Sight.Wall _; _ } -> true
    | _ -> false)

(* Persistence. The lamp rebuilds the hall from its parts — every wall, both
   planes, all new values — and the marks are still on it, because the demo
   keeps them and puts them back. *)
let the_chalk_demo_keeps_its_marks_through_a_rebuild () =
  let marked = Chalk.place (facing_the_partition Chalk.start) in
  let later = { marked with Chalk.elapsed = 4.5 } in
  Alcotest.(check bool)
    "the lamp really did change" true
    (Float.abs (Chalk.lamp 0. -. Chalk.lamp 4.5) > 0.2);
  Alcotest.(check bool)
    "so not one wall of the room is the value it was" true
    (Array.for_all2
       (fun a b -> a != b)
       (World.room (Chalk.dressed later) 0).Room.walls
       (World.room (Chalk.dressed marked) 0).Room.walls);
  (* The lamp is two things. The materials are what force the rebuild above; the
     air is what reaches the chalk, since a decal is fogged like the wall it is
     on. A lamp that moved only the first would leave the marks bright in the
     dark. *)
  Alcotest.(check bool)
    "and the air closed in with it" true
    ((Chalk.dressed later).World.atmosphere.Atmosphere.fog_distance
   < (Chalk.dressed marked).World.atmosphere.Atmosphere.fog_distance);
  Alcotest.(check (option int))
    "and the mark is still there" (Some 0) (decal_under later)

(* Chalk-capacity rejection, and the through-a-doorway rule. Both are this
   demo's, not the engine's, and both are one line of {!Chalk.markable}. *)
let the_chalk_demo_runs_out_of_chalk () =
  (* Eight strokes, each at a slightly different spot along the partition. *)
  let spend state k =
    Chalk.place
      (facing_the_partition
         ~from:(Vec.make (-1. +. (float_of_int k *. 0.4)) (-0.5))
         state)
  in
  let spent = List.fold_left spend Chalk.start (List.init 8 Fun.id) in
  Alcotest.(check int) "eight marks placed" 8 (List.length spent.Chalk.marks);
  Alcotest.(check int) "and no chalk left" 0 spent.Chalk.left;
  let ninth = spend spent 8 in
  Alcotest.(check int) "the ninth is refused" 8 (List.length ninth.Chalk.marks);
  Alcotest.(check int) "and costs nothing" 0 ninth.Chalk.left;
  let why state =
    Chalk.refusal state (Sight.cast (Chalk.dressed state) state.Chalk.player)
  in
  Alcotest.(check (option string))
    "and it says so" (Some "no chalk left")
    (why (facing_the_partition ninth));
  (* A wall in the room through the doorway is named but not markable. Pitched
     up over the figure standing in there, or the crosshair finds that instead
     and a sprite is not something this demo has a word about. *)
  let through =
    {
      Chalk.start with
      Chalk.player =
        Player.pitch_by
          (Player.create ~room:0 ~pos:(Vec.make 3. 0.) ~angle:0.)
          ~delta:0.3;
    }
  in
  Alcotest.(check bool)
    "the eye does reach a wall of the next room" true
    (match Sight.cast (Chalk.dressed through) through.Chalk.player with
    | Some { Sight.kind = Sight.Wall _; crossed; _ } -> crossed > 0
    | _ -> false);
  Alcotest.(check (option string))
    "and that is why it cannot be chalked" (Some "another room") (why through);
  (* The same partition from too far back: named, refused, and the refusal says
     the one thing the player can act on. *)
  let far = facing_the_partition ~from:(Vec.make 0.5 (-4.)) Chalk.start in
  Alcotest.(check bool)
    "a wall out of reach is still seen" true
    (match Sight.cast (Chalk.dressed far) far.Chalk.player with
    | Some { Sight.kind = Sight.Wall _; distance; _ } -> distance > Chalk.reach
    | _ -> false);
  Alcotest.(check (option string)) "but refused" (Some "too far") (why far);
  Alcotest.(check int)
    "and pressing the key does nothing" 0
    (List.length (Chalk.place far).Chalk.marks);
  (* Walk up to it and the same wall takes a mark. *)
  Alcotest.(check int)
    "until you walk up to it" 1
    (List.length (Chalk.place (facing_the_partition Chalk.start)).Chalk.marks)

(* The demo's two chalks are meant to behave differently as the lamp goes down:
   the arrow is paint and the cross glows. That is a property of the numbers in
   the symbol table, and a table where both were the same would still draw two
   distinguishable marks and pass every other test here. *)
let the_chalk_demo_has_one_glowing_symbol_and_one_not () =
  let glow_of i =
    let _, _, g = Chalk.symbols.(i) in
    g
  in
  Alcotest.(check (float 1e-9)) "the arrow is plain chalk" 0. (glow_of 0);
  Alcotest.(check bool) "the cross makes its own light" true (glow_of 1 > 0.);
  Alcotest.(check bool)
    "and not so much that it stops dimming at all" true
    (glow_of 1 < 1.);
  (* The lamp is the atmosphere and only the atmosphere: nothing about the room
     it lights depends on it. Two very different brightnesses, same materials. *)
  let hall_at t =
    (World.room (Chalk.dressed { Chalk.start with Chalk.elapsed = t }) 0)
      .Room.walls.(2)
      .Room.material
  in
  Alcotest.(check bool)
    "so the walls are lit the same at any lamp" true
    (hall_at 0. = hall_at (Chalk.lamp_period /. 2.))

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
          case "the chalk demo marks what the crosshair is on"
            the_chalk_demo_marks_what_the_crosshair_is_on;
          case "the chalk demo keeps its marks through a rebuild"
            the_chalk_demo_keeps_its_marks_through_a_rebuild;
          case "the chalk demo runs out of chalk"
            the_chalk_demo_runs_out_of_chalk;
          case "the chalk demo has one glowing symbol and one not"
            the_chalk_demo_has_one_glowing_symbol_and_one_not;
        ] );
    ]
