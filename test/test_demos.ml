(** The demos are content, but they are content the engine's invariants are
    asserted against: a world that does not close its rooms or match its floors
    across a threshold renders wrongly rather than failing, and nobody notices
    until they walk into it.

    Every world in the {!Camlcast_demo.Catalogue} is checked here, so adding a
    demo to that list is also adding it to this suite. The showcase level has
    its own suite besides this one, for the things only it does. *)

open Camlcast_core
open Camlcast_demo
open Camlcast
open Support

let each name check =
  List.map
    (fun (demo : Catalogue.t) ->
      case (demo.Catalogue.name ^ ": " ^ name) (fun () -> check demo))
    Catalogue.demos

(* How many directions "all round" is asked in. A gap you could walk through is
   most of a cell wide, and these rooms are tens of cells across, so rays this
   far apart — under a degree — cannot fall either side of one. *)
let directions = 512

(* Somewhere inside a room to look around from.

   A threshold's normal faces into the room the threshold belongs to: that is the
   winding rule Room states and World.make enforces across a link. So half a cell
   in from the middle of any doorway is inside, clear of the jambs, and the spawn
   serves for a world with one room and no doorway at all. Whichever is found
   first is checked against Room.blocked before it is used, so a partition
   standing right behind a doorway cannot make the whole test lie. *)
let inside world i =
  let room = World.room world i in
  let spawn = World.spawn world in
  List.find_opt
    (fun p -> not (Room.blocked room p))
    ((if spawn.World.room = i then [ spawn.World.pos ] else [])
    @ List.map
        (fun t ->
          let threshold = Room.threshold_at room t in
          Vec.add
            (Vec.scale (Vec.add threshold.Room.a threshold.Room.b) 0.5)
            (Vec.scale threshold.Room.normal 0.5))
        (List.init (Room.threshold_count room) Fun.id))

(* The directions in which a room is not there. A boundary that closes is crossed
   by every ray leaving a point inside it, whether the ray meets a wall or the
   doorway cut into one; a ray that meets neither has left through a gap, and
   what it would have drawn is floor and sky all the way to the horizon.

   Asking it this way is what lets furniture alone. lib/check.mli explains that
   the geometry does not say which walls are a room's boundary and which are not,
   so a partition you can walk round the end of is indistinguishable from a wall
   left out — by its ends. It is not indistinguishable by this: a ray past the end
   of a partition goes on to meet the boundary behind it, and a ray through a gap
   in the boundary meets nothing. Five demos stand a free-standing partition in a
   room on purpose — glass, floating, chalk, loading and showcase — and counting
   loose ends calls every one of them a hole. This calls none of them one. *)
let escaping room ~origin =
  List.filter
    (fun angle ->
      let direction = Vec.of_angle angle in
      Ray.cast room ~origin ~direction = []
      && Ray.openings room ~origin ~direction = [])
    (List.init directions (fun k ->
         2. *. Float.pi *. float_of_int k /. float_of_int directions))

(* World.make raises on a world it cannot join up, and these values are built
   when the module is loaded, so merely reaching this suite has already run
   every one of them. What is checked here is what make does not: that the
   result is somewhere you can stand and look. *)
let is_walkable (demo : Catalogue.t) =
  let world = Lazy.force demo.Catalogue.world in
  let spawn = World.spawn world in
  Alcotest.(check bool)
    "the spawn point is not inside a wall" false
    (Room.blocked (World.room world spawn.World.room) spawn.World.pos);
  List.iteri
    (fun i (room : Room.t) ->
      match inside world i with
      | None ->
          Alcotest.fail (World.name world i ^ " has nowhere to stand in it")
      | Some origin ->
          let escapes = escaping room ~origin in
          Alcotest.(check int)
            (Printf.sprintf "%s is walled all round, looked at from (%g, %g)%s"
               (World.name world i) origin.Vec.x origin.Vec.y
               (match escapes with
               | [] -> ""
               | angle :: _ ->
                   Printf.sprintf " — the first gap bears %.1f degrees"
                     (angle *. 180. /. Float.pi)))
            0 (List.length escapes))
    (rooms world)

let is_consistent (demo : Catalogue.t) =
  World.check (Lazy.force demo.Catalogue.world)

(* A floor mismatch across a doorway is a visible step you walk into. Every one
   of these worlds either has flat floors on both sides or derives the second
   from the first with Plane.through, so the gap should be zero to the last bit
   in all of them. *)
let has_no_seams (demo : Catalogue.t) =
  let world = Lazy.force demo.Catalogue.world in
  List.iter
    (fun (room, _, p) ->
      let portal : World.portal = Option.get p in
      Alcotest.check close
        (World.name world room ^ "." ^ portal.World.threshold.Room.name)
        0.
        (World.seam_gap world ~room portal))
    (doorways world)

(* Every room is reachable from the spawn, so nothing in a demo is content
   nobody can get to. *)
let is_connected (demo : Catalogue.t) =
  let world = Lazy.force demo.Catalogue.world in
  let seen = Array.make (World.room_count world) false in
  let rec visit i =
    if not seen.(i) then begin
      seen.(i) <- true;
      for threshold = 0 to World.doorway_count world ~room:i - 1 do
        visit (Option.get (World.portal world ~room:i ~threshold)).World.to_room
      done
    end
  in
  visit (World.spawn world).World.room;
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

(* {!Catalogue.attempt} is the seam between the demos' two ways of failing, and
   the only part of the launcher reachable without a window. Above it,
   bin/demo.ml turns a [`Msg] into "camlcast-demo: " and an exit code of 1; below
   it the asset loaders raise, because a demo already inside a frame has nowhere
   to put a result. With nothing in between, a missing picture printed OCaml's
   own fatal-error banner and stopped with its own code — past both of this
   program's conventions at once. What matters is that the message survives the
   trip, since naming the file is the whole reason the loaders can afford to
   raise at all. *)
let a_demo_that_cannot_read_its_art_is_reported_and_not_a_crash () =
  let ran : (Engine.ending, [ `Msg of string ]) result =
    Catalogue.attempt (fun () -> Ok Engine.Left)
  in
  Alcotest.(check bool)
    "an ending is passed through untouched" true (ran = Ok Engine.Left);
  let refused : (Engine.ending, [ `Msg of string ]) result =
    Catalogue.attempt (fun () -> Error (`Msg "no window"))
  in
  Alcotest.(check bool)
    "and so is the error channel it already had" true
    (refused = Error (`Msg "no window"));
  let broken : (Engine.ending, [ `Msg of string ]) result =
    Catalogue.attempt (fun () ->
        Reading.or_raise "the loading demo could not read its art"
          (Error (`Msg "assets/tiles.png")))
  in
  (match broken with
  | Ok _ -> Alcotest.fail "a demo that could not read its art came back Ok"
  | Error (`Msg message) ->
      Alcotest.(check bool)
        "a raised failure arrives as a message that still names the file" true
        (mentions message "assets/tiles.png"));
  (* And nothing else. The seam catches one exception and it is the demos' own,
     so a mistake inside a frame — the [List.nth] that is one past the end, the
     [Option.get] of nothing — goes out as itself rather than arriving here
     dressed as a demo whose art could not be read. Reported that way it would
     be reported calmly, under a message about a file that was never the
     trouble, and the real mistake would appear nowhere in it. *)
  Alcotest.check_raises "a Failure from anywhere else is not ours"
    (Failure "index out of bounds") (fun () ->
      ignore (Catalogue.attempt (fun () -> failwith "index out of bounds")));
  Alcotest.check_raises "and neither is a refusal"
    (Invalid_argument "Room.doorway: the opening has to fit under the wall")
    (fun () ->
      ignore
        (Catalogue.attempt (fun () ->
             invalid_arg "Room.doorway: the opening has to fit under the wall")))

(* Growing is where a world was most easily broken, and the shape of the risk
   has changed. The old corridor grew by surgery — open_doorway to give a dead
   end a way on, add_room for what lay beyond it, link to join the two — and
   every one of those checks an invariant the generator had to keep by hand.

   A description grows by being longer. There is nothing to keep by hand, so
   what is asserted is not that the surgery was done right but that the result
   is a world: longer corridors, each of them checked, with no seam anywhere and
   no threshold left leading nowhere. *)
let growing_leaves_a_world_that_still_works () =
  let before = World.room_count Endless.world in
  let longest = ref before in
  for built = Endless.ahead to Endless.ahead + 8 do
    let world = (Mount.build (Endless.corridor ~built)).Scene.world in
    longest := World.room_count world;
    World.check world;
    List.iter
      (fun (room, _, p) ->
        let portal : World.portal = Option.get p in
        Alcotest.check close "no seam appeared" 0.
          (World.seam_gap world ~room portal))
      (doorways world);
    (* Every room has a way back and a way on, and no threshold anywhere is left
       leading nowhere. *)
    List.iter
      (fun (room, index, portal) ->
        Alcotest.(check bool)
          (Printf.sprintf "room %d threshold %d leads somewhere" room index)
          true (portal <> None))
      (doorways world)
  done;
  Alcotest.(check bool)
    "the corridor is longer than it was" true (!longest > before)

(* The trail demo builds a return route out of the crossings each frame reports,
   pushing one unless it undoes the one on top. Walking to the far end of the
   corridor and back again has to leave that route exactly as it was found —
   which is the whole point of a traversal trace, asserted over a few hundred
   frames of walking rather than a single step.

   The route is the component's own state and nothing outside it can read it, so
   what is counted here is what a player sees: one tick on the HUD per doorway
   between here and the way out, in the colour the demo draws a route in. That
   is a better thing to assert than the state anyway. *)
let the_trail_demo_unwinds_its_own_route () =
  let route = Color.rgb 235 200 110 in
  let ticks (scene : Scene.t) =
    List.length
      (List.filter
         (function Prim.Rect { color; _ } -> color = route | _ -> false)
         scene.Scene.hud)
  in
  let mount = Mount.create () in
  let player = ref None and crossings = ref [] in
  let frame forward =
    let scene =
      Mount.render mount
        (Element.provide Events.context
           { Events.still with Events.crossings = !crossings }
           [ Trail.unwinding () ])
    in
    let walking =
      match !player with
      | Some walking -> walking
      | None -> Player.spawn scene.Scene.world
    in
    let moved =
      Engine.move scene.Scene.world walking { Input.still with Input.forward }
    in
    player := Some moved.Player.player;
    crossings := Run.crossings_of scene moved;
    scene
  in
  let walk ~forward ~frames =
    let last = ref (frame forward) in
    for _ = 2 to frames do
      last := frame forward
    done;
    !last
  in
  Alcotest.(check int) "nothing behind you to begin with" 0 (ticks (frame 0.));
  let out = walk ~forward:0.15 ~frames:320 in
  Alcotest.(check int)
    "walking east reaches the far chamber" 4 (Option.get !player).Player.room;
  Alcotest.(check int) "with four doorways on the way home" 4 (ticks out);
  (* Backwards down the same corridor, still facing the same way. *)
  let home = walk ~forward:(-0.15) ~frames:320 in
  Alcotest.(check int)
    "and back where it started" 0 (Option.get !player).Player.room;
  Alcotest.(check int) "with the route unwound to nothing" 0 (ticks home)

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
    List.exists
      (fun w -> Texture.size (pattern w) = n)
      (List.init (Room.wall_count room) (Room.wall_at room))
  in
  Alcotest.(check bool) "a 128-texel pattern came off the disk" true (sized 128);
  Alcotest.(check bool)
    "beside a 64-texel generated one" true
    (sized Texture.default_size);
  (* grille.png has square holes cut out of it, and nothing told Material so:
     the alpha came out of the file and Material.opaque read it. *)
  Alcotest.(check bool)
    "and a loaded pattern carries the alpha it was drawn with" true
    (List.exists
       (fun (w : Room.wall) ->
         Texture.size (pattern w) = 64
         && (not (Material.opaque w.Room.material))
         && w.Room.height = 2.6)
       (List.init (Room.wall_count room) (Room.wall_at room)));
  let decals =
    List.init (Room.wall_count room) (Room.wall_at room)
    |> List.concat_map (fun w -> w.Room.decals)
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
       (List.init (Room.sprite_count room) (Room.sprite_at room)))

(* The floating demo is where a sprite leaves the floor and a billboard stops
   being square. Both are properties of the world it is built from, so both are
   asserted here rather than by looking at it. *)
let the_floating_demo_lifts_its_sprites () =
  let world = Floating.world in
  let sprites =
    List.init
      (Room.sprite_count (World.room world 0))
      (Room.sprite_at (World.room world 0))
    @ List.init
        (Room.sprite_count (World.room world 1))
        (Room.sprite_at (World.room world 1))
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
   every one of them somewhere else each frame.

   The rule asserted here is §15's, by {e physical} equality: every mote's
   picture must be one of the images Pictures built when it loaded, not an equal
   one made during the frame. A version that generated a picture per mote per
   frame would draw exactly the same thing and fail here.

   This used to assert a second thing — that a moving room shared the walls of
   the room it moved from, so that seventy motes were not dragging four walls
   behind them sixty times a second. A described world has no such sharing: it
   is built from nothing every frame, on purpose, and bench/frame.exe is where
   that was measured and found to cost a seventh of one percent of drawing the
   frame it is for. What is asserted instead is what a reader of the demo
   actually cares about, which is that the room stands still while the dust
   falls. *)
let the_dust_demo_moves_without_making_anything () =
  let at t = (Mount.build (Dust.at ~t)).Scene.world in
  let early = at 0.4 and late = at 3.1 in
  let sprites world =
    List.init
      (Room.sprite_count (World.room world 0))
      (Room.sprite_at (World.room world 0))
  in
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
  (* And the room they fall in does not move with them. *)
  let walls world =
    List.init
      (Room.wall_count (World.room world 0))
      (fun index ->
        let w = Room.wall_at (World.room world 0) index in
        (w.Room.a, w.Room.b, w.Room.height))
  in
  Alcotest.(check int) "four walls, still" 4 (List.length (walls early));
  Alcotest.(check bool)
    "and in the same places both times" true
    (walls early = walls late)

(* The chalk demo, driven the way a player drives it: stand somewhere, aim, and
   work the use control, which is what Aim.crosshair does. Every claim below is
   one §13.5 asks for, and none of them is visible in a screenshot.

   It used to call Chalk.place on a state it built by hand. There is no such
   function now — the wall is told, by on_use — so the driver below is a mount
   rendered into and a crosshair cast at it, which is a closer copy of what a
   player actually does than the old one was.

   The partition across the hall runs from (-1.5, 1) to (2.5, 1) and is the one
   wall here with two faces you can stand at, so it is what the side cases use.
   Facing it from the south is looking north, at +y, and standing a cell and a
   half back — inside Chalk.reach, since a wall further off than that is named
   but not markable. *)
let chalking () =
  let mount = Mount.create () in
  let render () = Mount.render mount (Chalk.marking ()) in
  ignore (render ());
  render

let aiming ?(from = Vec.make 0.5 (-0.5)) ?(angle = Float.pi /. 2.) ?(pitch = 0.)
    () =
  Player.pitch_by (Player.make ~room:0 ~pos:from ~angle) ~fraction:pitch

let use render player =
  let scene = render () in
  ignore
    (Aim.crosshair scene.Scene.targets
       ~sight:(Sight.look scene.Scene.world player)
       ~was:None ~used:true);
  render ()

(* How many marks are on the partition, which is wall five of the hall and what
   every case below aims at. *)
let on_partition (scene : Scene.t) =
  List.length (Room.wall_at (World.room scene.Scene.world 0) 5).Room.decals

let spot player (scene : Scene.t) =
  Option.map Aim.spot_of (Sight.look scene.Scene.world player)

let the_chalk_demo_marks_what_the_crosshair_is_on () =
  let render = chalking () in
  let aimed = aiming () in
  Alcotest.(check int) "bare wall to start with" 0 (on_partition (render ()));
  let marked = use render aimed in
  Alcotest.(check int) "and one mark on it after" 1 (on_partition marked);
  (* Side specificity: from the far side of the same partition there is nothing
     on it. Standing north of it, looking south. *)
  let behind = aiming ~from:(Vec.make 0.5 2.5) ~angle:(-.Float.pi /. 2.) () in
  Alcotest.(check bool)
    "though the partition is still what is being looked at" true
    (match Sight.look marked.Scene.world behind with
    | Some { Sight.kind = Sight.Wall _; _ } -> true
    | _ -> false);
  Alcotest.(check (option int))
    "and nothing on its back" None
    (match Sight.look marked.Scene.world behind with
    | Some { Sight.kind = Sight.Wall w; _ } -> w.decal
    | _ -> Some (-1))

(* Persistence. Every wall of the room is a new value every frame — that is what
   a described world is — and the marks are still on it, because the component
   keeps them and describes them again. *)
let the_chalk_demo_keeps_its_marks_through_a_rebuild () =
  let render = chalking () in
  let marked = use render (aiming ()) in
  Alcotest.(check int) "one mark to begin with" 1 (on_partition marked);
  let walls (scene : Scene.t) =
    Array.init
      (Room.wall_count (World.room scene.Scene.world 0))
      (Room.wall_at (World.room scene.Scene.world 0))
  in
  let later = render () in
  Alcotest.(check bool)
    "not one wall of the room is the value it was" true
    (Array.for_all2 (fun a b -> a != b) (walls later) (walls marked));
  Alcotest.(check int) "and the mark is still there" 1 (on_partition later);
  (* The lamp is two things. The materials are what force the rebuild above; the
     air is what reaches the chalk, since a decal is fogged like the wall it is
     on. A lamp that moved only the first would leave the marks bright in the
     dark. *)
  let fog elapsed =
    (World.atmosphere
       (Mount.build (Chalk.world_of ~marks:[] ~selected:0 ~left:8 ~elapsed))
         .Scene.world)
      .Atmosphere.fog_distance
  in
  Alcotest.(check bool)
    "the lamp really does move the air" true
    (Float.abs (Chalk.lamp 0. -. Chalk.lamp 4.5) > 0.2 && fog 4.5 < fog 0.)

(* Chalk-capacity rejection, and the through-a-doorway rule. Both are this
   demo's, not the engine's, and both are one line of Chalk.markable. *)
let the_chalk_demo_runs_out_of_chalk () =
  let render = chalking () in
  let last = ref (render ()) in
  (* Eight strokes, each at a slightly different spot along the partition. *)
  for k = 0 to 7 do
    last :=
      use render
        (aiming ~from:(Vec.make (-1. +. (float_of_int k *. 0.4)) (-0.5)) ())
  done;
  Alcotest.(check int) "eight marks placed" 8 (on_partition !last);
  let ninth = use render (aiming ~from:(Vec.make 2.2 (-0.5)) ()) in
  Alcotest.(check int) "the ninth is refused" 8 (on_partition ninth);
  Alcotest.(check (option string))
    "and it says so" (Some "no chalk left")
    (Chalk.refusal ~left:0 (spot (aiming ()) ninth));
  (* A wall in the room through the doorway is named but not markable. Pitched
     up over the figure standing in there, or the crosshair finds that instead
     and a sprite is not something this demo has a word about. *)
  let start = chalking () () in
  let through = aiming ~from:(Vec.make 3. 0.) ~angle:0. ~pitch:0.3 () in
  Alcotest.(check bool)
    "the eye does reach a wall of the next room" true
    (match Sight.look start.Scene.world through with
    | Some { Sight.kind = Sight.Wall _; crossed; _ } -> crossed > 0
    | _ -> false);
  Alcotest.(check (option string))
    "and that is why it cannot be chalked" (Some "another room")
    (Chalk.refusal ~left:8 (spot through start));
  (* The same partition from too far back: named, refused, and the refusal says
     the one thing the player can act on. *)
  Alcotest.(check (option string))
    "too far to reach" (Some "too far")
    (Chalk.refusal ~left:8 (spot (aiming ~from:(Vec.make 0.5 (-4.)) ()) start))

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
  let hall_at elapsed =
    (Room.wall_at
       (World.room
          (Mount.build (Chalk.world_of ~marks:[] ~selected:0 ~left:8 ~elapsed))
            .Scene.world 0)
       2)
      .Room.material
  in
  Alcotest.(check bool)
    "so the walls are lit the same at any lamp" true
    (hall_at 0. = hall_at (Chalk.lamp_period /. 2.))

(* The controls demo is the one that binds anything of its own, and what it
   demonstrates is a claim its doc header makes to the player: both sets of
   walking keys work, and holding one of each does not walk twice as fast. That
   is exactly the sort of thing nobody notices going wrong, so it is asserted
   rather than felt. *)
let the_controls_demo_binds_a_second_set_of_walking_keys () =
  let tick = 1. /. 60. in
  let frame held =
    Input.advance Input.untouched
      ~down:(fun control -> List.mem control held)
      ~mouse:(0., 0.) ~pointer:(0, 0) ~dt:tick
  in
  (* Qualified: [Camlcast.Controls] is the record a run's controls arrive in,
     and it shadows the demo of the same name for every file that opens the
     library. The demo's own table is the one wanted here. *)
  let table = Camlcast_demo.Controls.bindings in
  let forward held =
    (Binding.motion table (frame held) ~dt:tick).Input.forward
  in
  let expected = Config.move_speed *. tick in
  Alcotest.check close "the engine's own key still walks" expected
    (forward [ Input.Key Key.w ]);
  Alcotest.check close "and so does the one the demo added" expected
    (forward [ Input.Key Key.i ]);
  Alcotest.check close "one of each is still one step, not two" expected
    (forward [ Input.Key Key.w; Input.Key Key.i ]);
  Alcotest.check close "and the opposite of each cancels it" 0.
    (forward [ Input.Key Key.i; Input.Key Key.s ]);
  Alcotest.(check bool)
    "Escape leaves, which the engine's table would not have done" true
    (Binding.taken table.Binding.leave (frame [ Input.Key Key.escape ]))

(* The declarative layer's checker, pointed at twenty-odd worlds that are known
   to be right. Every complaint it can make is one of these worlds' invariants
   said another way, so silence here is what says the checker agrees with the
   suite above rather than merely running. A demo added to the catalogue is
   added to this too. *)
let passes_the_checker (demo : Catalogue.t) =
  let world = Lazy.force demo.Catalogue.world in
  match Check.assembled world with
  | [] -> ()
  | found -> Alcotest.failf "%s" (Check.format found)

let () =
  Alcotest.run "Demos"
    [
      ("walkable", each "is walkable" is_walkable);
      ("checked", each "passes the checker" passes_the_checker);
      ("consistent", each "is consistent" is_consistent);
      ("seams", each "has no seams" has_no_seams);
      ("connected", each "is connected" is_connected);
      ( "the catalogue",
        [
          case "names are distinct" names_are_distinct;
          case "a demo that cannot read its art is reported and not a crash"
            a_demo_that_cannot_read_its_art_is_reported_and_not_a_crash;
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
          case "the controls demo binds a second set of walking keys"
            the_controls_demo_binds_a_second_set_of_walking_keys;
        ] );
    ]
