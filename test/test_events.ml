(* Time and input reaching a description, with no window and no SDL.

   Input reads the keyboard through SDL, but its edges and hold timers are a
   pure function of two frames and a duration — which is why test_input can
   drive them directly, and why a whole game's worth of frames can be played
   here without anything being opened. A frame is a value; playing one is
   binding it and rendering. *)

open Camlcast
open Camlcast_stage
open Camlcast_loom
open Support

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let height = 4.
let flat = Plane.horizontal 0.
let floor = Room.floor ~plane:flat ~material:stone
let ceiling = Room.roof ~plane:(Plane.above flat height) ~material:stone
let tick = 1. /. 60.

let box name reach =
  Parts.room ~name ~floor ~ceiling
    [
      Parts.outline ~height ~material:stone
        [
          Vec.make (-.reach) (-.reach);
          Vec.make reach (-.reach);
          Vec.make reach reach;
          Vec.make (-.reach) reach;
        ];
    ]

let around ?(atmosphere = Atmosphere.default) children =
  Parts.world ~atmosphere ~spawn:("room", Vec.make 0. 0.) children

(* A driver: successive frames, each with whatever is held down, rendered into
   one mount so that state carries across them. *)
type driver = {
  mount : Mount.t;
  mutable actions : Input.actions;
  description : Parts.t;
}

let driving description =
  { mount = Mount.create (); actions = Input.untouched; description }

let play ?(held = []) ?(dt = tick) driver =
  driver.actions <-
    Input.advance driver.actions
      ~down:(fun control -> List.mem control held)
      ~mouse:(0., 0.) ~pointer:(0, 0) ~dt;
  Mount.render driver.mount
    (Element.provide Events.context
       { Events.dt; motion = Input.still; actions = driver.actions }
       [ driver.description ])

let () =
  Alcotest.run "Events"
    [
      ( "the frame",
        [
          case "use_frame runs once a frame, with its length" (fun () ->
              let seen = ref [] in
              let ticker =
                Element.declare ~name:"ticker" @@ fun () ->
                Events.use_frame (fun ~dt -> seen := dt :: !seen);
                around [ box "room" 4. ]
              in
              let driver = driving (ticker ()) in
              ignore (play driver);
              ignore (play ~dt:0.5 driver);
              ignore (play ~dt:0.25 driver);
              Alcotest.(check (list (float 1e-9)))
                "one entry per frame, in order" [ tick; 0.5; 0.25 ]
                (List.rev !seen));
          case "a description rendered outside a run sees no time" (fun () ->
              (* Which is what Check.report and any test that only wants the
                 geometry gets, and why it has to be a value that means
                 nothing happened rather than an exception. *)
              let seen = ref (-1.) in
              let ticker =
                Element.declare ~name:"ticker" @@ fun () ->
                seen := Events.use_dt ();
                around [ box "room" 4. ]
              in
              ignore (Mount.build (ticker ()));
              Alcotest.(check (float 1e-9)) "still" 0. !seen);
        ] );
      ( "keys",
        [
          case "use_key_down fires on the tap and not on the hold" (fun () ->
              let taps = ref 0 in
              let listener =
                Element.declare ~name:"listener" @@ fun () ->
                Events.use_key_down Key.space (fun () -> incr taps);
                around [ box "room" 4. ]
              in
              let driver = driving (listener ()) in
              ignore (play driver);
              Alcotest.(check int) "nothing yet" 0 !taps;
              ignore (play ~held:[ Input.Key Key.space ] driver);
              Alcotest.(check int) "down" 1 !taps;
              ignore (play ~held:[ Input.Key Key.space ] driver);
              ignore (play ~held:[ Input.Key Key.space ] driver);
              Alcotest.(check int) "and held is not down again" 1 !taps;
              ignore (play driver);
              ignore (play ~held:[ Input.Key Key.space ] driver);
              Alcotest.(check int) "released and pressed again is" 2 !taps);
          case "use_key_held reads the key during the render" (fun () ->
              let held = ref None in
              let listener =
                Element.declare ~name:"listener" @@ fun () ->
                held := Some (Events.use_key_held Key.w);
                around [ box "room" 4. ]
              in
              let driver = driving (listener ()) in
              ignore (play driver);
              Alcotest.(check (option bool)) "up" (Some false) !held;
              ignore (play ~held:[ Input.Key Key.w ] driver);
              Alcotest.(check (option bool)) "down" (Some true) !held;
              ignore (play ~held:[ Input.Key Key.w ] driver);
              Alcotest.(check (option bool))
                "and still down while held" (Some true) !held);
        ] );
      ( "a game played out",
        [
          case "the fuse burns down and the light goes with it" (fun () ->
              (* examples/described_fuse.ml, driven. The point is not the
                 arithmetic — it is that a component holds a clock, nothing
                 above it knows, and a test can play the whole thing through
                 without opening anything. *)
              let fuse = 1. in
              let air ~light =
                Atmosphere.make ~fog_distance:(2. +. (10. *. light)) ()
              in
              let game =
                Element.declare ~name:"game" @@ fun () ->
                let burning, set_burning = Hook.use_state false in
                let left, set_left = Hook.use_state fuse in
                Events.use_key_down Key.space (fun () -> set_burning true);
                Events.use_frame (fun ~dt ->
                    if burning && left > 0. then
                      set_left (Float.max 0. (left -. dt)));
                around ~atmosphere:(air ~light:(left /. fuse)) [ box "room" 4. ]
              in
              let driver = driving (game ()) in
              let fog scene =
                (World.atmosphere scene.Scene.world).Atmosphere.fog_distance
              in
              Alcotest.check close "full light to begin with" 12.
                (fog (play driver));
              (* Time passing on its own changes nothing: the fuse is not lit. *)
              for _ = 1 to 10 do
                ignore (play ~dt:0.05 driver)
              done;
              Alcotest.check close "and nothing has happened" 12.
                (fog (play driver));
              ignore (play ~held:[ Input.Key Key.space ] driver);
              (* A setter shows up on the frame after the one that called it, so
                 the light is still full on the frame the key went down. *)
              Alcotest.check close "lit, and not yet burnt" 12.
                (fog (play ~dt:0.5 driver));
              let after = fog (play ~dt:0.5 driver) in
              Alcotest.(check bool)
                "half gone, and dimmer for it" true
                (after < 12. && after > 2.);
              for _ = 1 to 10 do
                ignore (play ~dt:0.5 driver)
              done;
              Alcotest.check close "and then it is out" 2. (fog (play driver)));
        ] );
      ( "endings",
        [
          case "a description can say it is over" (fun () ->
              let ending =
                Element.declare ~name:"ending" @@ fun () ->
                let over, set_over = Hook.use_state false in
                Events.use_key_down Key.escape (fun () -> set_over true);
                around
                  [
                    box "room" 4.;
                    (if over then Parts.finish else Element.empty);
                  ]
              in
              let driver = driving (ending ()) in
              Alcotest.(check bool) "not yet" false (play driver).Scene.finished;
              ignore (play ~held:[ Input.Key Key.escape ] driver);
              Alcotest.(check bool)
                "and now it is" true (play driver).Scene.finished);
        ] );
      ( "the camera",
        [
          case "a description that places the eye is obeyed" (fun () ->
              let scene =
                Mount.build
                  (around
                     [
                       box "room" 4.;
                       Parts.camera ~room:"room" ~pos:(Vec.make 1.5 (-2.))
                         ~angle:0. ();
                     ])
              in
              match scene.Scene.camera with
              | None -> Alcotest.fail "the description placed one"
              | Some player ->
                  Alcotest.check vec "where it said" (Vec.make 1.5 (-2.))
                    player.Player.pos);
          case "and one that does not, leaves it to the runtime" (fun () ->
              let scene = Mount.build (around [ box "room" 4. ]) in
              Alcotest.(check bool)
                "nobody placed it" true
                (Option.is_none scene.Scene.camera));
          case "the eye can be moved from a component's own state" (fun () ->
              (* Which is the whole of what controlled means: the description
                 says where the eye is, every frame, out of state it keeps. *)
              let lift =
                Element.declare ~name:"lift" @@ fun () ->
                let y, set_y = Hook.use_state 0. in
                Events.use_frame (fun ~dt -> set_y (y +. dt));
                around
                  [
                    box "room" 4.;
                    Parts.camera ~room:"room" ~pos:(Vec.make 0. y) ~angle:0. ();
                  ]
              in
              let driver = driving (lift ()) in
              let at scene = (Option.get scene.Scene.camera).Player.pos.Vec.y in
              (* Frames of no length, so the only time that passes is the one
                 second in the middle and the arithmetic is exact. *)
              Alcotest.check close "starts at nothing" 0.
                (at (play ~dt:0. driver));
              ignore (play ~dt:1. driver);
              Alcotest.check close "and has moved by what passed" 1.
                (at (play ~dt:0. driver)));
        ] );
    ]
