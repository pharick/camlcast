(** [Binding] is the half of the input that decides what a frame of controls
    means, and it touches nothing at all: a table, a frame of {!Input.actions}
    and a duration go in, a {!Input.motion} comes out. So these drive it with
    scripted keys and a scripted mouse, and assert the arithmetic the player
    would otherwise have to feel. *)

open Camlcast
open Support

let tick = 1. /. 60.

(* A frame in which [held] is everything down and the mouse moved [mouse]. Built
   from [untouched] each time: [Binding] reads what is down and never the edges,
   so a frame's history does not come into it. *)
let frame ?(mouse = (0., 0.)) held =
  Input.advance Input.untouched
    ~down:(fun control -> List.mem control held)
    ~mouse ~pointer:(0, 0) ~dt:tick

let key k = Input.Key k
let w = key Key.w
let s = key Key.s
let d = key Key.d
let up = key Key.up

(* {1 The engine's own table}

   It is a default and not a rule, but it is the one every demo walks on, so it
   is worth pinning to the numbers {!Config} quotes. *)

let the_default_table_walks_at_the_configured_speed () =
  let asked = Binding.motion Binding.default (frame [ w ]) ~dt:tick in
  Alcotest.check close "one frame of forward"
    (Config.move_speed *. tick)
    asked.Input.forward;
  Alcotest.check close "and nothing sideways" 0. asked.Input.strafe

let a_second_of_walking_covers_the_speed () =
  let one = frame [ w ] in
  let covered =
    List.fold_left
      (fun total () ->
        total +. (Binding.motion Binding.default one ~dt:tick).Input.forward)
      0.
      (List.init 60 (fun _ -> ()))
  in
  Alcotest.check close "sixty frames of a sixtieth" Config.move_speed covered

let the_opposite_key_walks_backwards () =
  let asked = Binding.motion Binding.default (frame [ s ]) ~dt:tick in
  Alcotest.check close "back at the same pace"
    (-.Config.move_speed *. tick)
    asked.Input.forward

(* Both ends of one axis at once cancel, rather than one of them winning. *)
let holding_both_ends_of_an_axis_stands_still () =
  let asked = Binding.motion Binding.default (frame [ w; s ]) ~dt:tick in
  Alcotest.check close "forward and back come to nothing" 0. asked.Input.forward

(* Forward and sideways are separate axes and neither steals from the other.
   Squaring the diagonal off is [Player.traverse]'s job, not this one's. *)
let the_axes_are_read_separately () =
  let asked = Binding.motion Binding.default (frame [ w; d ]) ~dt:tick in
  Alcotest.check close "full forward"
    (Config.move_speed *. tick)
    asked.Input.forward;
  Alcotest.check close "and full sideways"
    (Config.move_speed *. tick)
    asked.Input.strafe

let the_arrow_keys_turn_and_look () =
  let turned = Binding.motion Binding.default (frame [ key Key.right ]) ~dt:tick
  and looked = Binding.motion Binding.default (frame [ up ]) ~dt:tick in
  Alcotest.check close "a frame of turning" (Config.rot_speed *. tick)
    turned.Input.turn;
  Alcotest.check close "a frame of looking up"
    (Config.pitch_speed *. tick)
    looked.Input.pitch

(* The mouse is a displacement: it reports what has already happened, so the
   frame's length must not be applied to it a second time. *)
let the_mouse_turns_by_its_sensitivity_per_pixel () =
  let asked =
    Binding.motion Binding.default (frame ~mouse:(10., 0.) []) ~dt:tick
  in
  Alcotest.check close "ten pixels of yaw"
    (10. *. Config.look_sensitivity)
    asked.Input.turn

let a_long_frame_does_not_multiply_the_mouse () =
  let moved = frame ~mouse:(10., 0.) [] in
  Alcotest.check close "the same ten pixels, however long the frame"
    (Binding.motion Binding.default moved ~dt:tick).Input.turn
    (Binding.motion Binding.default moved ~dt:(10. *. tick)).Input.turn

(* SDL's mouse-up is a negative delta, and looking up is positive pitch. The
   default table states that as the one negative weight in it. *)
let pushing_the_mouse_up_looks_up () =
  let asked =
    Binding.motion Binding.default (frame ~mouse:(0., -10.) []) ~dt:tick
  in
  Alcotest.check close "up the screen is up the world"
    (10. *. Config.pitch_sensitivity)
    asked.Input.pitch

(* The two sources land on one number, which is what lets a player nudge the
   mouse mid-turn without the arrow key being ignored. *)
let the_mouse_and_the_arrows_add_up () =
  let asked =
    Binding.motion Binding.default
      (frame ~mouse:(10., 0.) [ key Key.right ])
      ~dt:tick
  in
  Alcotest.check close "the rate plus the displacement"
    ((Config.rot_speed *. tick) +. (10. *. Config.look_sensitivity))
    asked.Input.turn

(* {1 Tables of a game's own} *)

let rebinding_moves_the_keys_and_nothing_else () =
  let table =
    Binding.make
      ~forward:
        {
          Binding.speed = Config.move_speed;
          terms = [ { source = Binding.Hold (key Key.i); weight = 1. } ];
        }
      ()
  in
  Alcotest.check close "the key it was given walks"
    (Config.move_speed *. tick)
    (Binding.motion table (frame [ key Key.i ]) ~dt:tick).Input.forward;
  Alcotest.check close "the one it replaced does not" 0.
    (Binding.motion table (frame [ w ]) ~dt:tick).Input.forward;
  Alcotest.check close "and the axes it said nothing about are untouched"
    (Config.move_speed *. tick)
    (Binding.motion table (frame [ d ]) ~dt:tick).Input.strafe

(* Two keys on one axis is what a table that keeps WASD {e and} adds the arrows
   looks like. Summing them unclamped would walk at twice the speed the axis
   says — which is the bug the clamp is there for. *)
let two_keys_on_one_axis_do_not_double_the_speed () =
  let both =
    Binding.make
      ~forward:
        {
          Binding.speed = Config.move_speed;
          terms =
            [
              { source = Binding.Hold w; weight = 1. };
              { source = Binding.Hold (key Key.up); weight = 1. };
            ];
        }
      ()
  in
  Alcotest.check close "one of them walks at the speed"
    (Config.move_speed *. tick)
    (Binding.motion both (frame [ w ]) ~dt:tick).Input.forward;
  Alcotest.check close "and both of them walk at the same speed"
    (Config.move_speed *. tick)
    (Binding.motion both (frame [ w; key Key.up ]) ~dt:tick).Input.forward

let a_mouse_button_can_walk_as_well_as_a_key () =
  let table =
    Binding.make
      ~forward:
        {
          Binding.speed = Config.move_speed;
          terms =
            [
              { source = Binding.Hold (Input.Button Input.Right); weight = 1. };
            ];
        }
      ()
  in
  Alcotest.check close "a button is a control like any other"
    (Config.move_speed *. tick)
    (Binding.motion table (frame [ Input.Button Input.Right ]) ~dt:tick)
      .Input.forward

let an_axis_with_no_terms_asks_for_nothing () =
  let table = Binding.make ~forward:{ Binding.speed = 9.; terms = [] } () in
  Alcotest.check close "however fast it says it is" 0.
    (Binding.motion table (frame [ w ]) ~dt:tick).Input.forward

(* A weight is a scale and not only a sign, which is what a "walk slowly" key or
   a coarser mouse is made of. *)
let a_fractional_weight_scales_the_ask () =
  let table =
    Binding.make
      ~forward:
        {
          Binding.speed = Config.move_speed;
          terms = [ { source = Binding.Hold w; weight = 0.5 } ];
        }
      ()
  in
  Alcotest.check close "half a rate is half the speed"
    (Config.move_speed *. tick /. 2.)
    (Binding.motion table (frame [ w ]) ~dt:tick).Input.forward

(* {1 The two lists the engine reads} *)

let the_default_binds_fullscreen_but_no_way_out () =
  Alcotest.(check bool)
    "F11 is in the table" true
    (Binding.taken Binding.default.Binding.fullscreen (frame [ key Key.f11 ]));
  Alcotest.(check int)
    "and nothing leaves the run" 0
    (List.length Binding.default.Binding.leave)

let any_control_in_a_list_answers_for_it () =
  let leave = [ key Key.escape; key Key.q; Input.Button Input.Middle ] in
  List.iter
    (fun control ->
      Alcotest.(check bool)
        "each one leaves on its own" true
        (Binding.taken leave (frame [ control ])))
    leave;
  Alcotest.(check bool)
    "and something else does not" false
    (Binding.taken leave (frame [ w ]))

(* [taken] is the edge and not the state: a key held down for a hundred frames
   must not leave the run a hundred times. *)
let taken_is_the_edge_not_the_state () =
  let leave = [ key Key.escape ] in
  let down = frame [ key Key.escape ] in
  let still_down =
    Input.advance down
      ~down:(fun control -> control = key Key.escape)
      ~mouse:(0., 0.) ~pointer:(0, 0) ~dt:tick
  in
  Alcotest.(check bool) "the frame it goes down" true (Binding.taken leave down);
  Alcotest.(check bool)
    "not the frame after" false
    (Binding.taken leave still_down);
  Alcotest.(check bool)
    "though it is still held" true
    (Input.down still_down (key Key.escape))

let () =
  Alcotest.run "Binding"
    [
      ( "the default table",
        [
          case "walks at the configured speed"
            the_default_table_walks_at_the_configured_speed;
          case "a second of walking covers the speed"
            a_second_of_walking_covers_the_speed;
          case "the opposite key walks backwards"
            the_opposite_key_walks_backwards;
          case "holding both ends of an axis stands still"
            holding_both_ends_of_an_axis_stands_still;
          case "the axes are read separately" the_axes_are_read_separately;
          case "the arrow keys turn and look" the_arrow_keys_turn_and_look;
        ] );
      ( "the mouse",
        [
          case "turns by its sensitivity per pixel"
            the_mouse_turns_by_its_sensitivity_per_pixel;
          case "a long frame does not multiply it"
            a_long_frame_does_not_multiply_the_mouse;
          case "pushing it up looks up" pushing_the_mouse_up_looks_up;
          case "and the arrows add up" the_mouse_and_the_arrows_add_up;
        ] );
      ( "a game's own table",
        [
          case "rebinding moves the keys and nothing else"
            rebinding_moves_the_keys_and_nothing_else;
          case "two keys on one axis do not double the speed"
            two_keys_on_one_axis_do_not_double_the_speed;
          case "a mouse button can walk as well as a key"
            a_mouse_button_can_walk_as_well_as_a_key;
          case "an axis with no terms asks for nothing"
            an_axis_with_no_terms_asks_for_nothing;
          case "a fractional weight scales the ask"
            a_fractional_weight_scales_the_ask;
        ] );
      ( "fullscreen and leaving",
        [
          case "the default binds fullscreen but no way out"
            the_default_binds_fullscreen_but_no_way_out;
          case "any control in a list answers for it"
            any_control_in_a_list_answers_for_it;
          case "taken is the edge not the state" taken_is_the_edge_not_the_state;
        ] );
    ]
