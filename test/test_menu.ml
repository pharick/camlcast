(** The launcher's list, driven without a window.

    {!Camlcast_demo.Menu.update} is a pure function of the state and one frame's
    input, in the same way {!Camlcast.Input.advance} is, so everything about
    choosing — where the cursor goes, what wraps, what Enter settles on, and
    what the arrow keys leave alone — can be checked here. Only the drawing
    needs a window, and the drawing is the part with nothing to decide. *)

open Camlcast
open Camlcast_demo
open Support

let tick = 1. /. 60.
let count = List.length Catalogue.demos

(* One frame in which [held] is everything the player is holding down. Built
   from [untouched] every time, so the key reads as newly pressed — which is
   what the menu asks about. *)
let frame held =
  Input.advance Input.untouched
    ~down:(fun control -> List.mem control held)
    ~mouse:(0., 0.) ~pointer:(0, 0) ~dt:tick

let after keys state =
  Menu.update state ~dt:tick ~motion:Input.still
    ~actions:(frame (List.map (fun key -> Input.Key key) keys))

let idle state = after [] state
let down = Key.down
let up = Key.up
let enter = Key.return

let selection =
  [
    case "opens on the first demo" (fun () ->
        Alcotest.(check int) "selected" 0 Menu.start.Menu.selected);
    case "down moves to the next" (fun () ->
        Alcotest.(check int)
          "selected" 1 (after [ down ] Menu.start).Menu.selected);
    case "up from the first wraps to the last" (fun () ->
        Alcotest.(check int)
          "selected" (count - 1) (after [ up ] Menu.start).Menu.selected);
    case "down from the last wraps to the first" (fun () ->
        let last = after [ up ] Menu.start in
        Alcotest.(check int) "selected" 0 (after [ down ] last).Menu.selected);
    case "a held key moves once, not every frame" (fun () ->
        (* [pressed] is the edge, so a key that was already down on the previous
           frame must not scroll again — the difference between a list you can
           land on an entry of and one that runs away from you. *)
        let hold previous =
          Input.advance previous
            ~down:(fun _ -> true)
            ~mouse:(0., 0.) ~dt:tick ~pointer:(0, 0)
        in
        let again = hold (hold Input.untouched) in
        let moved = after [ down ] Menu.start in
        let still =
          Menu.update moved ~dt:tick ~motion:Input.still ~actions:again
        in
        Alcotest.(check int) "selected" 1 still.Menu.selected);
  ]

let choosing =
  [
    case "nothing is chosen by standing still" (fun () ->
        Alcotest.(check bool)
          "chosen" false
          ((idle Menu.start).Menu.chosen <> None));
    case "enter settles on what is highlighted" (fun () ->
        let second = after [ down ] Menu.start in
        Alcotest.(check (option int))
          "chosen" (Some 1) (after [ enter ] second).Menu.chosen);
    case "the choice does not linger into the next frame" (fun () ->
        (* [finished] ends the run on the frame Enter arrives, but the state is
           a value like any other and must not claim a second choice if it is
           stepped again. *)
        let taken = after [ enter ] Menu.start in
        Alcotest.(check (option int)) "chosen" None (idle taken).Menu.chosen);
  ]

let backdrop =
  [
    case "every demo can be reached by holding one direction" (fun () ->
        let rec walk state seen = function
          | 0 -> List.rev seen
          | n ->
              let state = after [ down ] state in
              walk state (state.Menu.selected :: seen) (n - 1)
        in
        Alcotest.(check (list int))
          "each in turn"
          (List.init count (fun i -> (i + 1) mod count))
          (walk Menu.start [] count));
    case "the backdrop turns on its own" (fun () ->
        (* Nothing is pressed and no motion is asked for, so a camera that moves
           at all is the menu's own doing. *)
        let facing state = state.Menu.player.Player.dir.Vec.x in
        Alcotest.(check bool)
          "facing moved" true
          (facing (idle Menu.start) <> facing Menu.start));
    case "moving the cursor does not move the camera" (fun () ->
        (* The point of one fixed room: an arrow key changes the highlighted
           line and nothing else. Were the world still rebuilt per entry, the
           player would be respawned and the facing would snap back. *)
        let turned = idle (idle Menu.start) in
        let after_arrow = after [ down ] turned in
        Alcotest.(check bool)
          "same room" true
          (fst (Menu.view after_arrow) == Menu.backdrop);
        Alcotest.(check (float 1e-12))
          "facing kept"
          (Engine.step Menu.backdrop turned.Menu.player
             { Input.still with Input.turn = Menu.turn_rate /. 60. })
            .Player.dir
            .Vec.x
          after_arrow.Menu.player.Player.dir.Vec.x);
  ]

let () =
  Alcotest.run "Menu"
    [ ("selection", selection); ("choosing", choosing); ("backdrop", backdrop) ]
