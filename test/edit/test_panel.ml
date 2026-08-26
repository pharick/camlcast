(** How much room an overlay actually has, and what it does with too little.

    The number this suite pins is the one that decides what these views can look
    like. A HUD is drawn in the framebuffer's pixels rather than the window's,
    and the engine renders at whatever whole-number fraction of the window keeps
    it under {!Camlcast.Config.max_render_height}. So the answer does not grow
    with the display, and a view written as though it did would be discovered to
    be wrong only on somebody's monitor. *)

open Camlcast

let case name body = Alcotest.test_case name `Quick body

(* The demos' typeface: a six-by-ten grid from code point 32. *)
let font =
  Font.make ~fallback:'?' ~width:6 ~height:10 ~first:32
    ~atlas:
      (Image.make ~width:(6 * 16) ~height:(10 * 6) (fun ~u:_ ~v:_ ->
           (Color.rgb 0 0 0, 255)))
    ()

let a_full_height_panel_holds_forty_eight_lines () =
  Alcotest.(check int)
    "the tallest buffer the engine will render, at the demos' typeface" 48
    (Camlcast_edit.Panel.fits ~font ~height:Config.max_render_height);
  Alcotest.(check int)
    "and that is the ceiling however large the window" 480
    Config.max_render_height

let a_box_too_short_for_a_line_holds_none () =
  Alcotest.(check int)
    "nine pixels is not a line" 0
    (Camlcast_edit.Panel.fits ~font ~height:9);
  Alcotest.(check int) "ten is" 1 (Camlcast_edit.Panel.fits ~font ~height:10)

(* A view with more to say than room to say it must not spill: a panel drawn
   past its own box lands over the game and is unreadable against it. *)
let more_lines_than_fit_are_dropped () =
  let line text = { Camlcast_edit.Panel.text; color = Color.rgb 255 255 255 } in
  let drawn count =
    let panel =
      Camlcast_edit.Panel.draw ~font ~backing:(Color.rgb 0 0 0) ~x:0 ~y:0
        ~width:100 ~height:30
        (List.init count (fun i -> line (string_of_int i)))
    in
    match panel with
    | Camlcast_loom.Element.Fragment { children; _ } ->
        (* One backing rectangle, and a line each after it. *)
        List.length children - 1
    | _ -> Alcotest.fail "a panel is a fragment"
  in
  Alcotest.(check int) "three lines fit in thirty pixels" 3 (drawn 3);
  Alcotest.(check int) "and a fourth is dropped, not drawn outside" 3 (drawn 4);
  Alcotest.(check int) "two lines are two" 2 (drawn 2)

let () =
  Alcotest.run "Panel"
    [
      ( "the room there is",
        [
          case "a full-height panel holds forty-eight lines"
            a_full_height_panel_holds_forty_eight_lines;
          case "a box too short for a line holds none"
            a_box_too_short_for_a_line_holds_none;
          case "more lines than fit are dropped" more_lines_than_fit_are_dropped;
        ] );
    ]
