(* What a description says to draw over the frame, and where it lands.

   A HUD is pixels, so this asserts pixels — on an offscreen buffer, with no
   window. What is being checked is not that Paint works, which test_paint
   already does, but that a description reaches it: that the items come out in
   the order they were written, that nesting only groups them, and that the
   coordinates a component wrote are the coordinates they are drawn at. *)

open Camlcast
open Camlcast_stage
open Camlcast_loom
open Support

let width = 200
let height = 120

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let wall_height = 4.
let flat = Plane.horizontal 0.
let floor = Room.floor ~plane:flat ~material:stone
let ceiling = Room.roof ~plane:(Plane.above flat wall_height) ~material:stone

let room =
  Parts.room ~name:"room" ~floor ~ceiling
    [
      Parts.outline ~height:wall_height ~material:stone
        [
          Vec.make (-4.) (-4.);
          Vec.make 4. (-4.);
          Vec.make 4. 4.;
          Vec.make (-4.) 4.;
        ];
    ]

let showing items =
  Parts.world ~atmosphere:Atmosphere.default
    ~spawn:("room", Vec.make 0. 0.)
    [ room; Parts.hud items ]

(* Only the overlay, over a buffer that starts black — so every pixel that is
   not black came from the HUD and nothing has to be subtracted. *)
let painted description =
  let buffer = Framebuffer.offscreen ~width ~height in
  Overlay.draw buffer (Mount.build description).Scene.hud;
  buffer

let at buffer x y = Framebuffer.pixel buffer ~x ~y
let black = Color.rgb 0 0 0
let red = Color.rgb 220 40 40
let blue = Color.rgb 40 80 220

(* A font of one glyph: a solid block in cell zero, so "  " is nothing and any
   character the atlas has is a filled square. Enough to ask where text lands
   without asking what it looks like. *)
let block_font =
  let atlas =
    Image.make ~width:6 ~height:8 (fun ~u:_ ~v:_ ->
        (Color.rgb 255 255 255, 255))
  in
  Font.make ~atlas ~width:6 ~height:8 ~first:(Char.code 'A') ()

let () =
  Alcotest.run "Overlay"
    [
      ( "where it lands",
        [
          case "a rectangle is drawn where it was written" (fun () ->
              let buffer =
                painted
                  (showing [ Parts.rect ~x:10 ~y:20 ~w:30 ~h:15 ~color:red () ])
              in
              Alcotest.check color "inside" red (at buffer 20 25);
              Alcotest.check color "just left of it" black (at buffer 9 25);
              Alcotest.check color "just above it" black (at buffer 20 19);
              Alcotest.check color "just right of it" black (at buffer 40 25);
              Alcotest.check color "just below it" black (at buffer 20 35));
          case "a picture is drawn where it was written" (fun () ->
              let image =
                Image.make ~width:4 ~height:4 (fun ~u:_ ~v:_ -> (blue, 255))
              in
              let buffer =
                painted (showing [ Parts.picture ~x:50 ~y:60 image ])
              in
              Alcotest.check color "inside" blue (at buffer 52 62);
              Alcotest.check color "outside" black (at buffer 49 62));
          case "text is drawn where it was written" (fun () ->
              let buffer =
                painted
                  (showing
                     [ Parts.text ~font:block_font ~color:red ~x:8 ~y:8 "A" ])
              in
              Alcotest.check color "the glyph" red (at buffer 10 10);
              Alcotest.check color "before it" black (at buffer 7 10));
          case "a crosshair sits at the middle of the buffer" (fun () ->
              let buffer =
                painted (showing [ Parts.crosshair ~color:red () ])
              in
              Alcotest.check color "dead centre" red
                (at buffer (width / 2) (height / 2)));
          case "a meter fills from the left" (fun () ->
              let buffer =
                painted
                  (showing
                     [
                       Parts.bar ~x:10 ~y:10 ~w:100 ~h:8 ~fraction:0.5
                         ~color:red ();
                     ])
              in
              Alcotest.check color "the filled end" red (at buffer 20 14);
              Alcotest.(check bool)
                "and the empty one is not filled" false
                (at buffer 100 14 = red));
          case "an empty hud draws nothing at all" (fun () ->
              let buffer = painted (showing []) in
              let touched = ref 0 in
              for x = 0 to width - 1 do
                for y = 0 to height - 1 do
                  if at buffer x y <> black then incr touched
                done
              done;
              Alcotest.(check int) "not one pixel" 0 !touched);
        ] );
      ( "the order it is written in",
        [
          case "the last one written is on top" (fun () ->
              let over =
                painted
                  (showing
                     [
                       Parts.rect ~x:0 ~y:0 ~w:40 ~h:40 ~color:blue ();
                       Parts.rect ~x:0 ~y:0 ~w:40 ~h:40 ~color:red ();
                     ])
              in
              Alcotest.check color "red went on second" red (at over 20 20);
              let under =
                painted
                  (showing
                     [
                       Parts.rect ~x:0 ~y:0 ~w:40 ~h:40 ~color:red ();
                       Parts.rect ~x:0 ~y:0 ~w:40 ~h:40 ~color:blue ();
                     ])
              in
              Alcotest.check color "and the other way round" blue
                (at under 20 20));
          case "nesting groups and changes nothing else" (fun () ->
              (* A component returning three labels as one thing should not have
                 to say where each of them goes relative to the others twice. *)
              let flat_scene =
                (Mount.build
                   (showing
                      [
                        Parts.rect ~x:0 ~y:0 ~w:4 ~h:4 ~color:red ();
                        Parts.rect ~x:4 ~y:0 ~w:4 ~h:4 ~color:blue ();
                      ]))
                  .Scene.hud
              in
              let nested_scene =
                (Mount.build
                   (showing
                      [
                        Parts.hud
                          [
                            Parts.rect ~x:0 ~y:0 ~w:4 ~h:4 ~color:red ();
                            Parts.rect ~x:4 ~y:0 ~w:4 ~h:4 ~color:blue ();
                          ];
                      ]))
                  .Scene.hud
              in
              Alcotest.(check int)
                "the same two items" (List.length flat_scene)
                (List.length nested_scene);
              Alcotest.(check int)
                "and there are two" 2 (List.length nested_scene));
        ] );
      ( "what may go on it",
        [
          case "a wall on the hud is refused" (fun () ->
              match
                Mount.build
                  (showing
                     [
                       Parts.wall ~height:wall_height ~material:stone
                         (Vec.make 0. 0.) (Vec.make 1. 0.);
                     ])
              with
              | _ ->
                  Alcotest.fail "a wall is not something to draw over a frame"
              | exception Camlcast_stage.Host.Malformed _ -> ());
          case "and Check says so with a path" (fun () ->
              let summaries =
                List.map
                  (fun (d : Check.t) -> d.Check.summary)
                  (Check.report
                     (showing
                        [
                          Parts.wall ~height:wall_height ~material:stone
                            (Vec.make 0. 0.) (Vec.make 1. 0.);
                        ]))
              in
              Alcotest.(check (list string))
                "named for what it is"
                [ "a wall (0,0)-(1,0) cannot go on the hud" ]
                summaries);
        ] );
      ( "a component drawing its own",
        [
          case "a meter follows the state behind it" (fun () ->
              (* The HUD is a description like any other, so a component that
                 keeps a number can draw it without anything being told. *)
              let gauge =
                Element.declare ~name:"gauge" @@ fun () ->
                let left, set_left = Hook.use_state 1. in
                Events.use_frame (fun ~dt ->
                    set_left (Float.max 0. (left -. dt)));
                showing
                  [
                    Parts.bar ~x:10 ~y:10 ~w:100 ~h:8 ~fraction:left ~color:red
                      ();
                  ]
              in
              let mount = Mount.create () in
              let play dt =
                Mount.render mount
                  (Element.provide Events.context
                     { Events.still with Events.dt }
                     [ gauge () ])
              in
              let fraction scene =
                match scene.Scene.hud with
                | [ Prim.Bar { fraction; _ } ] -> fraction
                | _ -> Alcotest.fail "expected exactly one meter"
              in
              Alcotest.check close "full" 1. (fraction (play 0.));
              ignore (play 0.25);
              Alcotest.check close "and draining" 0.75 (fraction (play 0.));
              ignore (play 1.);
              Alcotest.check close "and empty" 0. (fraction (play 0.)));
        ] );
    ]
