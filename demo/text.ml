(** {b Writing on the screen.} A bitmap font, laid out and clipped over a
    finished frame.

    {!Camlcast_core.Font} is a picture of every glyph on a fixed grid, and four
    numbers describing that grid. A character's place in the atlas is arithmetic
    on its code point and its advance is the cell width, so there is no metrics
    file to keep in step with the picture — the price is that the text is
    monospaced, which at this size it would very nearly look like anyway.

    Four things to look at.

    The {b panel} is a page of wrapped text: {!Camlcast_core.Font.wrap} breaks
    the paragraph to a pixel width, and {!Camlcast_core.Font.measure} is what
    sizes the box behind it, so the box fits the text rather than the text being
    trusted to fit the box.

    The {b colours} are the same atlas three times over. The file is white and
    {!Camlcast_core.Paint.sub} multiplies it by the colour asked for, the way
    {!Camlcast_core.Color.level} scales a surface pattern's colour — one
    typeface, dressed at the point of use.

    The {b line that runs off the edge} is not truncated by anything here. Every
    glyph goes through {!Camlcast_core.Paint}, which clips against the buffer,
    so it is simply cut where the screen stops.

    The {b box} in the last line is a character the atlas has no cell for. It
    takes its space rather than closing up, because a substitution that changed
    the width would turn one wrong character into a wrong line.

    As with everything an overlay draws, the coordinates are the framebuffer's
    and not the window's — see {!Camlcast_core.Renderer.internal_size}. Resize
    the window and the text keeps its place and its size relative to the screen,
    and gets blockier or crisper with it. *)

open Camlcast
open Camlcast_core.Result_ext

let height = 4.
let flat = Plane.horizontal 0.
let sw = Vec.make (-7.) (-7.)
let se = Vec.make 7. (-7.)
let ne = Vec.make 7. 7.
let nw = Vec.make (-7.) 7.

(** The typeface, read once. [\127] is the atlas's 96th cell, a hollow box, and
    naming it as the fallback is what puts something visible in the place of a
    character the grid does not reach. *)
let font =
  lazy
    (match
       let+ atlas = Image.of_asset "assets/font.png" in
       Font.make ~fallback:'\127' ~atlas ~width:6 ~height:10 ~first:32 ()
     with
    | Ok font -> font
    | Error (`Msg m) -> failwith ("the text demo could not read its font: " ^ m))

let paragraph =
  "The engine holds no content: not one colour, pattern, picture or room. What \
   it has instead are the types those things are values of, so a game supplies \
   its own and two games can share an engine without sharing a look."

let ink = Color.rgb 236 232 224
let dim = Color.rgb 150 156 170
let warn = Color.rgb 232 176 96

(** The page, as a function of the buffer it has to fit in. Every measurement
    here is Font's rather than a number written down, which is the point: the
    panel is measured from the text and the count in the corner places itself by
    its own size. *)
let page ~viewport:(width, height) =
  let font = Lazy.force font in
  let lh = font.Font.height in
  let pad = 6 in
  (* A page of wrapped text, in a panel measured from the text rather than
     guessed at. *)
  let column = (width * 2 / 3) - (2 * pad) in
  let lines = Font.wrap font paragraph ~width:column in
  let body = String.concat "\n" lines in
  let tw, th = Font.measure font body in
  let y = th + (4 * pad) in
  let note =
    Printf.sprintf "%d lines wrapped to %dpx" (List.length lines) column
  in
  let nw, nh = Font.measure font note in
  P.
    [
      rect ~x:pad ~y:pad
        ~w:(tw + (2 * pad))
        ~h:(th + (2 * pad))
        ~color:(Color.rgb 12 14 22) ~alpha:190 ();
      text ~font ~x:(2 * pad) ~y:(2 * pad) ~color:ink body;
      (* The same atlas in three colours, and every printable character in
         it. *)
      text ~font ~x:(2 * pad) ~y ~color:ink "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      text ~font ~x:(2 * pad) ~y:(y + lh) ~color:dim
        "abcdefghijklmnopqrstuvwxyz gjpqy";
      text ~font ~x:(2 * pad)
        ~y:(y + (2 * lh))
        ~color:warn "0123456789 !?.,:;()[]{}<>+-*/=@#$%&";
      (* Clipped, not truncated: this line is longer than the screen and nothing
         here knows or cares where the screen ends. *)
      text ~font ~x:(2 * pad)
        ~y:(y + (4 * lh))
        ~color:ink
        "This line is longer than the buffer is wide and is cut by the \
         clipping in Paint, not by anything that measured it first.";
      (* A character the grid has no cell for, keeping its place. *)
      text ~font ~x:(2 * pad)
        ~y:(y + (5 * lh))
        ~color:dim "a missing glyph:\208 keeps its space";
      (* And the count, bottom right, placed by measure rather than by a number
         written down here. *)
      text ~font ~x:(width - nw - pad) ~y:(height - nh - pad) ~color:dim note;
      crosshair ~color:(Color.rgb 245 245 245) ();
    ]

let chamber =
  P.(
    room ~name:"room"
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
      [
        outline ~height ~material:Surfaces.brick [ sw; se; ne; nw ];
        sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 3. 0.);
      ])

let spawn = ("room", Vec.make (-4.5) 0.)

let printed =
  Element.declare ~name:"printed" @@ fun () ->
  P.(
    world ~atmosphere:Surfaces.air ~spawn
      [ chamber; hud (page ~viewport:(Events.use_viewport ())) ])

(* The room alone, for the catalogue and the suites: they ask what world this
   demo is, and the page over it is not part of that answer — nor should
   building one make them read a font off the disk. *)
let world =
  (Mount.build P.(world ~atmosphere:Surfaces.air ~spawn [ chamber ]))
    .Scene.world

let run window = Run.on window ~controls:Bindings.escapable (printed ())
