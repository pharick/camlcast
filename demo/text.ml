(** {b Writing on the screen.} A bitmap font, laid out and clipped over a
    finished frame.

    {!Raycaster.Font} is a picture of every glyph on a fixed grid, and four
    numbers describing that grid. A character's place in the atlas is arithmetic
    on its code point and its advance is the cell width, so there is no metrics
    file to keep in step with the picture — the price is that the text is
    monospaced, which at this size it would very nearly look like anyway.

    Four things to look at.

    The {b panel} is a page of wrapped text: {!Raycaster.Font.wrap} breaks the
    paragraph to a pixel width, and {!Raycaster.Font.measure} is what sizes the
    box behind it, so the box fits the text rather than the text being trusted
    to fit the box.

    The {b colours} are the same atlas three times over. The file is white and
    {!Raycaster.Paint.sub} multiplies it by the colour asked for, the way
    {!Raycaster.Color.level} scales a surface pattern's colour — one typeface,
    dressed at the point of use.

    The {b line that runs off the edge} is not truncated by anything here. Every
    glyph goes through {!Raycaster.Paint}, which clips against the buffer, so it
    is simply cut where the screen stops.

    The {b box} in the last line is a character the atlas has no cell for. It
    takes its space rather than closing up, because a substitution that changed
    the width would turn one wrong character into a wrong line.

    As with everything an overlay draws, the coordinates are the framebuffer's
    and not the window's — see {!Raycaster.Renderer.internal_size}. Resize the
    window and the text keeps its place and its size relative to the screen, and
    gets blockier or crisper with it. *)

open Raycaster
open Result_ext

let height = 4.

let world =
  let sw = Vec.make (-7.) (-7.)
  and se = Vec.make 7. (-7.)
  and ne = Vec.make 7. 7.
  and nw = Vec.make (-7.) 7. in
  let wall a b = Room.wall ~height ~material:Surfaces.brick a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:
        [ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 3. 0.) ]
      [ wall sw se; wall se ne; wall ne nw; wall nw sw ]
  in
  World.make ~rooms:[ ("room", room) ] ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-4.5) 0.)

(** The typeface, read once. [\127] is the atlas's 96th cell, a hollow box, and
    naming it as the fallback is what puts something visible in the place of a
    character the grid does not reach. *)
let font =
  lazy
    (match
       let* path = Asset.path "assets/font.png" in
       let+ atlas = Image.load path in
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

let overlay fb (_ : Player.t) =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let font = Lazy.force font in
  let lh = font.Font.height in
  let pad = 6 in
  (* A page of wrapped text, in a panel measured from the text rather than
     guessed at. *)
  let column = (width * 2 / 3) - (2 * pad) in
  let lines = Font.wrap font paragraph ~width:column in
  let body = String.concat "\n" lines in
  let tw, th = Font.measure font body in
  Paint.rect fb ~x:pad ~y:pad
    ~w:(tw + (2 * pad))
    ~h:(th + (2 * pad))
    ~r:12 ~g:14 ~b:22 ~alpha:190;
  Font.draw fb font body ~x:(2 * pad) ~y:(2 * pad) ~color:ink;
  (* The same atlas in three colours, and every printable character in it. *)
  let y = th + (4 * pad) in
  Font.draw fb font "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ~x:(2 * pad) ~y ~color:ink;
  Font.draw fb font "abcdefghijklmnopqrstuvwxyz gjpqy" ~x:(2 * pad) ~y:(y + lh)
    ~color:dim;
  Font.draw fb font "0123456789 !?.,:;()[]{}<>+-*/=@#$%&" ~x:(2 * pad)
    ~y:(y + (2 * lh)) ~color:warn;
  (* Clipped, not truncated: this line is longer than the screen and nothing
     here knows or cares where the screen ends. *)
  Font.draw fb font
    "This line is longer than the buffer is wide and is cut by the clipping in \
     Paint, not by anything that measured it first."
    ~x:(2 * pad)
    ~y:(y + (4 * lh))
    ~color:ink;
  (* A character the grid has no cell for, keeping its place. *)
  Font.draw fb font "a missing glyph:\208 keeps its space" ~x:(2 * pad)
    ~y:(y + (5 * lh)) ~color:dim;
  (* And the count, bottom right, to show measure placing something by its own
     size rather than by a number written down here. *)
  let note = Printf.sprintf "%d lines wrapped to %dpx" (List.length lines) column in
  let nw, nh = Font.measure font note in
  Font.draw fb font note ~x:(width - nw - pad) ~y:(height - nh - pad) ~color:dim;
  Paint.crosshair fb ~r:245 ~g:245 ~b:245

let run () =
  let+ _ =
    Engine.run_state
      ~update:(fun player ~dt:_ ~motion ~actions:_ ->
        Engine.step world player motion)
      ~view:(fun player -> (world, player))
      ~overlay (Player.spawn world)
  in
  ()
