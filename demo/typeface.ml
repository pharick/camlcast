(** The typeface the demos draw with.

    One 6x10 atlas, read from [assets/font.png], wanted by more than one demo
    and by the menu. {!Text} deliberately still builds its own: the font is that
    demo's whole subject, and a lesson that opens by calling somebody else's
    loader has taught nothing.

    ['\127'] is the atlas's 96th cell, a hollow box, and naming it as the
    fallback is what puts something visible in the place of a character the grid
    does not reach. *)

open Raycaster
open Result_ext

(** Read the atlas and build the font, reporting failure rather than raising.
    The menu wants this one: it is the first thing a bundled copy opens, and a
    missing picture there should fall back to the printed listing rather than
    take the program down. *)
let load () =
  let* path = Asset.path "assets/font.png" in
  let+ atlas = Image.load path in
  Font.make ~fallback:'\127' ~atlas ~width:6 ~height:10 ~first:32 ()

(** The same font, read once and shared, for a demo already deep in a frame
    where there is nothing useful to do about a failure. *)
let font =
  lazy
    (match load () with
    | Ok font -> font
    | Error (`Msg message) ->
        failwith ("could not read assets/font.png: " ^ message))
