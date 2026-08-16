(** The typeface the demos draw with: one 6x10 atlas, read from
    [assets/font.png], shared by several demos and the menu. {!Text}
    deliberately builds its own, because the font is that demo's subject and a
    lesson that starts from another module's loader teaches nothing.

    ['\127'] is the atlas's 96th cell, a hollow box; naming it as the fallback
    puts something visible in place of a character the grid does not reach. *)

open Camlcast_core
open Result_ext

(** Read the atlas and build the font, returning a [result] rather than raising.
    The menu uses this: it is the first file a bundled copy opens, and a missing
    file there should fall back to the printed listing rather than stop the
    program. *)
let load () =
  let+ atlas = Image.of_asset "assets/font.png" in
  Font.make ~fallback:'\127' ~atlas ~width:6 ~height:10 ~first:32 ()

(** The same font, read once and shared, for a demo already inside a frame where
    a failure cannot be handled. *)
let font = lazy (Reading.or_raise "could not read assets/font.png" (load ()))
