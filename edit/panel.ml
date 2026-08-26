(* Implementation of {!Camlcast_edit.Panel}; the interface carries the prose. *)

open Camlcast

type line = { text : string; color : Color.t }

(* The font's own cell height is the line height -- Font says so, and says the
   field is readable so that a layout reads as arithmetic rather than as a
   series of calls. *)
let fits ~font ~height = Int.max 0 (height / font.Font.height)

let draw ~font ~backing ~x ~y ~width ~height lines =
  let room = fits ~font ~height in
  let rec take taken count = function
    | line :: rest when count < room -> take (line :: taken) (count + 1) rest
    | _ -> List.rev taken
  in
  let shown = take [] 0 lines in
  P.(
    Camlcast_loom.Element.fragment
      (rect ~x ~y ~w:width ~h:height ~color:backing ~alpha:210 ()
      :: List.mapi
           (fun index line ->
             text ~key:(string_of_int index) ~color:line.color ~font ~x
               ~y:(y + (index * font.Font.height))
               line.text)
           shown))
