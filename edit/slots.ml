(* Implementation of {!Camlcast_edit.Slots}; the interface carries the prose. *)

open Camlcast_loom

let kind = function
  | Hook.State -> "state"
  | Hook.Ref -> "ref"
  | Hook.Memo -> "memo"
  | Hook.Effect -> "effect"

let line (slot : Hook.slot) =
  Printf.sprintf "%-7s %s %s" (kind slot.Hook.kind)
    (if slot.Hook.changed then "*" else " ")
    (match slot.Hook.shown with Some shown -> shown | None -> "-")

let lines slots =
  if Array.length slots = 0 then [ "(no hooks)" ]
  else Array.to_list (Array.map line slots)
