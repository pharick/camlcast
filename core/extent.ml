(* Implementation of {!Camlcast_core.Extent}; the interface carries the prose. *)

let fits ~limit ~width ~height = height <= limit / width
