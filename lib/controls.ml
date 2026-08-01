(* Implementation of {!Camlcast.Controls}; the interface carries the prose. *)

open Camlcast_core

type t = {
  bindings : Binding.t;
  use : Input.control list;
  map : Input.control list;
}

let default =
  {
    bindings = Binding.make ~leave:[ Input.Key Key.escape ] ();
    use = [ Input.Key Key.e ];
    map = [ Input.Key Key.f3 ];
  }

(* Defaulted from {!default}'s own fields rather than from constants repeated
   here, so that the two cannot come to disagree about what E is for. *)
let make ?(bindings = default.bindings) ?(use = default.use)
    ?(map = default.map) () =
  { bindings; use; map }
