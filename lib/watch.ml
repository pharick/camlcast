(* Implementation of {!Camlcast.Watch}; the interface carries the prose. *)

type node = Prim.t Camlcast_loom.Host.node

type t = {
  trace : (Prim.t Camlcast_loom.Trace.event -> unit) option;
  inspect :
    (Camlcast_loom.Path.t -> Camlcast_loom.Hook.slot array -> unit) option;
  patch :
    (Prim.t Camlcast_loom.Host.node list -> Prim.t Camlcast_loom.Host.node list)
    option;
}

let none = { trace = None; inspect = None; patch = None }
let make ?trace ?inspect ?patch () = { trace; inspect; patch }
