(* Implementation of {!Camlcast_loom.Host}; the interface carries the prose. *)

type 'prim node = { path : Path.t; prim : 'prim; children : 'prim node list }

module type HOST = sig
  type prim
  type scene

  val assemble : prim node list -> scene
end
