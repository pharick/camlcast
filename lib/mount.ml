(* Implementation of {!Camlcast_stage.Mount}; the interface carries the prose. *)

module Tree = Camlcast_loom.Reconcile.Make (Host)

type t = Tree.t

let create = Tree.create
let render = Tree.render
let dirty = Tree.dirty
let build description = Tree.render (Tree.create ()) description
