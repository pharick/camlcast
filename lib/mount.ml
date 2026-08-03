(* Implementation of {!Camlcast.Mount}; the interface carries the prose. *)

module Tree = Camlcast_loom.Reconcile.Make (Host)

type t = Tree.t

let create = Tree.create
let render = Tree.render
let dirty = Tree.dirty
let destroy = Tree.destroy

let build description =
  let mount = Tree.create () in
  Fun.protect
    ~finally:(fun () -> Tree.destroy mount)
    (fun () -> Tree.render mount description)
