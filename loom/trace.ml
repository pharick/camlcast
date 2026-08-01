(* Implementation of {!Camlcast_loom.Trace}; the interface carries the prose. *)

type 'prim node = Component of string | Primitive of 'prim

type 'prim event =
  | Mounted of Path.t * 'prim node
  | Updated of Path.t * 'prim node
  | Unmounted of Path.t * 'prim node

let parts = function
  | Mounted (path, node) -> ("mount", path, node)
  | Updated (path, node) -> ("update", path, node)
  | Unmounted (path, node) -> ("unmount", path, node)

let to_string describe event =
  let verb, path, node = parts event in
  (* The debug spelling, not the friendly one: a trace has to tell two sibling
     walls apart, and Path.to_string drops exactly the steps that would. A
     component's name is already the last step of its own path, so repeating it
     would say the same thing twice. *)
  let what =
    match node with
    | Component _ -> Path.to_debug_string path
    | Primitive prim -> Path.to_debug_string path ^ " : " ^ describe prim
  in
  Printf.sprintf "%-8s %s" verb what
