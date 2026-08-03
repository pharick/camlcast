(* Implementation of {!Camlcast_loom.Trace}; the interface carries the prose. *)

type 'prim node = Component of string | Primitive of 'prim

type 'prim event =
  | Mounted of Path.t * 'prim node
  | Updated of Path.t * 'prim node
  | Unmounted of Path.t * 'prim node
  | Refused

let parts = function
  | Mounted (path, node) -> Some ("mount", path, node)
  | Updated (path, node) -> Some ("update", path, node)
  | Unmounted (path, node) -> Some ("unmount", path, node)
  | Refused -> None

let to_string describe event =
  match parts event with
  (* Alone on its line and with nothing after the verb, because it is about the
     render and not about a place in the tree. *)
  | None -> "refused"
  | Some (verb, path, node) ->
      (* The debug spelling, not the friendly one: a trace has to tell two
         sibling walls apart, and Path.to_string drops exactly the steps that
         would. A component's name is already the last step of its own path, so
         repeating it would say the same thing twice. *)
      let what =
        match node with
        | Component _ -> Path.to_debug_string path
        | Primitive prim -> Path.to_debug_string path ^ " : " ^ describe prim
      in
      Printf.sprintf "%-8s %s" verb what
