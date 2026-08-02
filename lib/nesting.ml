(* Implementation of {!Camlcast.Nesting}; the interface carries the prose. *)

(* The parent handed down is the node's own primitive, always, which is the
   whole of what went wrong before: Host asked about a hud's grandchildren as
   though the hud were their parent, and so allowed a bar inside a rectangle.
   Nothing here may substitute one parent for another. *)
let rec misplaced ~parent (node : Prim.t Camlcast_loom.Host.node) =
  List.filter_map
    (fun (child : Prim.t Camlcast_loom.Host.node) ->
      if Prim.may_contain ~parent ~child:child.Camlcast_loom.Host.prim then None
      else Some (child, parent))
    node.Camlcast_loom.Host.children
  @ List.concat_map
      (fun (child : Prim.t Camlcast_loom.Host.node) ->
        misplaced ~parent:child.Camlcast_loom.Host.prim child)
      node.Camlcast_loom.Host.children
