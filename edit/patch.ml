(* Implementation of {!Camlcast_edit.Patch}; the interface carries the prose. *)

open Camlcast
open Camlcast_core

type edit =
  | Wall of { a : Vec.t; b : Vec.t }
  | Sprite of Vec.t
  | Whole of Prim.t

(* Keyed by the path's debug spelling for the reason Tree keys by it: Path is
   abstract and offers equality alone, and that spelling is the one built to
   tell two places apart. *)
module Places = Map.Make (String)

type t = { mutable edits : edit Places.t }

let create () = { edits = Places.empty }
let count t = Places.cardinal t.edits
let clear t = t.edits <- Places.empty

let hold t path edit =
  t.edits <- Places.add (Camlcast_loom.Path.to_debug_string path) edit t.edits

let replace t path prim = hold t path (Whole prim)
let move_wall t path ~a ~b = hold t path (Wall { a; b })
let move_sprite t path at = hold t path (Sprite at)

(* The edited primitive, or physically the one that was there. Returning the
   same value rather than a rebuilt equal one is what keeps a frame with
   nothing outstanding from allocating a second forest. *)
let edited edit (prim : Prim.t) =
  match (edit, prim) with
  | Whole prim, _ -> prim
  | Wall { a; b }, Prim.Wall wall -> Prim.Wall { wall with a; b }
  | Sprite at, Prim.Sprite (sprite, reacts) ->
      Prim.Sprite
        ( Room.sprite ~base:sprite.Room.base ~glow:sprite.Room.glow
            ~size:sprite.Room.size ~image:sprite.Room.image at,
          reacts )
  (* An edit against a primitive of another kind is stale rather than wrong:
     the description has been rebuilt since and describes something else at
     that path. Ignored, and the next write to the source settles it. *)
  | (Wall _ | Sprite _), other -> other

(* List.map always allocates, so a forest walked with it comes back entirely
   rebuilt however little changed. This keeps the original list when every
   child came back physically unchanged, which is what makes the promise above
   -- an unedited node is the value that was there -- true rather than nearly
   true. *)
let map_shared f items =
  let changed = ref false in
  let mapped =
    List.map
      (fun item ->
        let result = f item in
        if not (result == item) then changed := true;
        result)
      items
  in
  if !changed then mapped else items

let rec walk t (node : Watch.node) =
  let children = map_shared (walk t) node.Camlcast_loom.Host.children in
  let prim =
    match
      Places.find_opt
        (Camlcast_loom.Path.to_debug_string node.Camlcast_loom.Host.path)
        t.edits
    with
    | None -> node.Camlcast_loom.Host.prim
    | Some edit -> edited edit node.Camlcast_loom.Host.prim
  in
  if
    prim == node.Camlcast_loom.Host.prim
    && children == node.Camlcast_loom.Host.children
  then node
  else { node with Camlcast_loom.Host.prim; children }

let apply t forest =
  if Places.is_empty t.edits then forest else List.map (walk t) forest
