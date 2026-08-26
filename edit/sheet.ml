(* Implementation of {!Camlcast_edit.Sheet}; the interface carries the prose. *)

open Camlcast
open Camlcast_core

type item = {
  path : Camlcast_loom.Path.t;
  at : Camlcast_loom.Element.pos option;
  what : Prim.t;
}

type hit = Corner of { item : item; which : int } | Body of item | Nothing

let of_node (node : Watch.node) =
  {
    path = node.Camlcast_loom.Host.path;
    at = node.Camlcast_loom.Host.at;
    what = node.Camlcast_loom.Host.prim;
  }

(* What has a place on a plan. A decal is on a wall rather than in the room, a
   camera is where the eye is rather than a thing standing anywhere, and
   neither is drawn from above. *)
let drawable (prim : Prim.t) =
  match prim with
  | Prim.Wall _ | Prim.Sprite _ | Prim.Door _ | Prim.Spawn _ -> true
  | _ -> false

let items forest ~room =
  let rooms =
    List.concat_map
      (fun (node : Watch.node) ->
        List.filter
          (fun (child : Watch.node) ->
            match child.Camlcast_loom.Host.prim with
            | Prim.Room _ -> true
            | _ -> false)
          node.Camlcast_loom.Host.children)
      forest
  in
  match List.nth_opt rooms room with
  | None -> []
  | Some node ->
      List.filter_map
        (fun (child : Watch.node) ->
          if drawable child.Camlcast_loom.Host.prim then Some (of_node child)
          else None)
        node.Camlcast_loom.Host.children

(* The two ends of a thing that has two, or the one point of a thing that has
   one. A spawn and a sprite are a point; a wall and a doorway are a pair. *)
let ends item =
  match item.what with
  | Prim.Wall { a; b; _ } -> [ a; b ]
  | Prim.Door { along = a, b; _ } -> [ a; b ]
  | Prim.Sprite (sprite, _) -> [ sprite.Room.pos ]
  | Prim.Spawn at -> [ at ]
  | _ -> []

let bounds items =
  match List.concat_map ends items with
  | [] -> None
  | (first : Vec.t) :: rest ->
      Some
        (List.fold_left
           (fun (x0, y0, x1, y1) (v : Vec.t) ->
             ( Float.min x0 v.x,
               Float.min y0 v.y,
               Float.max x1 v.x,
               Float.max y1 v.y ))
           (first.x, first.y, first.x, first.y)
           rest)

(* In pixels rather than world units, so a corner is as easy to take hold of in
   a large room as in a small one: it is the pointer that has to reach it. *)
let grab = 5

let distance_to_segment (p : Vec.t) (a : Vec.t) (b : Vec.t) =
  let edge = Vec.sub b a in
  let length = Vec.length edge in
  if length < 1e-9 then Vec.length (Vec.sub p a)
  else
    let t =
      Float.max 0.
        (Float.min 1. (Vec.dot (Vec.sub p a) edge /. (length *. length)))
    in
    Vec.length (Vec.sub p (Vec.add a (Vec.scale edge t)))

let hit view items (px, py) =
  let point = (px, py) in
  let pixel_distance v =
    let qx, qy = Overhead.to_panel view v in
    Float.hypot (float_of_int (qx - px)) (float_of_int (qy - py))
  in
  (* A corner first, and the nearest of them: that is what makes a wall
     draggable by its ends and by its middle without the two fighting. *)
  let nearest_corner =
    List.fold_left
      (fun best item ->
        List.fold_left
          (fun best (which, v) ->
            let d = pixel_distance v in
            match best with
            | Some (_, _, bd) when bd <= d -> best
            | _ -> Some (item, which, d))
          best
          (List.mapi (fun which v -> (which, v)) (ends item)))
      None items
  in
  match nearest_corner with
  | Some (item, which, d) when d <= float_of_int grab -> Corner { item; which }
  | _ -> (
      let world = Overhead.to_room view point in
      let reach = float_of_int grab /. Overhead.scale view in
      match
        List.find_opt
          (fun item ->
            match ends item with
            | [ a; b ] -> distance_to_segment world a b <= reach
            | [ a ] -> Vec.length (Vec.sub world a) <= reach
            | _ -> false)
          items
      with
      | Some item -> Body item
      | None -> Nothing)

let same a b = Camlcast_loom.Path.equal a.path b.path

let picked_item = function
  | Corner { item; _ } -> Some item
  | Body item -> Some item
  | Nothing -> None

let draw ~view ~draggable ~ink ~computed ~picked ~selected items =
  let colour item =
    match picked_item selected with
    | Some chosen when same chosen item -> picked
    | _ -> if draggable item then ink else computed
  in
  let segment ?key a b ~color =
    let x0, y0 = Overhead.to_panel view a
    and x1, y1 = Overhead.to_panel view b in
    P.line ?key ~x0 ~y0 ~x1 ~y1 ~color ()
  in
  (* A point has no length to draw, so it is a small cross: two segments, which
     is the one shape this layer can make at any angle. *)
  let mark ?key at ~color =
    let x, y = Overhead.to_panel view at in
    Camlcast_loom.Element.fragment ?key
      [
        P.line ~x0:(x - 3) ~y0:(y - 3) ~x1:(x + 3) ~y1:(y + 3) ~color ();
        P.line ~x0:(x - 3) ~y0:(y + 3) ~x1:(x + 3) ~y1:(y - 3) ~color ();
      ]
  in
  Camlcast_loom.Element.fragment
    (List.mapi
       (fun index item ->
         let key = string_of_int index in
         let color = colour item in
         match ends item with
         | [ a; b ] -> segment ~key a b ~color
         | [ a ] -> mark ~key a ~color
         | _ -> Camlcast_loom.Element.empty)
       items)
