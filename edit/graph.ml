(* Implementation of {!Camlcast_edit.Graph}; the interface carries the prose. *)

open Camlcast

type door = {
  path : Camlcast_loom.Path.t;
  at : Camlcast_loom.Element.pos option;
  id : int;
  name : string option;
  joined : bool;
}

type room = {
  path : Camlcast_loom.Path.t;
  index : int;
  name : string option;
  doors : door list;
}

type t = { rooms : room list; links : (int * int) list }
type placed = { room : room; x : int; y : int; radius : int }
type hit = Room of placed | Door of { room : placed; door : door } | Nothing

let children (node : Watch.node) = node.Camlcast_loom.Host.children

let read forest =
  (* Connections are children of the world and doorways are children of rooms,
     which is what makes joining two rooms and unjoining them touch neither. *)
  let links =
    List.concat_map
      (fun node ->
        List.filter_map
          (fun (child : Watch.node) ->
            match child.Camlcast_loom.Host.prim with
            | Prim.Connect (a, b) -> Some (a, b)
            | _ -> None)
          (children node))
      forest
  in
  let joined id = List.exists (fun (a, b) -> a = id || b = id) links in
  let rooms =
    List.concat_map children forest
    |> List.filter (fun (node : Watch.node) ->
        match node.Camlcast_loom.Host.prim with
        | Prim.Room _ -> true
        | _ -> false)
    |> List.mapi (fun index (node : Watch.node) ->
        let name =
          match node.Camlcast_loom.Host.prim with
          | Prim.Room { name; _ } -> name
          | _ -> None
        in
        {
          path = node.Camlcast_loom.Host.path;
          index;
          name;
          doors =
            List.filter_map
              (fun (child : Watch.node) ->
                match child.Camlcast_loom.Host.prim with
                | Prim.Door { id; name; _ } ->
                    Some
                      {
                        path = child.Camlcast_loom.Host.path;
                        at = child.Camlcast_loom.Host.at;
                        id;
                        name;
                        joined = joined id;
                      }
                | _ -> None)
              (children node);
        })
  in
  { rooms; links }

let node_radius = 16
let stub_radius = 4

(* Around a circle, decided by how many rooms there are and the order they come
   in. Nothing here iterates towards a nicer arrangement, because a nicer
   arrangement that moves is one nobody can click on. *)
let place t ~x ~y ~width ~height =
  let count = List.length t.rooms in
  if count = 0 then []
  else
    let cx = float_of_int (x + (width / 2))
    and cy = float_of_int (y + (height / 2)) in
    let ring =
      float_of_int (Int.min width height / 2) -. float_of_int (2 * node_radius)
    in
    let ring = Float.max 1. ring in
    List.mapi
      (fun index room ->
        if count = 1 then
          {
            room;
            x = int_of_float cx;
            y = int_of_float cy;
            radius = node_radius;
          }
        else
          let angle =
            2. *. Float.pi *. float_of_int index /. float_of_int count
          in
          {
            room;
            x = int_of_float (cx +. (ring *. cos angle));
            y = int_of_float (cy +. (ring *. sin angle));
            radius = node_radius;
          })
      t.rooms

(* A doorway sits on its room's rim, spread evenly round it. *)
let stub placed index count =
  let angle =
    2. *. Float.pi *. float_of_int index /. float_of_int (Int.max 1 count)
  in
  let reach = float_of_int (placed.radius + stub_radius + 2) in
  ( placed.x + int_of_float (reach *. cos angle),
    placed.y + int_of_float (reach *. sin angle) )

let stubs placed =
  let count = List.length placed.room.doors in
  List.mapi
    (fun index door -> (door, stub placed index count))
    placed.room.doors

let within (px, py) (qx, qy) radius =
  let dx = float_of_int (px - qx) and dy = float_of_int (py - qy) in
  Float.hypot dx dy <= float_of_int radius

let hit view point =
  let door =
    List.fold_left
      (fun found placed ->
        match found with
        | Some _ -> found
        | None ->
            List.find_map
              (fun (door, at) ->
                if within point at (stub_radius + 3) then
                  Some (Door { room = placed; door })
                else None)
              (stubs placed))
      None view
  in
  match door with
  | Some found -> found
  | None -> (
      match
        List.find_opt
          (fun placed -> within point (placed.x, placed.y) placed.radius)
          view
      with
      | Some placed -> Room placed
      | None -> Nothing)

let picked_room = function
  | Room placed -> Some placed.room.path
  | Door { room; _ } -> Some room.room.path
  | Nothing -> None

let ring ~x ~y ~radius ~color =
  let steps = 12 in
  List.init steps (fun step ->
      let a = 2. *. Float.pi *. float_of_int step /. float_of_int steps
      and b = 2. *. Float.pi *. float_of_int (step + 1) /. float_of_int steps in
      P.line
        ~x0:(x + int_of_float (float_of_int radius *. cos a))
        ~y0:(y + int_of_float (float_of_int radius *. sin a))
        ~x1:(x + int_of_float (float_of_int radius *. cos b))
        ~y1:(y + int_of_float (float_of_int radius *. sin b))
        ~color ())

let draw ~view ~ink ~joined ~stub:stub_ink ~picked ~selected t =
  let chosen = picked_room selected in
  let at_door id =
    List.find_map
      (fun placed ->
        List.find_map
          (fun (door, at) -> if door.id = id then Some at else None)
          (stubs placed))
      view
  in
  let edges =
    List.filter_map
      (fun (a, b) ->
        match (at_door a, at_door b) with
        | Some (x0, y0), Some (x1, y1) ->
            Some (P.line ~x0 ~y0 ~x1 ~y1 ~color:joined ())
        | _ -> None)
      t.links
  in
  let nodes =
    List.concat_map
      (fun placed ->
        let colour =
          match chosen with
          | Some path when Camlcast_loom.Path.equal path placed.room.path ->
              picked
          | _ -> ink
        in
        ring ~x:placed.x ~y:placed.y ~radius:placed.radius ~color:colour
        @ List.map
            (fun (door, (dx, dy)) ->
              let colour = if door.joined then joined else stub_ink in
              Camlcast_loom.Element.fragment
                (ring ~x:dx ~y:dy ~radius:stub_radius ~color:colour))
            (stubs placed))
      view
  in
  Camlcast_loom.Element.fragment (edges @ nodes)

(* The identifier a door was made under, read off the cut that placed it. A
   door's own name is for diagnostics and is not what a connection can name;
   see the interface. *)
let binding parsed door =
  match door.at with
  | None ->
      Error
        (`Msg
           (Printf.sprintf
              "a doorway with no position%s: build with ppx_camlcast"
              (match door.name with Some n -> " (" ^ n ^ ")" | None -> "")))
  | Some (_, line, column, _) -> (
      match Span.at parsed ~line ~column with
      | None -> Error (`Msg "the doorway's own line no longer holds a cut")
      | Some call -> (
          match
            List.find_map
              (fun (argument : Span.argument) ->
                match (argument.label, argument.value) with
                | None, Span.Name span -> Some span
                | _ -> None)
              call.arguments
          with
          | Some span -> Ok (Span.slice parsed span)
          | None ->
              Error (`Msg "the cut does not name a doorway this can write down")
          ))

let join parsed ~world a b =
  if a.id = b.id then Error (`Msg "a doorway cannot be joined to itself")
  else if a.joined || b.joined then
    Error (`Msg "one of these doorways is already part of a connection")
  else
    match (binding parsed a, binding parsed b) with
    | (Error _ as error), _ | _, (Error _ as error) -> error
    | Ok left, Ok right -> (
        (* The world's children: its one positional argument, the list. *)
        match
          List.rev
            (List.filter
               (fun (argument : Span.argument) -> argument.label = None)
               world.Span.arguments)
        with
        | [] -> Error (`Msg "the world names no children to connect within")
        | children :: _ ->
            let source = Span.text parsed in
            let closing = children.Span.span.Span.stop - 1 in
            (* A list already ending in a separator, or an empty one, takes no
               second separator in front of what is appended. *)
            let rec back index =
              if index <= children.Span.span.Span.start then '['
              else
                match source.[index] with
                | ' ' | '\n' | '\t' | '\r' -> back (index - 1)
                | c -> c
            in
            let lead =
              match back (closing - 1) with '[' | ';' -> "" | _ -> "; "
            in
            Ok
              ( { Span.start = closing; stop = closing },
                Printf.sprintf "%sconnect %s %s " lead left right ))
