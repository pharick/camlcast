(* Implementation of {!Camlcast_stage.Host}; the interface carries the prose. *)

open Camlcast_core

type prim = Prim.t
type scene = Scene.t

exception Malformed of string

let node_path (node : prim Camlcast_loom.Host.node) =
  Camlcast_loom.Path.to_string node.Camlcast_loom.Host.path

let unexpected ~parent (node : prim Camlcast_loom.Host.node) =
  raise
    (Malformed
       (Printf.sprintf "%s: %s does not belong %s" (node_path node)
          (Prim.describe node.Camlcast_loom.Host.prim)
          (Prim.inside parent)))

(* Both readers of the nesting rule ask Prim for it rather than each carrying a
   copy: this one to refuse the first thing out of place, Check to collect every
   one of them with the component that wrote it. *)
let refuse_strangers ~parent (node : prim Camlcast_loom.Host.node) =
  List.iter
    (fun (child : prim Camlcast_loom.Host.node) ->
      if not (Prim.may_contain ~parent ~child:child.Camlcast_loom.Host.prim)
      then unexpected ~parent child)
    node.Camlcast_loom.Host.children

(* A wall's decals are its children, because they are the one thing that has to
   be in hand before {!Room.wall} can be called at all. *)
let decals_of ~parent (node : prim Camlcast_loom.Host.node) =
  refuse_strangers ~parent node;
  List.filter_map
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Decal decal -> Some decal
      | _ -> None)
    node.Camlcast_loom.Host.children

(* Flattened in the order they were written, so the last one written is the last
   one drawn and therefore the one on top. Nesting is allowed and means nothing
   but grouping: a component that returns three labels as one thing should not
   have to say where each of them goes relative to the others twice. *)
let rec collect_hud (node : prim Camlcast_loom.Host.node) =
  refuse_strangers ~parent:Prim.Hud node;
  List.concat_map
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Hud -> collect_hud child
      | item -> item :: collect_hud child)
    node.Camlcast_loom.Host.children

(* Nothing where a thing asked for nothing, so a world full of scenery costs an
   array of Nones rather than a closure each. *)
let reaction_of (node : prim Camlcast_loom.Host.node) (r : Prim.reacts) =
  match (r.Prim.on_gaze, r.Prim.on_use) with
  | None, None -> None
  | on_gaze, on_use ->
      Some { Aim.path = node.Camlcast_loom.Host.path; on_gaze; on_use }

let build_room ~floor ~ceiling (node : prim Camlcast_loom.Host.node) =
  (* Accumulated reversed and reversed back, so that each list reaches
     {!Room.make} in the order the game wrote it. That order is not cosmetic: a
     wall's index is what {!Sight} reports and what a decal is added by, and a
     threshold's is what a portal runs parallel to — and it is what the arrays
     of reactions beside them are found by. *)
  let walls = ref [] and thresholds = ref [] and sprites = ref [] in
  let wall_reacts = ref []
  and threshold_reacts = ref []
  and sprite_reacts = ref [] in
  refuse_strangers ~parent:node.Camlcast_loom.Host.prim node;
  List.iter
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Wall { a; b; height; material; reacts } as parent ->
          walls :=
            Room.wall ~height ~material ~decals:(decals_of ~parent child) a b
            :: !walls;
          wall_reacts := reaction_of child reacts :: !wall_reacts
      | Prim.Threshold (threshold, reacts) ->
          thresholds := threshold :: !thresholds;
          threshold_reacts := reaction_of child reacts :: !threshold_reacts
      | Prim.Sprite (sprite, reacts) ->
          sprites := sprite :: !sprites;
          sprite_reacts := reaction_of child reacts :: !sprite_reacts
      | _ -> ())
    node.Camlcast_loom.Host.children;
  let built =
    Room.make ~thresholds:(List.rev !thresholds) ~sprites:(List.rev !sprites)
      ~floor ~ceiling (List.rev !walls)
  in
  let array reacts = Array.of_list (List.rev !reacts) in
  (built, (array wall_reacts, array sprite_reacts, array threshold_reacts))

let assemble nodes =
  match nodes with
  | [
   ({ Camlcast_loom.Host.prim = Prim.World { atmosphere; spawn }; _ } as root);
  ] ->
      let rooms = ref [] and links = ref [] and eye = ref None in
      let over = ref false and hud = ref [] in
      refuse_strangers ~parent:root.Camlcast_loom.Host.prim root;
      List.iter
        (fun (child : prim Camlcast_loom.Host.node) ->
          match child.Camlcast_loom.Host.prim with
          | Prim.Room { name; floor; ceiling } ->
              rooms := (name, build_room ~floor ~ceiling child) :: !rooms
          | Prim.Link { here; there } -> links := (here, there) :: !links
          | Prim.Camera camera -> eye := Some camera
          | Prim.Finish -> over := true
          | Prim.Hud -> hud := !hud @ collect_hud child
          | _ -> ())
        root.Camlcast_loom.Host.children;
      let built = List.rev !rooms in
      let world =
        World.make
          ~rooms:(List.map (fun (name, (room, _)) -> (name, room)) built)
          ~links:(List.rev !links) ~atmosphere ~spawn
      in
      let targets = Aim.of_rooms (List.map (fun (_, (_, r)) -> r) built) in
      (* The camera is resolved once the world exists, because a room's name
         only becomes an index here. A name that is not a room's is the same
         mistake as a spawn that names one, and is refused in the same words. *)
      let camera =
        Option.map
          (fun (c : Prim.camera) ->
            match World.named world c.room with
            | None ->
                raise
                  (Malformed
                     (Printf.sprintf "the camera is in a room called %S" c.room))
            | Some room ->
                Player.pitch_by
                  (Player.make ~room ~pos:c.pos ~angle:c.angle)
                  ~radians:c.pitch)
          !eye
      in
      { Scene.world; camera; finished = !over; hud = !hud; targets }
  | [] -> raise (Malformed "a description has to have a world in it")
  | [ node ] ->
      raise
        (Malformed
           (Printf.sprintf "%s: %s is not a world" (node_path node)
              (Prim.describe node.Camlcast_loom.Host.prim)))
  | _ -> raise (Malformed "a description has to have exactly one world in it")
