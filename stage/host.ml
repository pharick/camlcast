(* Implementation of {!Camlcast_stage.Host}; the interface carries the prose. *)

open Camlcast

type prim = Prim.t
type scene = Scene.t

exception Malformed of string

let node_path (node : prim Camlcast_loom.Host.node) =
  Camlcast_loom.Path.to_string node.Camlcast_loom.Host.path

let unexpected what (node : prim Camlcast_loom.Host.node) =
  raise
    (Malformed
       (Printf.sprintf "%s: %s does not belong %s" (node_path node)
          (Prim.describe node.Camlcast_loom.Host.prim)
          what))

(* A wall's decals are its children, because they are the one thing that has to
   be in hand before {!Room.wall} can be called at all. *)
let decals_of (node : prim Camlcast_loom.Host.node) =
  List.filter_map
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Decal decal -> Some decal
      | _ -> unexpected "on a wall" child)
    node.Camlcast_loom.Host.children

let build_room ~floor ~ceiling (node : prim Camlcast_loom.Host.node) =
  (* Accumulated reversed and reversed back, so that each list reaches
     {!Room.make} in the order the game wrote it. That order is not cosmetic: a
     wall's index is what {!Sight} reports and what a decal is added by, and a
     threshold's is what a portal runs parallel to. *)
  let walls = ref [] and thresholds = ref [] and sprites = ref [] in
  List.iter
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Wall { a; b; height; material } ->
          walls :=
            Room.wall ~height ~material ~decals:(decals_of child) a b :: !walls
      | Prim.Threshold threshold -> thresholds := threshold :: !thresholds
      | Prim.Sprite sprite -> sprites := sprite :: !sprites
      | _ -> unexpected "in a room" child)
    node.Camlcast_loom.Host.children;
  Room.make ~thresholds:(List.rev !thresholds) ~sprites:(List.rev !sprites)
    ~floor ~ceiling (List.rev !walls)

let assemble nodes =
  match nodes with
  | [
   ({ Camlcast_loom.Host.prim = Prim.World { atmosphere; spawn }; _ } as root);
  ] ->
      let rooms = ref [] and links = ref [] and eye = ref None in
      let over = ref false in
      List.iter
        (fun (child : prim Camlcast_loom.Host.node) ->
          match child.Camlcast_loom.Host.prim with
          | Prim.Room { name; floor; ceiling } ->
              rooms := (name, build_room ~floor ~ceiling child) :: !rooms
          | Prim.Link { here; there } -> links := (here, there) :: !links
          | Prim.Camera camera -> eye := Some camera
          | Prim.Finish -> over := true
          | _ -> unexpected "in a world" child)
        root.Camlcast_loom.Host.children;
      let world =
        World.make ~rooms:(List.rev !rooms) ~links:(List.rev !links) ~atmosphere
          ~spawn
      in
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
      { Scene.world; camera; finished = !over }
  | [] -> raise (Malformed "a description has to have a world in it")
  | [ node ] ->
      raise
        (Malformed
           (Printf.sprintf "%s: %s is not a world" (node_path node)
              (Prim.describe node.Camlcast_loom.Host.prim)))
  | _ -> raise (Malformed "a description has to have exactly one world in it")
