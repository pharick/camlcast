(* Implementation of {!Camlcast_stage.Check}; the interface carries the prose. *)

open Camlcast
module Loom = Camlcast_loom

type severity = Error | Warning

type t = {
  severity : severity;
  where : string;
  summary : string;
  detail : string list;
}

let error ?(detail = []) where summary =
  { severity = Error; where; summary; detail }

let warning ?(detail = []) where summary =
  { severity = Warning; where; summary; detail }

let to_string diagnostic =
  let head =
    Printf.sprintf "%s  %s"
      (match diagnostic.severity with Error -> "error" | Warning -> "warning")
      diagnostic.where
  in
  String.concat "\n"
    (head
    :: ("  " ^ diagnostic.summary)
    :: List.map (fun line -> "  " ^ line) diagnostic.detail)

let format = function
  | [] -> "nothing to report"
  | diagnostics -> String.concat "\n\n" (List.map to_string diagnostics)

(* {1 Reading a world} *)

let point (v : Vec.t) = Printf.sprintf "(%g, %g)" v.x v.y
let near a b = Vec.length (Vec.sub a b) < 1e-6

(* What counts as no step at all. A world that derives one room's floor from
   its neighbour's with Plane.through means the two to meet exactly, and on a
   sloped floor they come out a bit or two apart — the showcase level disagrees
   with itself by 1.11e-16 across one doorway and is right. This is the
   tolerance test/support.ml already calls close, and the demo suite has always
   compared seams at it, so the two agree about zero by construction rather than
   by coincidence. *)
let flat_enough = 1e-9

let doorway_ends_meet_a_wall ~locate world room =
  let here = World.room world room in
  let ends =
    List.concat
      (List.init (Room.wall_count here) (fun index ->
           let wall = Room.wall_at here index in
           [ wall.Room.a; wall.Room.b ]))
  in
  List.concat
    (List.init (Room.threshold_count here) (fun index ->
         let threshold = Room.threshold_at here index in
         List.filter_map
           (fun corner ->
             if List.exists (near corner) ends then None
             else
               Some
                 (error (locate room)
                    (Printf.sprintf
                       "the doorway %S has a corner that meets no wall"
                       threshold.Room.name)
                    ~detail:
                      [
                        Printf.sprintf "the corner at %s stands on its own."
                          (point corner);
                        "A doorway is an opening in a boundary, so each of its \
                         ends has to be the end of a wall. One that is not \
                         leaves a gap beside the opening, and the room shows \
                         its floor and sky to the horizon through it.";
                      ]))
           [ threshold.Room.a; threshold.Room.b ]))

let spawn_is_clear ~locate world =
  let spawn = World.spawn world in
  if Room.blocked (World.room world spawn.World.room) spawn.World.pos then
    [
      error (locate spawn.World.room) "the player starts inside a wall"
        ~detail:
          [
            Printf.sprintf "the spawn is at %s." (point spawn.World.pos);
            Printf.sprintf
              "Nothing within %g of a wall can be stood in, so the first step \
               in any direction is refused."
              Config.collision_padding;
          ];
    ]
  else []

let every_room_is_reachable ~locate world =
  let seen = Array.make (World.room_count world) false in
  let rec visit room =
    if not seen.(room) then begin
      seen.(room) <- true;
      for threshold = 0 to World.doorway_count world ~room - 1 do
        match World.portal world ~room ~threshold with
        | Some portal -> visit portal.World.to_room
        | None -> ()
      done
    end
  in
  visit (World.spawn world).World.room;
  List.filter_map
    (fun room ->
      if seen.(room) then None
      else
        Some
          (error (locate room) "no doorway leads to this room"
             ~detail:
               [
                 "Nothing reaches it from the spawn, through any number of \
                  doorways, so a player cannot get to whatever is in it.";
                 "A door being shut does not count against a room here: a shut \
                  door is still a way through, and whether it opens is the \
                  game's own rule.";
               ]))
    (List.init (World.room_count world) Fun.id)

let floors_meet_at_doorways ~locate world =
  List.concat
    (List.init (World.room_count world) (fun room ->
         List.filter_map
           (fun threshold ->
             match World.portal world ~room ~threshold with
             | None -> None
             | Some portal ->
                 let gap = World.seam_gap world ~room portal in
                 if Float.abs gap <= flat_enough then None
                 else
                   Some
                     (warning (locate room)
                        (Printf.sprintf
                           "the floor steps by %g through the doorway %S" gap
                           portal.World.threshold.Room.name)
                        ~detail:
                          [
                            "Each room has a floor plane of its own and \
                             nothing makes two of them meet.";
                            "The floor seen through the opening sits above or \
                             below the one you are standing on, and walking \
                             through is a step up or a drop.";
                          ]))
           (List.init (World.doorway_count world ~room) Fun.id)))

let inspect ~locate world =
  spawn_is_clear ~locate world
  @ every_room_is_reachable ~locate world
  @ List.concat
      (List.init (World.room_count world)
         (doorway_ends_meet_a_wall ~locate world))
  @ floors_meet_at_doorways ~locate world

let world w = inspect ~locate:(World.name w) w

(* {1 Reading a description}

   The same reconciler, against a host that assembles nothing: the forest a
   frame would have been built from, with every path still on it. That is the
   whole of what a second host costs, and it is why the seam is one function. *)

module Forest = Loom.Reconcile.Make (struct
  type prim = Prim.t
  type scene = Prim.t Loom.Host.node list

  let assemble nodes = nodes
end)

let path_of (node : Prim.t Loom.Host.node) =
  Loom.Path.to_string node.Loom.Host.path

type described_room = {
  room_path : string;
  room_name : string;
  thresholds : (string * Room.threshold * string) list;
}

let structure forest =
  let problems = ref [] in
  let complain d = problems := d :: !problems in
  let rooms = ref [] and links = ref [] and spawn = ref None in
  let visit_wall (node : Prim.t Loom.Host.node) =
    List.iter
      (fun (child : Prim.t Loom.Host.node) ->
        match child.Loom.Host.prim with
        | Prim.Decal _ -> ()
        | other ->
            complain
              (error (path_of child)
                 (Printf.sprintf "a %s cannot hang on a wall"
                    (Prim.describe other))
                 ~detail:[ "Only decals go on a wall." ]))
      node.Loom.Host.children
  in
  let visit_room (node : Prim.t Loom.Host.node) =
    let thresholds = ref [] in
    List.iter
      (fun (child : Prim.t Loom.Host.node) ->
        match child.Loom.Host.prim with
        | Prim.Wall _ -> visit_wall child
        | Prim.Sprite _ -> ()
        | Prim.Threshold threshold ->
            thresholds :=
              (threshold.Room.name, threshold, path_of child) :: !thresholds
        | other ->
            complain
              (error (path_of child)
                 (Printf.sprintf "a %s cannot go in a room"
                    (Prim.describe other))
                 ~detail:
                   [
                     "A room holds walls, doorways and sprites. Rooms and \
                      links belong to the world outside it.";
                   ]))
      node.Loom.Host.children;
    List.rev !thresholds
  in
  let visit_world (node : Prim.t Loom.Host.node) =
    List.iter
      (fun (child : Prim.t Loom.Host.node) ->
        match child.Loom.Host.prim with
        | Prim.Room { name; _ } ->
            rooms :=
              {
                room_path = path_of child;
                room_name = name;
                thresholds = visit_room child;
              }
              :: !rooms
        | Prim.Link { here; there } ->
            links := (here, there, path_of child) :: !links
        | other ->
            complain
              (error (path_of child)
                 (Printf.sprintf "a %s cannot go in a world"
                    (Prim.describe other))
                 ~detail:[ "A world holds rooms and the links between them." ]))
      node.Loom.Host.children
  in
  (match forest with
  | [ ({ Loom.Host.prim = Prim.World { spawn = where; _ }; _ } as root) ] ->
      spawn := Some where;
      visit_world root
  | [] ->
      complain
        (error "(root)" "there is no world here"
           ~detail:
             [
               "Every description is one Parts.world with everything inside it.";
             ])
  | [ node ] ->
      complain
        (error (path_of node)
           (Printf.sprintf "a %s is not a world"
              (Prim.describe node.Loom.Host.prim))
           ~detail:
             [
               "Every description is one Parts.world with everything inside it.";
             ])
  | _ :: _ :: _ ->
      complain
        (error "(root)" "there is more than one world here"
           ~detail:
             [
               "A description has exactly one world in it. Wrap them in a \
                fragment and it is still two.";
             ]));
  (List.rev !problems, List.rev !rooms, List.rev !links, !spawn)

(* The second and any later use of a name is what is complained about, so the
   one that was there first is left alone and a report reads as "this one is the
   duplicate" rather than "two of these are". *)
let duplicates ~what names =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun (name, at) ->
      if Hashtbl.mem seen name then
        Some
          (error at
             (Printf.sprintf "there is already a %s called %S" what name)
             ~detail:
               [
                 "A name has to be unique, because that is how a link finds it.";
               ])
      else begin
        Hashtbl.add seen name ();
        None
      end)
    names

let naming rooms =
  duplicates ~what:"room"
    (List.map (fun room -> (room.room_name, room.room_path)) rooms)
  @ List.concat_map
      (fun room ->
        duplicates ~what:"doorway"
          (List.map (fun (name, _, at) -> (name, at)) room.thresholds))
      rooms

let linking rooms links =
  let problems = ref [] in
  let complain d = problems := d :: !problems in
  let find name = List.find_opt (fun room -> room.room_name = name) rooms in
  (* Every threshold, and how many links claimed it. *)
  let claimed = Hashtbl.create 16 in
  List.iter
    (fun room ->
      List.iter
        (fun (name, _, at) ->
          Hashtbl.replace claimed (room.room_name, name) (0, at))
        room.thresholds)
    rooms;
  let side at (room_name, threshold_name) =
    match find room_name with
    | None ->
        complain
          (error at
             (Printf.sprintf "this link names a room called %S" room_name)
             ~detail:[ "There is no room by that name in this world." ]);
        false
    | Some room -> (
        match
          List.find_opt
            (fun (name, _, _) -> name = threshold_name)
            room.thresholds
        with
        | None ->
            complain
              (error at
                 (Printf.sprintf "the room %S has no doorway called %S"
                    room_name threshold_name)
                 ~detail:
                   [
                     "A link joins two doorways by the names their rooms gave \
                      them.";
                   ]);
            false
        | Some _ ->
            let count, first =
              Hashtbl.find claimed (room_name, threshold_name)
            in
            Hashtbl.replace claimed
              (room_name, threshold_name)
              (count + 1, first);
            true)
  in
  List.iter
    (fun (here, there, at) ->
      let ok_here = side at here and ok_there = side at there in
      if ok_here && ok_there then begin
        let threshold (room_name, threshold_name) =
          let room = Option.get (find room_name) in
          let _, threshold, _ =
            List.find
              (fun (name, _, _) -> name = threshold_name)
              room.thresholds
          in
          threshold
        in
        let one = threshold here and other = threshold there in
        let name (room_name, threshold_name) =
          room_name ^ "." ^ threshold_name
        in
        if Float.abs (one.Room.length -. other.Room.length) > 1e-9 then
          complain
            (error at "the two sides of this link are different widths"
               ~detail:
                 [
                   Printf.sprintf "%s is %g wide and %s is %g." (name here)
                     one.Room.length (name there) other.Room.length;
                   "They are the same doorway seen from either side, so they \
                    have to be the same size.";
                 ]);
        if Float.abs (one.Room.height -. other.Room.height) > 1e-9 then
          complain
            (error at "the two sides of this link are different heights"
               ~detail:
                 [
                   Printf.sprintf "%s is %g tall and %s is %g." (name here)
                     one.Room.height (name there) other.Room.height;
                 ]);
        if Option.is_some one.Room.door <> Option.is_some other.Room.door then
          complain
            (error at "one side of this link has a door and the other does not"
               ~detail:
                 [
                   "A door hangs in one opening, so both sides have to agree \
                    that it is there.";
                 ])
      end)
    links;
  Hashtbl.iter
    (fun (room_name, threshold_name) (count, at) ->
      if count = 0 then
        complain
          (error at
             (Printf.sprintf "the doorway %S leads nowhere" threshold_name)
             ~detail:
               [
                 Printf.sprintf "Nothing links %s.%s to another room's doorway."
                   room_name threshold_name;
                 "An unlinked doorway is drawn as haze and cannot be walked \
                  through.";
               ])
      else if count > 1 then
        complain
          (error at
             (Printf.sprintf "the doorway %S is linked %d times" threshold_name
                count)
             ~detail:
               [
                 "A doorway has two sides and joins exactly one other. Two \
                  links claiming the same one describe a place that cannot \
                  exist.";
               ]))
    claimed;
  (* Hashtbl order is unspecified, so the leftovers are sorted into the one
     order a report can be read — and asserted on — twice. *)
  List.sort
    (fun a b -> compare (a.where, a.summary) (b.where, b.summary))
    (List.rev !problems)

let report description =
  let forest = Forest.render (Forest.create ()) description in
  let structural, rooms, links, spawn = structure forest in
  let named = naming rooms in
  if structural <> [] || named <> [] then structural @ named
  else
    let spawn_room =
      match spawn with
      | Some (room_name, _) ->
          if List.exists (fun room -> room.room_name = room_name) rooms then []
          else
            [
              error "(root)"
                (Printf.sprintf "the player starts in a room called %S"
                   room_name)
                ~detail:[ "There is no room by that name in this world." ];
            ]
      | None -> []
    in
    let linked = linking rooms links in
    if spawn_room <> [] || linked <> [] then spawn_room @ linked
    else
      (* Everything that could stop a world being built has been ruled out, so
         what is left is what only an assembled world can answer. *)
      let by_index =
        Array.of_list (List.map (fun room -> room.room_path) rooms)
      in
      let locate index =
        if index < Array.length by_index then by_index.(index)
        else string_of_int index
      in
      match Host.assemble forest with
      | scene -> inspect ~locate scene.Scene.world
      | exception Invalid_argument message ->
          [
            error "(root)" "the engine refused to build this world"
              ~detail:
                [
                  message;
                  "This is a check that has not been written here yet; the \
                   message above is the engine's own.";
                ];
          ]
