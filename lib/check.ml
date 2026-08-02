(* Implementation of {!Camlcast.Check}; the interface carries the prose. *)

open Camlcast_core
module Loom = Camlcast_loom

type severity = Error | Warning

type t = {
  severity : severity;
  where : string;
  summary : string;
  detail : string list;
  spot : (int * Vec.t) option;
}

let error ?(detail = []) ?spot where summary =
  { severity = Error; where; summary; detail; spot }

let warning ?(detail = []) ?spot where summary =
  { severity = Warning; where; summary; detail; spot }

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
                 (error (locate room) ~spot:(room, corner)
                    (Printf.sprintf
                       "the doorway %S has a corner that meets no wall"
                       threshold.Room.name)
                    ~detail:
                      [
                        Printf.sprintf "the corner at %s stands on its own."
                          (point corner);
                        "An opening is a gap in a boundary and not a boundary \
                         of its own, so each of its ends has to be the end of \
                         a wall. One that is not leaves a hole beside the \
                         opening, and the room shows its floor and sky to the \
                         horizon through it.";
                        "P.doorway cuts the gap and its jambs together and \
                         cannot leave one of these. P.threshold makes the \
                         opening alone and leaves the walls either side to the \
                         description, which is where this is usually lost.";
                      ]))
           [ threshold.Room.a; threshold.Room.b ]))

let spawn_is_clear ~locate world =
  let spawn = World.spawn world in
  if Room.blocked (World.room world spawn.World.room) spawn.World.pos then
    [
      error (locate spawn.World.room)
        ~spot:(spawn.World.room, spawn.World.pos)
        "the player starts inside a wall"
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

let assembled w = inspect ~locate:(World.name w) w

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

(* The nesting rule is Prim's, so this and Host cannot drift about what may go
   where. What differs is the job: Host raises on the first thing out of place,
   and this collects every one of them with the component that wrote it. *)
let belongs_here = function
  | Prim.World _ ->
      "A world holds rooms, the links between them, and the layer drawn over \
       the top."
  | Prim.Room _ ->
      "A room holds walls, doorways and sprites. Rooms and links belong to the \
       world outside it."
  | Prim.Wall _ -> "Only decals hang on a wall."
  | Prim.Hud -> "A hud holds what is drawn over the finished frame."
  | _ -> "Nothing goes inside that."

let walk ~parent (node : Prim.t Loom.Host.node) =
  List.map
    (fun ((child : Prim.t Loom.Host.node), parent) ->
      error (path_of child)
        (Prim.misplaced ~child:child.Loom.Host.prim ~parent)
        ~detail:[ belongs_here parent ])
    (Nesting.misplaced ~parent node)

let structure forest =
  let rooms = ref [] and links = ref [] and spawn = ref None in
  let cameras = ref [] in
  let thresholds_of (node : Prim.t Loom.Host.node) =
    List.filter_map
      (fun (child : Prim.t Loom.Host.node) ->
        match child.Loom.Host.prim with
        | Prim.Threshold (threshold, _) ->
            Some (threshold.Room.name, threshold, path_of child)
        | _ -> None)
      node.Loom.Host.children
  in
  let problems =
    match forest with
    | [ ({ Loom.Host.prim = Prim.World { spawn = where; _ }; _ } as root) ] ->
        spawn := Some where;
        List.iter
          (fun (child : Prim.t Loom.Host.node) ->
            match child.Loom.Host.prim with
            | Prim.Room { name; _ } ->
                rooms :=
                  {
                    room_path = path_of child;
                    room_name = name;
                    thresholds = thresholds_of child;
                  }
                  :: !rooms
            | Prim.Link { here; there } ->
                links := (here, there, path_of child) :: !links
            (* All of them, in the order they were written. Host takes the last
               and says nothing about the rest, which is exactly the thing a
               reader of this cannot see for themselves. *)
            | Prim.Camera { room; _ } ->
                cameras := (room, path_of child) :: !cameras
            | _ -> ())
          root.Loom.Host.children;
        walk ~parent:root.Loom.Host.prim root
    | [] ->
        [
          error "(root)" "there is no world here"
            ~detail:
              [ "Every description is one P.world with everything inside it." ];
        ]
    | [ node ] ->
        [
          error (path_of node)
            (Prim.not_a_world node.Loom.Host.prim)
            ~detail:
              [ "Every description is one P.world with everything inside it." ];
        ]
    | _ :: _ :: _ ->
        [
          error "(root)" "there is more than one world here"
            ~detail:
              [
                "A description has exactly one world in it. Wrap them in a \
                 fragment and it is still two.";
              ];
        ]
  in
  (problems, List.rev !rooms, List.rev !links, !spawn, List.rev !cameras)

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

(* A room named by something that is not a room: the same mistake wherever it is
   made — a link, a spawn, a camera — so the same sentence under it. *)
let no_such_room = "There is no room by that name in this world."

(* The one {!Host} takes, and the ones it drops. Host keeps the camera it saw
   last and overwrites as it goes, so it is the final one written that is obeyed
   and the rest are never read at all — not even for the room they name. Both of
   the things there are to say about a description with more than one camera are
   said from here, so neither can drift from the other or from the engine. *)
let camera_taken cameras =
  match List.rev cameras with
  | [] -> (None, [])
  | last :: earlier -> (Some last, List.rev earlier)

(* Every camera but the last. The complaint goes on the ones that are not being
   listened to rather than on the one that is: "this camera does nothing" is
   what there is to act on, and the winner is not itself wrong. A warning and
   not an error — the world builds, and one of the cameras is even obeyed. *)
let overruled_cameras cameras =
  let _, earlier = camera_taken cameras in
  List.map
    (fun (_, at) ->
      warning at "this camera is overruled by a later one"
        ~detail:
          [
            "A world is drawn from one eye. Where a description places it more \
             than once, the last one written is the one taken and the others \
             are dropped.";
          ])
    earlier

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
             ~detail:[ no_such_room ]);
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
        (* Asked of World rather than measured here, and negated rather than
           inverted, for the reasons that interface gives. What this file used
           to do instead — its own 1e-9 against the engine's 1e-6, and
           Option.is_some against the engine's door state — is what made a
           checker that failed worlds the engine builds and passed worlds it
           refuses. *)
        List.iter
          (fun (side, t) ->
            if not (World.has_length t) then
              complain
                (error at "this doorway is too narrow to link"
                   ~detail:
                     [
                       Printf.sprintf "%s is %g wide." (name side) t.Room.length;
                       "A doorway that small has no direction, and the engine \
                        cannot work out how the two rooms are turned relative \
                        to one another through it.";
                     ]))
          [ (here, one); (there, other) ];
        if not (World.lengths_agree one other) then
          complain
            (error at "the two sides of this link are different widths"
               ~detail:
                 [
                   Printf.sprintf "%s is %g wide and %s is %g." (name here)
                     one.Room.length (name there) other.Room.length;
                   "They are the same doorway seen from either side, so they \
                    have to be the same size.";
                 ]);
        if not (World.heights_agree one other) then
          complain
            (error at "the two sides of this link are different heights"
               ~detail:
                 [
                   Printf.sprintf "%s is %g tall and %s is %g." (name here)
                     one.Room.height (name there) other.Room.height;
                 ]);
        if not (World.doors_agree one other) then
          let describe (t : Room.threshold) =
            match t.Room.door with
            | None -> "no door"
            | Some { Door.state = Door.Open; _ } -> "a door standing open"
            | Some { Door.state = Door.Closed; _ } -> "a door standing closed"
          in
          complain
            (match (one.Room.door, other.Room.door) with
            | Some _, Some _ ->
                error at
                  "the two sides of this link disagree about whether the door \
                   is open"
                  ~detail:
                    [
                      Printf.sprintf "%s has %s and %s has %s." (name here)
                        (describe one) (name there) (describe other);
                      "It is one leaf in one opening, so a door open from one \
                       room and closed from the other is one the player could \
                       walk through in only one direction.";
                      "Door.set_state through World.set_door moves both sides \
                       at once; two descriptions written apart do not.";
                    ]
            | _ ->
                error at
                  "one side of this link has a door and the other does not"
                  ~detail:
                    [
                      Printf.sprintf "%s has %s and %s has %s." (name here)
                        (describe one) (name there) (describe other);
                      "A door hangs in one opening, so both sides have to \
                       agree that it is there.";
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

let of_forest forest =
  let structural, rooms, links, spawn, cameras = structure forest in
  let named = naming rooms in
  let found =
    if structural <> [] || named <> [] then structural @ named
    else
      let names_a_room name =
        List.exists (fun room -> room.room_name = name) rooms
      in
      let spawn_room =
        match spawn with
        | Some (room_name, _) when not (names_a_room room_name) ->
            [
              error "(root)"
                (Printf.sprintf "the player starts in a room called %S"
                   room_name)
                ~detail:[ no_such_room ];
            ]
        | Some _ | None -> []
      in
      (* The camera's own words are Host's, which raises on this from deep inside
         assembling the world. Caught here instead, where the component that wrote
         the camera can be named.

         Only the camera Host takes, because only that one's room is ever looked
         for. An overruled camera may name anything it likes and the world will
         still build; what is wrong with it is that it does nothing, which it is
         already told. *)
      let camera_room =
        match camera_taken cameras with
        | Some (room_name, at), _ when not (names_a_room room_name) ->
            [
              error at
                (Printf.sprintf "the camera is in a room called %S" room_name)
                ~detail:[ no_such_room ];
            ]
        | Some _, _ | None, _ -> []
      in
      let linked = linking rooms links in
      if spawn_room <> [] || camera_room <> [] || linked <> [] then
        spawn_room @ camera_room @ linked
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
        (* Both of the ways the engine has of refusing a description, caught so
           that a check written to replace {e these} crashes does not end in
           one. Not a promise about every crash, which this used to read as: a
           component of the game's own that raises during the render below
           comes straight out of {!report}, and {!Check.report}'s own docstring
           is where that is set out. What is bounded here is the engine's half. *)
        let refused message =
          [
            error "(root)" "the engine refused to build this world"
              ~detail:
                [
                  message;
                  "This is a check that has not been written here yet; the \
                   message above is the engine's own.";
                ];
          ]
        in
        match Host.assemble forest with
        | scene -> inspect ~locate scene.Scene.world
        | exception Invalid_argument message -> refused message
        | exception Host.Malformed message -> refused message
  in
  (* Appended rather than folded into the tiers above: a description that
     places the camera twice is saying two things whatever else is or is not
     wrong with it, and nothing in those tiers depends on the answer. *)
  found @ overruled_cameras cameras

let report description =
  let root = Forest.create () in
  (* Rendering a description starts its effects, and reading one is over when
     the reading is. See the interface: a check is a frame that is not drawn,
     and it is not a frame that is still running afterwards either. *)
  Fun.protect ~finally:(fun () -> Forest.destroy root) @@ fun () ->
  match Forest.render root description with
  | forest -> of_forest forest
  (* A primitive refusing what a component handed it — a doorway wider than the
     wall it is cut into is the common one — and the mistake this whole module
     exists to report rather than raise. It arrives named: the runtime turns the
     bare Invalid_argument into {!Loom.Element.Render_refused} carrying the path
     of the component whose description raised it, which is the line the reader
     has to go and change.

     Caught here rather than beside the two refusals in [of_forest] because it
     happens earlier than either: a description is built lazily, so a primitive
     that will not take its arguments says so while the forest is still being
     walked, and there is no forest yet to read. *)
  | exception Loom.Element.Render_refused { at; message } ->
      [
        error at "this part of the description was refused"
          ~detail:
            [
              message;
              "That is the engine's own message, raised by the primitive that \
               would not take what it was given.";
            ];
      ]
  (* The same mistake in a description assembled outside any component, where
     there is no path to name it with because nothing lazy was ever entered. *)
  | exception Invalid_argument message ->
      [
        error "(root)" "this description was refused"
          ~detail:
            [
              message;
              "That is the engine's own message. It is not attributed to a \
               component because the primitive ran while the description was \
               being built rather than while one was being rendered.";
            ];
      ]
  (* The one mistake that stops a description becoming a forest at all, and so
     the one that has to be caught here rather than read off one. Reported
     rather than raised, for the same reason the two refusals above are, and
     bounded the same way: the engine's refusals, not a game's own exception. *)
  | exception Loom.Element.Duplicate_key { at; key } ->
      [
        error at
          (Printf.sprintf "two of these children are keyed %S" key)
          ~detail:
            [
              "A key is what tells one of a parent's children from another, so \
               two under one key are two parts of a description with one name \
               between them.";
              "Give them keys of their own, or take the keys off if nothing \
               here is ever rearranged.";
            ];
      ]
