(* Implementation of {!Camlcast.Host}; the interface carries the prose. *)

open Camlcast_core

type prim = Prim.t
type scene = Scene.t

exception Malformed of string

let node_path (node : prim Camlcast_loom.Host.node) =
  Camlcast_loom.Path.to_string node.Camlcast_loom.Host.path

(* {!Prim} words the offence; this module prefixes the path. Check carries the
   path in a field of its own, while this has only the one line. *)
let unexpected ~parent (node : prim Camlcast_loom.Host.node) =
  raise
    (Malformed
       (Printf.sprintf "%s: %s" (node_path node)
          (Prim.misplaced ~child:node.Camlcast_loom.Host.prim ~parent)))

(* Both readers of the nesting rule ask {!Nesting} for it rather than each
   walking the tree their own way: this one refuses the first thing out of
   place, Check collects every one of them with the component that wrote it.
   One pass over the whole description, before any of it is built, because a
   rule applied only in the places assembly happens to visit has holes exactly
   where assembly does not go. *)
let refuse_strangers ~parent (node : prim Camlcast_loom.Host.node) =
  match Nesting.misplaced ~parent node with
  | [] -> ()
  | (child, parent) :: _ -> unexpected ~parent child

(* A wall's decals are its children, because they are the one thing that must
   be collected before {!Room.wall} can be called at all. Their placing was
   validated by the pass above. *)
let decals_of (node : prim Camlcast_loom.Host.node) =
  List.filter_map
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Decal decal -> Some decal
      | _ -> None)
    node.Camlcast_loom.Host.children

(* Flattened in the order they were written, so the last one written is the
   last one drawn and therefore the one on top. Nesting is allowed and means
   only grouping: a component that returns three labels as one thing should not
   have to state where each goes relative to the others twice. *)
let rec collect_hud (node : prim Camlcast_loom.Host.node) =
  List.concat_map
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Hud -> collect_hud child
      | item -> item :: collect_hud child)
    node.Camlcast_loom.Host.children

(* None when neither handler was given, so a world full of scenery costs an
   array of Nones rather than a closure each. *)
let reaction_of (node : prim Camlcast_loom.Host.node) (r : Prim.reacts) =
  match (r.Prim.on_gaze, r.Prim.on_use) with
  | None, None -> None
  | on_gaze, on_use ->
      Some { Aim.path = node.Camlcast_loom.Host.path; on_gaze; on_use }

(* Two corners name a leg, in either order. Matched on the very floats the
   outline was written with rather than within a tolerance: a corner computed
   twice by different arithmetic is a leg the description did not mean, and
   guessing which one it meant is worse than saying so. *)
let same_leg (a1, b1) (a2, b2) =
  let same (p : Vec.t) (q : Vec.t) = p.Vec.x = q.Vec.x && p.Vec.y = q.Vec.y in
  (same a1 a2 && same b1 b2) || (same a1 b2 && same b1 a2)

let doors_of (node : prim Camlcast_loom.Host.node) =
  List.filter_map
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Door _ -> Some child
      | _ -> None)
    node.Camlcast_loom.Host.children

(* Where each of a room's doors lands on the leg it is cut into, worked out
   before any room is built. A floor carried through a connection is carried by
   the transform these four points imply, and a room cannot be built until its
   floor is settled — so the points have to be known first.
   {!Room.cut_points} is what {!Room.doorway} cuts at, so these are the same
   points and not a second opinion about them. *)
let openings_of (node : prim Camlcast_loom.Host.node) =
  let legs =
    List.filter_map
      (fun (c : prim Camlcast_loom.Host.node) ->
        match c.Camlcast_loom.Host.prim with
        | Prim.Wall { a; b; _ } -> Some (a, b)
        | _ -> None)
      node.Camlcast_loom.Host.children
  in
  List.filter_map
    (fun (c : prim Camlcast_loom.Host.node) ->
      match c.Camlcast_loom.Host.prim with
      | Prim.Door { id; along; width; _ } ->
          Option.map
            (fun (a, b) -> (id, Room.cut_points ~width a b))
            (List.find_opt (fun leg -> same_leg along leg) legs)
      | _ -> None)
    node.Camlcast_loom.Host.children

let build_room ~floor ~ceiling (node : prim Camlcast_loom.Host.node) =
  (* Accumulated reversed and reversed back, so that each list reaches
     {!Room.make} in the order the game wrote it. That order is not cosmetic.
     A wall's index is what {!Sight} reports and what a decal is added by, and
     a threshold's is what a portal runs parallel to. The same indices are how
     the arrays of reactions beside them are found. *)
  let walls = ref [] and thresholds = ref [] and sprites = ref [] in
  (* Which door became which threshold, so that a {!Prim.Connect} naming two
     doors can be turned into the pair of names {!World.make} joins by. *)
  let cut = ref [] in
  let wall_reacts = ref []
  and threshold_reacts = ref []
  and sprite_reacts = ref [] in
  List.iter
    (fun (child : prim Camlcast_loom.Host.node) ->
      match child.Camlcast_loom.Host.prim with
      | Prim.Wall { a; b; height; material; reacts } -> (
          match
            List.find_opt
              (fun (d : prim Camlcast_loom.Host.node) ->
                match d.Camlcast_loom.Host.prim with
                | Prim.Door { along; _ } -> same_leg along (a, b)
                | _ -> false)
              (doors_of node)
          with
          | None ->
              walls :=
                Room.wall ~height ~material ~decals:(decals_of child) a b
                :: !walls;
              wall_reacts := reaction_of child reacts :: !wall_reacts
          | Some
              ({
                 Camlcast_loom.Host.prim =
                   Prim.Door
                     {
                       id;
                       width;
                       clearance;
                       name = given;
                       leaf;
                       lintel;
                       reacts = worked;
                       _;
                     };
                 _;
               } as opening) ->
              (* Cut from the leg as the outline laid it, not as the door named
                 it. That is what makes the winding the layer's business: the
                 outline is closed, so its legs are wound already, and a
                 threshold cut out of one inherits that winding whichever way
                 round the two corners were given. *)
              let name =
                match given with
                | Some name -> name
                | None -> Printf.sprintf "door-%d" id
              in
              (* Said here rather than left to {!Room.doorway}, which would
                 refuse it in terms of a wall the description never wrote. *)
              if not (clearance <= height) then
                raise
                  (Malformed
                     (Printf.sprintf
                        "%s: the door %s is %g tall and the room it is cut \
                         into is %g"
                        (node_path opening) name clearance height));
              (* A door as wide as the leg it is cut into is allowed and
                 leaves no jamb, which is how a description says "this whole
                 leg is the opening" — a room that names its own cut points as
                 corners, so that each jamb is a leg of its own, is written
                 that way. The width and the span are then the same number
                 arrived at by two routes, and agree to the last bits rather
                 than exactly, so the comparison is made at the tolerance
                 {!World} uses for the same question and the span is what gets
                 cut. Wider than that is a real mistake and says so. *)
              let span = Vec.length (Vec.sub b a) in
              if width -. span > 1e-6 then
                raise
                  (Malformed
                     (Printf.sprintf
                        "%s: the door %s is %g wide and the leg it is cut into \
                         is %g"
                        (node_path opening) name width span));
              let jambs, threshold =
                Room.doorway ?door:leaf ~name ~width:(Float.min width span)
                  ~opening:clearance ~height ~material a b
              in
              let threshold =
                match lintel with
                | None -> threshold
                | Some lintel -> Room.with_lintel threshold (Some lintel)
              in
              List.iter
                (fun jamb ->
                  walls := jamb :: !walls;
                  wall_reacts := reaction_of child reacts :: !wall_reacts)
                jambs;
              thresholds := threshold :: !thresholds;
              threshold_reacts :=
                reaction_of opening worked :: !threshold_reacts;
              cut := (id, name) :: !cut
          | Some _ -> ())
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
  (* A door names a leg of the outline by its two corners. One that names no leg
     was cut nowhere, and would otherwise go missing in silence — the room would
     build with a solid wall where the description asked for a way through, and
     nothing would say so until a connection failed to find it, or never, if
     nothing connected it. *)
  List.iter
    (fun (d : prim Camlcast_loom.Host.node) ->
      match d.Camlcast_loom.Host.prim with
      | Prim.Door { id; along = a, b; name; _ }
        when not (List.mem_assoc id !cut) ->
          raise
            (Malformed
               (Printf.sprintf
                  "%s: the door %s runs between %s and %s, and this room's \
                   outline has no leg there"
                  (node_path d)
                  (match name with
                  | Some name -> name
                  | None -> Printf.sprintf "#%d" id)
                  (Prim.point a) (Prim.point b)))
      | _ -> ())
    (doors_of node);
  let array reacts = Array.of_list (List.rev !reacts) in
  ( built,
    (array wall_reacts, array sprite_reacts, array threshold_reacts),
    List.rev !cut )

let assemble nodes =
  match nodes with
  | [
   ({ Camlcast_loom.Host.prim = Prim.World { atmosphere; spawn }; _ } as root);
  ] ->
      let rooms = ref [] and links = ref [] and eye = ref None in
      let connections = ref [] and start = ref None in
      let over = ref false and hud = ref [] and pointing = ref false in
      refuse_strangers ~parent:root.Camlcast_loom.Host.prim root;
      List.iter
        (fun (child : prim Camlcast_loom.Host.node) ->
          match child.Camlcast_loom.Host.prim with
          | Prim.Room { name; floor; ceiling; height } ->
              (* A description need not name its rooms: nothing in it refers
                 to a room by name any more, and {!World.make} still wants one.
                 The debug spelling of the path, not the readable one: the
                 readable one drops steps that have neither a name nor a key,
                 so two unnamed rooms both print as "(root)" and World.make
                 refuses the pair as one name twice. This spelling promises
                 that two different places never print the same way. *)
              let name =
                match name with
                | Some name -> name
                | None ->
                    Camlcast_loom.Path.to_debug_string
                      child.Camlcast_loom.Host.path
              in
              (* The spawn is a child of the room it is in, so the room it
                 names is the one it was written inside and no description ever
                 spells it. *)
              List.iter
                (fun (g : prim Camlcast_loom.Host.node) ->
                  match g.Camlcast_loom.Host.prim with
                  | Prim.Camera camera -> eye := Some (name, camera)
                  | Prim.Spawn at ->
                      if Option.is_some !start then
                        raise
                          (Malformed
                             (Printf.sprintf
                                "%s: this description says twice where the \
                                 player starts"
                                (node_path g)));
                      start := Some (name, at)
                  | _ -> ())
                child.Camlcast_loom.Host.children;
              rooms := (name, floor, ceiling, height, child) :: !rooms
          | Prim.Link { here; there } -> links := (here, there) :: !links
          | Prim.Connect (a, b) -> connections := (a, b) :: !connections
          | Prim.Finish -> over := true
          | Prim.Cursor -> pointing := true
          | Prim.Hud -> hud := !hud @ collect_hud child
          | _ -> ())
        root.Camlcast_loom.Host.children;
      let collected = List.rev !rooms in
      let count = List.length collected in
      (* Every door in the world, by the identity a connection joins it by,
         with the room it stands in and the two ends it was cut at. *)
      let openings =
        List.concat
          (List.mapi
             (fun i (_, _, _, _, node) ->
               List.map (fun (id, ends) -> (id, (i, ends))) (openings_of node))
             collected)
      in
      let joins = List.rev !connections in
      (* Floors, carried out through the connections from every room that gave
         a plane of its own. A room that gave none takes its neighbour's,
         through the transform that neighbour's opening implies — which is the
         pair of ends the engine itself cut, so it cannot be the wrong pair,
         which is the mistake this exists to make unwritable.

         Breadth-first, and the first plane to arrive wins. A room a cycle
         reaches two ways could be given two floors; taking the first and
         leaving {!Check}'s seam warning to report a disagreement is the rule
         the engine already has for two floors that do not meet, and a second
         rule here would be a second answer to one question. *)
      let planes = Array.make (Int.max count 1) None in
      List.iteri
        (fun i (_, (f : Prim.surface), _, _, _) ->
          match f.Prim.plane with Some p -> planes.(i) <- Some p | None -> ())
        collected;
      let beside i =
        List.filter_map
          (fun (a, b) ->
            match (List.assoc_opt a openings, List.assoc_opt b openings) with
            | Some (ra, ea), Some (rb, eb) ->
                if ra = i then Some (rb, ea, eb)
                else if rb = i then Some (ra, eb, ea)
                else None
            | _ -> None)
          joins
      in
      let rec carry = function
        | [] -> ()
        | i :: rest ->
            carry
              (rest
              @ List.filter_map
                  (fun (j, (a1, a2), (b1, b2)) ->
                    match (planes.(i), planes.(j)) with
                    | Some plane, None ->
                        planes.(j) <-
                          Some
                            (Plane.through
                               (Transform.between ~a1 ~a2 ~b1 ~b2)
                               plane);
                        Some j
                    | _ -> None)
                  (beside i))
      in
      carry (List.init count Fun.id);
      let built =
        List.mapi
          (fun i (name, (f : Prim.surface), c, height, node) ->
            let plane =
              match planes.(i) with
              | Some plane -> plane
              | None ->
                  raise
                    (Malformed
                       (Printf.sprintf
                          "%s: this room's floor is neither given nor \
                           reachable from one that is — give it a plane, or \
                           connect it to a room that has one"
                          name))
            in
            let floor = Room.floor ~plane ~material:f.Prim.material in
            let ceiling =
              match c with
              | Prim.Sky sky -> Room.open_sky sky
              | Prim.Roof { plane = given; headroom; material } ->
                  let over =
                    match (given, headroom, height) with
                    | Some given, _, _ -> given
                    | None, Some headroom, _ -> Plane.above plane headroom
                    | None, None, Some height -> Plane.above plane height
                    | None, None, None ->
                        raise
                          (Malformed
                             (Printf.sprintf
                                "%s: this room's ceiling says neither where it \
                                 is nor how far over the floor, and the room \
                                 has no height to fall back on"
                                name))
                  in
                  Room.roof ~plane:over ~material
            in
            (name, build_room ~floor ~ceiling node))
          collected
      in
      (* Every door in the world, by the identity a connection joins it by.
         A door is cut into exactly one room, so this is where a connection
         stops being two identities and becomes the two names World.make wants:
         nothing in the description ever wrote either of them. *)
      let cut_doors =
        List.concat_map
          (fun (room, (_, _, cut)) ->
            List.map (fun (id, threshold) -> (id, (room, threshold))) cut)
          built
      in
      let connected =
        List.map
          (fun (a, b) ->
            let side id =
              match List.assoc_opt id cut_doors with
              | Some found -> found
              | None ->
                  raise
                    (Malformed
                       "a connection joins a door that no room cuts: a door is \
                        connected where it is made and cut where it stands, \
                        and this one was never cut")
            in
            (side a, side b))
          (List.rev !connections)
      in
      let world =
        World.make
          ~rooms:(List.map (fun (name, (room, _, _)) -> (name, room)) built)
          ~links:(List.rev !links @ connected)
          ~atmosphere
          ~spawn:
            (match (spawn, !start) with
            | _, Some found -> found
            | Some given, None -> given
            | None, None ->
                raise
                  (Malformed
                     "this description does not say where the player starts: \
                      put a spawn in the room they start in"))
      in
      let targets = Aim.of_rooms (List.map (fun (_, (_, r, _)) -> r) built) in
      (* The camera is resolved once the world exists, because a room's name
         only becomes an index here. A name that is not a room's is the same
         mistake as a spawn that names one, and is refused in the same words. *)
      let camera =
        Option.map
          (fun (where, (c : Prim.camera)) ->
            match World.named world where with
            | None ->
                raise
                  (Malformed
                     (Printf.sprintf "the camera is in a room called %S" where))
            | Some room ->
                Player.pitch_by
                  (Player.make ~room ~pos:c.pos ~angle:c.angle)
                  ~fraction:c.pitch)
          !eye
      in
      {
        Scene.world;
        camera;
        pointing = !pointing;
        finished = !over;
        hud = !hud;
        targets;
      }
  (* These two say something different from {!Check}'s, and unlike the pair
     above the difference comes from the shape of the report rather than from
     the words. Check has a summary and a detail: it spends the first on what
     it found ("there is no world here") and the second on the rule. This has
     one line and no room for both, so it spends it on the rule, the half that
     tells someone what to do. The offence with a primitive in it, a wall
     where the world should be, is {!Prim.not_a_world} on both sides, because
     there one sentence serves both. *)
  | [] -> raise (Malformed "a description has to have a world in it")
  | [ node ] ->
      raise
        (Malformed
           (Printf.sprintf "%s: %s" (node_path node)
              (Prim.not_a_world node.Camlcast_loom.Host.prim)))
  | _ -> raise (Malformed "a description has to have exactly one world in it")
