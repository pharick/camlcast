(* Implementation of {!Camlcast_stage.Aim}; the interface carries the prose. *)

open Camlcast_core

type where =
  | On_wall of {
      along : float;
      z : float;
      facing : Room.side;
      decal : int option;
    }
  | On_sprite
  | On_doorway

type spot = { distance : float; crossed : int; where : where }

let spot_of (sight : Sight.t) =
  {
    distance = sight.distance;
    crossed = sight.crossed;
    where =
      (match sight.kind with
      | Sight.Wall { along; z; facing; decal; _ } ->
          On_wall { along; z; facing; decal }
      | Sight.Sprite _ -> On_sprite
      | Sight.Doorway _ -> On_doorway);
  }

type reaction = {
  path : Camlcast_loom.Path.t;
  on_gaze : (bool -> unit) option;
  on_use : (spot -> unit) option;
}

type room = {
  walls : reaction option array;
  sprites : reaction option array;
  thresholds : reaction option array;
}

type t = room array

let none = [||]

let of_rooms rooms =
  Array.of_list
    (List.map
       (fun (walls, sprites, thresholds) -> { walls; sprites; thresholds })
       rooms)

(* Guarded rather than trusted at every step. A Sight is cast against the world
   a frame was drawn from, and this is built from the same one, so they agree —
   but a world that grew between the two would disagree by an index, and a frame
   out of date is not a reason to stop. *)
let at array index =
  if index >= 0 && index < Array.length array then array.(index) else None

let find t (sight : Sight.t) =
  if sight.room < 0 || sight.room >= Array.length t then None
  else
    let room = t.(sight.room) in
    match sight.kind with
    | Sight.Wall { index; _ } -> at room.walls index
    | Sight.Sprite { index } -> at room.sprites index
    | Sight.Doorway { index } -> at room.thresholds index

let leaving t path =
  let matching array =
    Array.fold_left
      (fun found reaction ->
        match (found, reaction) with
        | Some _, _ -> found
        | None, Some r when Camlcast_loom.Path.equal r.path path -> r.on_gaze
        | None, _ -> None)
      None array
  in
  Array.fold_left
    (fun found room ->
      match found with
      | Some _ -> found
      | None -> (
          match matching room.walls with
          | Some _ as hit -> hit
          | None -> (
              match matching room.sprites with
              | Some _ as hit -> hit
              | None -> matching room.thresholds)))
    None t

let crosshair t world player ~was ~used =
  let sight = Sight.look world player in
  let reaction = Option.bind sight (find t) in
  let now = Option.map (fun r -> r.path) reaction in
  if not (Option.equal Camlcast_loom.Path.equal was now) then begin
    (* The one losing it first. Only the path was kept, so the leaving one is
       found by looking it up again — the world it was found in has been rebuilt
       since, and its indices have moved. *)
    (match was with
    | Some path -> (
        match leaving t path with Some on_gaze -> on_gaze false | None -> ())
    | None -> ());
    match reaction with
    | Some { on_gaze = Some on_gaze; _ } -> on_gaze true
    | _ -> ()
  end;
  (if used then
     match (reaction, sight) with
     | Some { on_use = Some on_use; _ }, Some sight -> on_use (spot_of sight)
     | _ -> ());
  now
