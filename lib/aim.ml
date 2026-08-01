(* Implementation of {!Camlcast.Aim}; the interface carries the prose. *)

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

(* The cast comes in rather than being made here. It is one ray, and one ray is
   nothing beside a frame — but the loop wants the same answer for a second
   purpose, and casting it twice would leave two answers to one question that
   nothing guarantees agree. *)
let crosshair t ~sight ~was ~used =
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

let ring world player ~width ~height =
  let here = World.room world player.Player.room in
  let viewport =
    Viewport.make ~pitch:player.Player.pitch
      ~eye_z:
        (Plane.elevation (Room.floor_plane here) player.Player.pos
        +. Config.eye_height)
      ~width ~height
  in
  let whole = List.filter_map Fun.id in
  match Sight.look world player with
  | Some { Sight.kind = Sight.Sprite s; room; pose; distance; _ } ->
      let there = World.room world room in
      let sprite = Room.sprite_at there s.index in
      let left, top, right, bottom =
        Viewport.sprite_box viewport pose
          ~floor_z:(Plane.elevation (Room.floor_plane there) sprite.Room.pos)
          ~distance sprite
      in
      Some [ (left, top); (right, top); (right, bottom); (left, bottom) ]
  | Some { Sight.kind = Sight.Wall { index; decal = Some d; _ }; room; pose; _ }
    ->
      let there = World.room world room in
      let wall = Room.wall_at there index in
      let decal = List.nth wall.Room.decals d in
      (* Where along the wall the picture starts and stops, as points on it. *)
      let at along =
        Vec.add wall.Room.a
          (Vec.scale wall.Room.edge (along /. wall.Room.length))
      in
      let near = at (decal.Room.along -. decal.Room.half_width)
      and far = at (decal.Room.along +. decal.Room.half_width) in
      (* A decal hangs above the floor under the wall, so on a sloped one its two
         ends are at different elevations — measured at each end, not once. *)
      let corner point up =
        let foot = Plane.elevation (Room.floor_plane there) point in
        Viewport.project_point viewport pose ~point
          ~z:(foot +. decal.Room.z +. (up *. decal.Room.half_height))
      in
      let corners =
        whole
          [ corner near 1.; corner far 1.; corner far (-1.); corner near (-1.) ]
      in
      if List.length corners = 4 then Some corners else None
  | _ -> None
