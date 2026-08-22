(* Implementation of {!Camlcast.Prim}; the interface carries the prose. *)

open Camlcast_core

type reacts = {
  on_gaze : (bool -> unit) option;
  on_use : (Aim.spot -> unit) option;
}

type camera = { room : string; pos : Vec.t; angle : float; pitch : float }

type t =
  | World of { atmosphere : Atmosphere.t; spawn : (string * Vec.t) option }
  | Room of {
      name : string option;
      floor : Room.surface;
      ceiling : Room.ceiling;
    }
  | Wall of {
      a : Vec.t;
      b : Vec.t;
      height : float;
      material : Material.t;
      reacts : reacts;
    }
  | Decal of Room.decal
  | Threshold of Room.threshold * reacts
  | Sprite of Room.sprite * reacts
  | Camera of camera
  | Hud
  | Rect of { x : int; y : int; w : int; h : int; color : Color.t; alpha : int }
  | Bar of {
      x : int;
      y : int;
      w : int;
      h : int;
      fraction : float;
      color : Color.t;
    }
  | Text of { x : int; y : int; text : string; color : Color.t; font : Font.t }
  | Picture of { x : int; y : int; image : Image.t; tint : Color.t option }
  | Highlight of Color.t
  | Crosshair of Color.t
  | Cursor
  | Finish
  | Door of {
      id : int;
      along : Vec.t * Vec.t;
      width : float;
      clearance : float;
      name : string option;
      leaf : Door.t option;
      lintel : Room.lintel option;
      reacts : reacts;
    }
  | Connect of int * int
  | Spawn of Vec.t
  | Link of { here : string * string; there : string * string }

let point (v : Vec.t) = Printf.sprintf "(%g,%g)" v.x v.y

let describe = function
  | World _ -> "world"
  | Room { name; _ } -> (
      match name with Some name -> "room " ^ name | None -> "room")
  | Wall { a; b; _ } -> "wall " ^ point a ^ "-" ^ point b
  | Decal _ -> "decal"
  | Threshold (t, _) -> "threshold " ^ t.Room.name
  | Sprite (s, _) -> "sprite " ^ point s.Room.pos
  | Camera { room; pos; _ } -> "camera in " ^ room ^ " at " ^ point pos
  | Hud -> "hud"
  | Rect { x; y; _ } -> Printf.sprintf "rect at %d,%d" x y
  | Bar { x; y; _ } -> Printf.sprintf "bar at %d,%d" x y
  | Text { text; _ } -> Printf.sprintf "text %S" text
  | Picture { x; y; _ } -> Printf.sprintf "picture at %d,%d" x y
  | Highlight _ -> "highlight"
  | Crosshair _ -> "crosshair"
  | Cursor -> "cursor"
  | Finish -> "finish"
  | Door { along = a, b; _ } -> "door " ^ point a ^ "-" ^ point b
  | Connect _ -> "connection"
  | Spawn at -> "spawn at " ^ point at
  | Link { here = ra, ta; there = rb, tb } ->
      Printf.sprintf "link %s.%s-%s.%s" ra ta rb tb

let inside = function
  | World _ -> "in a world"
  | Room _ -> "in a room"
  | Wall _ -> "on a wall"
  | Hud -> "on the hud"
  | _ -> "there"

let misplaced ~child ~parent =
  Printf.sprintf "a %s cannot go %s" (describe child) (inside parent)

let not_a_world prim = Printf.sprintf "a %s is not a world" (describe prim)

let may_contain ~parent ~child =
  match (parent, child) with
  | World _, (Room _ | Link _ | Connect _ | Camera _ | Cursor | Finish | Hud) ->
      true
  | Room _, (Wall _ | Threshold _ | Sprite _ | Door _ | Spawn _) -> true
  | Wall _, Decal _ -> true
  | Hud, (Rect _ | Bar _ | Text _ | Picture _ | Highlight _ | Crosshair _ | Hud)
    ->
      true
  | _ -> false
