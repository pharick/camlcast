(* Implementation of {!Camlcast.Prim}; the interface carries the prose. *)

open Camlcast_core

type reacts = {
  on_gaze : (bool -> unit) option;
  on_use : (Aim.spot -> unit) option;
}

let deaf = { on_gaze = None; on_use = None }

type camera = { room : string; pos : Vec.t; angle : float; pitch : float }

type t =
  | World of { atmosphere : Atmosphere.t; spawn : string * Vec.t }
  | Room of { name : string; floor : Room.surface; ceiling : Room.ceiling }
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
  | Link of { here : string * string; there : string * string }

let point (v : Vec.t) = Printf.sprintf "(%g,%g)" v.x v.y

let describe = function
  | World _ -> "world"
  | Room { name; _ } -> "room " ^ name
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
  | Link { here = ra, ta; there = rb, tb } ->
      Printf.sprintf "link %s.%s-%s.%s" ra ta rb tb

let inside = function
  | World _ -> "in a world"
  | Room _ -> "in a room"
  | Wall _ -> "on a wall"
  | Hud -> "on the hud"
  | _ -> "there"

let may_contain ~parent ~child =
  match (parent, child) with
  | World _, (Room _ | Link _ | Camera _ | Cursor | Finish | Hud) -> true
  | Room _, (Wall _ | Threshold _ | Sprite _) -> true
  | Wall _, Decal _ -> true
  | Hud, (Rect _ | Bar _ | Text _ | Picture _ | Highlight _ | Crosshair _ | Hud)
    ->
      true
  | _ -> false
