(* Implementation of {!Camlcast.Prim}; the interface carries the prose. *)

open Camlcast_core

type reacts = {
  on_gaze : (bool -> unit) option;
  on_use : (Aim.spot -> unit) option;
}

type surface = { plane : Plane.t option; material : Material.t }
(** A floor or a ceiling as a description gives it: what it is made of always,
    and where it lies only when that is not the obvious place. A floor with no
    plane is carried through a {!Connect} from the room on the other side; a
    ceiling with none is the room's height above whatever floor it ends up with.
*)

type ceiling =
  | Roof of {
      plane : Plane.t option;
      headroom : float option;
      material : Material.t;
    }
  | Sky of Sky.t

type camera = { pos : Vec.t; angle : float; pitch : float }

type t =
  | World of { atmosphere : Atmosphere.t }
  | Room of {
      name : string option;
      floor : surface;
      ceiling : ceiling;
      height : float option;
    }
  | Wall of {
      a : Vec.t;
      b : Vec.t;
      height : float;
      material : Material.t;
      reacts : reacts;
    }
  | Decal of Room.decal
  | Sprite of Room.sprite * reacts
  | Camera of camera
  | Hud
  | Rect of { x : int; y : int; w : int; h : int; color : Color.t; alpha : int }
  | Line of { x0 : int; y0 : int; x1 : int; y1 : int; color : Color.t }
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

let point (v : Vec.t) = Printf.sprintf "(%g,%g)" v.x v.y

let describe = function
  | World _ -> "world"
  | Room { name; _ } -> (
      match name with Some name -> "room " ^ name | None -> "room")
  | Wall { a; b; _ } -> "wall " ^ point a ^ "-" ^ point b
  | Decal _ -> "decal"
  | Sprite (s, _) -> "sprite " ^ point s.Room.pos
  | Camera { pos; _ } -> "camera at " ^ point pos
  | Hud -> "hud"
  | Rect { x; y; _ } -> Printf.sprintf "rect at %d,%d" x y
  | Line { x0; y0; x1; y1; _ } -> Printf.sprintf "line %d,%d-%d,%d" x0 y0 x1 y1
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
  | World _, (Room _ | Connect _ | Cursor | Finish | Hud) -> true
  | Room _, (Wall _ | Sprite _ | Door _ | Spawn _ | Camera _) -> true
  | Wall _, Decal _ -> true
  | ( Hud,
      ( Rect _ | Line _ | Bar _ | Text _ | Picture _ | Highlight _ | Crosshair _
      | Hud ) ) ->
      true
  | _ -> false
