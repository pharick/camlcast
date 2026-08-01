(* Implementation of {!Camlcast_stage.Prim}; the interface carries the prose. *)

open Camlcast

type t =
  | World of { atmosphere : Atmosphere.t; spawn : string * Vec.t }
  | Room of { name : string; floor : Room.surface; ceiling : Room.ceiling }
  | Wall of { a : Vec.t; b : Vec.t; height : float; material : Material.t }
  | Decal of Room.decal
  | Threshold of Room.threshold
  | Sprite of Room.sprite
  | Link of { here : string * string; there : string * string }

let point (v : Vec.t) = Printf.sprintf "(%g,%g)" v.x v.y

let describe = function
  | World _ -> "world"
  | Room { name; _ } -> "room " ^ name
  | Wall { a; b; _ } -> "wall " ^ point a ^ "-" ^ point b
  | Decal _ -> "decal"
  | Threshold t -> "threshold " ^ t.Room.name
  | Sprite s -> "sprite " ^ point s.Room.pos
  | Link { here = ra, ta; there = rb, tb } ->
      Printf.sprintf "link %s.%s-%s.%s" ra ta rb tb
