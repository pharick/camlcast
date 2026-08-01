(* Implementation of {!Camlcast_stage.Scene}; the interface carries the prose. *)

open Camlcast_core

type t = {
  world : World.t;
  camera : Player.t option;
  finished : bool;
  hud : Prim.t list;
}
