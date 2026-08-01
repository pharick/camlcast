(* Implementation of {!Camlcast}; the interface carries the prose.

   Nothing but aliases. This file exists to be the library's one visible module,
   which is what makes everything it does not mention unreachable — see
   stage/dune for why that is the point rather than a side effect. *)

module P = P
module Element = Camlcast_loom.Element
module Hook = Camlcast_loom.Hook
module Context = Camlcast_loom.Context
module Store = Camlcast_loom.Store
module Events = Events
module Run = Run
module Check = Check
module Scene = Scene
module Mount = Mount
module Debug_map = Debug_map
module Prim = Prim
module Host = Host
module Overlay = Overlay
module Vec = Camlcast_core.Vec
module Color = Camlcast_core.Color
module Plane = Camlcast_core.Plane
module Material = Camlcast_core.Material
module Texture = Camlcast_core.Texture
module Image = Camlcast_core.Image
module Atmosphere = Camlcast_core.Atmosphere
module Sky = Camlcast_core.Sky
module Door = Camlcast_core.Door
module Font = Camlcast_core.Font
module Asset = Camlcast_core.Asset
module Key = Camlcast_core.Key
module Input = Camlcast_core.Input
module Binding = Camlcast_core.Binding
module Config = Camlcast_core.Config

type surface = Camlcast_core.Room.surface = {
  plane : Plane.t;
  material : Material.t;
}

type ceiling = Camlcast_core.Room.ceiling = Roof of surface | Open of Sky.t
type side = Camlcast_core.Room.side = Front | Back
