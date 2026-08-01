(** A raycasting engine you describe rather than drive.

    A game says what its world should be, every frame, and the runtime works out
    what changed. Parts of a world are components with props, state and events;
    components compose into other components; and the framebuffer, the renderer
    and the world itself are on the other side of this file.

    {1 Everything a game opens}

    This one module. What is in it is what a description is made of, and what is
    not in it is the platform underneath — {!Camlcast_core.Engine},
    {!Camlcast_core.Renderer}, {!Camlcast_core.Framebuffer},
    {!Camlcast_core.World}, {!Camlcast_core.Player}. Those are reachable, by
    adding [camlcast.core] to a dune file and saying so, which is the boundary
    being a decision with a diff rather than something autocomplete finds for
    you.

    {1 The shape of a game}

    {[
    open Camlcast

    let torch =
      Element.declare ~name:"torch" @@ fun pos ->
      let lit, set_lit = Hook.use_state true in
      Events.use_key_down Key.e (fun () -> set_lit (not lit));
      P.sprite ~size:0.8 ~image:(if lit then flame else stub) pos

    let level =
      P.(
        world ~atmosphere:Atmosphere.default ~spawn:("room", origin)
          [
            room ~name:"room" ~floor ~ceiling
              [ outline ~height:4. ~material:stone corners; torch here ];
          ])

    let () = ignore (Run.play ~title:"A game" level)
    ]}

    {!P} is written [P.( ... )] around a description rather than opened over a
    whole file. Its names are short and ordinary — [wall], [room], [text],
    [path] — and a local open puts them in scope exactly where a description is
    being written and nowhere else, which also marks where one starts and stops.

    {1 Where to read next}

    {!P} for what a world is made of. {!Element} for what a component is and the
    one rule it asks of you. {!Hook} for state. {!Events} for time and input.
    {!Check} for what is wrong with a level before anyone walks into it. {!Run}
    for putting it on a window. *)

(** {1 Describing a world} *)

module P = P
module Element = Camlcast_loom.Element
module Hook = Camlcast_loom.Hook
module Context = Camlcast_loom.Context
module Store = Camlcast_loom.Store
module Events = Events

(** {1 Running one} *)

module Run = Run
module Check = Check
module Scene = Scene
module Mount = Mount
module Debug_map = Debug_map

(** {1 The seam}

    Not the platform — these are this library's own, and a game meets them at
    its edges. {!Host.Malformed} is what a description that could not be a world
    raises, {!Prim} is what a {!Scene} is made of, {!Overlay} is what turns the
    last of those into pixels, and {!Aim} is everything an interacting frame
    does — written as a function of values so that a game can drive it in a test
    without opening a window. *)

module Prim = Prim
module Host = Host
module Overlay = Overlay
module Aim = Aim

(** {1 The things a description is made of}

    The engine holds no content — not one colour, pattern, picture or room. What
    it has instead are the types those things are values of, so a game supplies
    its own and two games can share an engine without sharing a look. *)

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

(** {1 Controls} *)

module Key = Camlcast_core.Key
module Input = Camlcast_core.Input
module Binding = Camlcast_core.Binding
module Config = Camlcast_core.Config

(** {1 What is under a room and over it}

    {!Camlcast_core.Room} is not re-exported — most of it is the platform's
    business — so the three things about a room a description does name are
    here, and {!P.floor}, {!P.roof} and {!P.open_sky} are how they are made. *)

type surface = Camlcast_core.Room.surface = {
  plane : Plane.t;
  material : Material.t;
}
(** What a floor or a ceiling is: where it is, and what it is made of. *)

type ceiling = Camlcast_core.Room.ceiling =
  | Roof of surface  (** an inclined plane overhead, of some material *)
  | Open of Sky.t  (** nothing overhead, and which sky shows instead *)

type side = Camlcast_core.Room.side =
  | Front
  | Back
      (** Which face of a wall a decal is on. {!Front} is the inside of the
          room. *)
