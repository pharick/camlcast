(** A declarative raycasting engine.

    A game describes what its world should be, every frame, and the runtime
    works out what changed. Parts of a world are components with props, state
    and events; components compose into other components. The framebuffer, the
    renderer and the world itself are behind this module.

    {1 Everything a game opens}

    This one module. It contains what a description is made of. It excludes the
    platform underneath: {!Camlcast_core.Engine}, {!Camlcast_core.Renderer},
    {!Camlcast_core.Framebuffer}, {!Camlcast_core.World},
    {!Camlcast_core.Player}. Those stay reachable by adding [camlcast.core] to a
    dune file, so crossing the boundary is a decision with a diff rather than
    something autocomplete finds.

    {1 The shape of a game}

    Every line below is a line of [examples/step08_state.ml], a program the
    default build compiles; [test/test_docs.ml] holds the two together, so a
    sample on this page cannot outlive the API it was written for.

    {[
    open Camlcast

    let brazier =
      Element.declare ~name:"brazier" @@ fun (pos : Vec.t) ->
      let lit, set_lit = Hook.use_state false in
      Events.use_pressed (Input.Key Key.space) (fun () -> set_lit true);
      P.sprite ~size:0.9
        ~glow:(if lit then 0.85 else 0.)
        ~image:(if lit then brazier_hot else brazier_cold)
        pos

    let level =
      P.(
        world ~atmosphere:air
          [
            room ~height ~material:stone ~floor:(floor ~plane:flat ground)
              ~ceiling:(roof stone)
              ~outline:(corners [ c_sw; c_se; c_ne; c_nw ])
              [
                spawn (Vec.make (-4.5) 0.);
                brazier (Vec.make 0. 0.);
              ];
          ])

    let () =
      match Run.play ~title:"The Undercroft" level with
      | Ok _ending -> ()
      | Error (`Msg message) ->
          prerr_endline message;
          exit 1
    ]}

    A room is a closed [outline] of corners with its contents inside it, and
    where the player starts is a [spawn] among those contents rather than a room
    named from outside. A doorway is a [door] made once and [cut] into a leg of
    the outline it stands in; two of them joined by a [connect] are one opening.

    {!P} is written [P.( ... )] around a description rather than opened over a
    whole file. Its names are short and ordinary — [wall], [room], [text],
    [spawn] — and a local open puts them in scope exactly where a description is
    written and nowhere else. The open also marks where a description starts and
    stops.

    {1 Where to read next}

    {!P} for what a world is made of. {!Element} for what a component is and the
    one rule it imposes. {!Hook} for state. {!Events} for time and input.
    {!Check} for finding what is wrong with a level before running it. {!Run}
    for putting it on a window. *)

(** {1 Describing a world} *)

module P = P

module Element = Camlcast_loom.Element
(** The hooks a component may call, {e without} the
    {!Camlcast_loom.Hook.Runtime} half {!Camlcast_loom.Reconcile} drives them
    with. Out of reach on the same terms as {!Input}'s runtime half, for a
    milder version of the same reason. *)

module Hook :
  module type of Camlcast_loom.Hook
    with module Runtime := Camlcast_loom.Hook.Runtime

module Context = Camlcast_loom.Context
module Store = Camlcast_loom.Store
module Events = Events

(** {1 Running one} *)

module Run = Run
module Controls = Controls
module Watch = Watch
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

(** {1 What the player is doing}

    The platform's own vocabulary for it: places on a keyboard, the controls a
    frame reports, and the table from those to walking. What a {e run} does with
    them by itself is {!Controls}, above. *)

module Key = Camlcast_core.Key
(** A frame of what the player is doing, {e without} its
    {!Camlcast_core.Input.Runtime} half.

    That half is the loop's — draining SDL's event queue, reading and clearing
    its accumulated mouse motion — and two of the six do damage from a game:
    reading the motion leaves the loop's own read empty and the camera still,
    and draining the queue swallows the quit event. Sitting in this namespace
    beside {!Input.pressed}, they would be guarded by a docstring asking
    politely.

    Removed here rather than renamed there, so it is out of reach on the same
    terms as {!Camlcast_core.Engine} and {!Camlcast_core.World}: nothing stops a
    game adding [camlcast.core] and naming it, and that is a line in a dune file
    rather than something autocomplete offers. *)

module Input :
  module type of Camlcast_core.Input
    with module Runtime := Camlcast_core.Input.Runtime

module Binding = Camlcast_core.Binding
module Config = Camlcast_core.Config

(** {1 What is under a room and over it}

    {!Camlcast_core.Room} is not re-exported — most of it is the platform's
    business — so the three things about a room a description does name are
    here, and {!P.floor}, {!P.roof} and {!P.open_sky} are how they are made. *)

type lintel = Camlcast_core.Room.lintel = {
  top : float;  (** how far the wall a door was cut into rises *)
  material : Material.t;  (** and what the strip above the opening is made of *)
}
(** The wall over an opening.

    A {!P.cut} takes one where the strip above should not be made of the wall it
    was cut into — a brick transom over a stone jamb. It is given at the cut and
    not on the {!P.type-door} because it is how {e this} room presents the
    opening, and two rooms are allowed to present one differently. *)

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
