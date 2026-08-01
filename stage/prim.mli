(** The primitives a world is described with — this engine's [div] and [span].

    Every one of these is an inert description: what to build, never a built
    thing that something is holding on to. {!Host.assemble} is what turns a
    frame's worth of them into a {!Camlcast_core.World.t}, and a game never sees
    that happen.

    They carry parameters rather than assembled pieces wherever a child affects
    how the piece is made — a {!Wall} keeps its endpoints because the decals
    hung on it arrive as its children and have to be there when
    {!Camlcast_core.Room.val-wall} is finally called. Where nothing nests, the
    assembled value is carried directly, because there is nothing left to
    decide.

    A game does not name these. {!P} does that, and its constructors are what a
    world is written with. *)

open Camlcast_core

type reacts = {
  on_gaze : (bool -> unit) option;
  on_use : (unit -> unit) option;
}
(** What a thing in the world asked to be told about the crosshair. Carried by
    the three primitives {!Camlcast_core.Sight} can land on, and nothing else:
    an eye stops on a wall, a sprite or a doorway, and never on a decal or a
    room. *)

type camera = { room : string; pos : Vec.t; angle : float; pitch : float }
(** Where a description says the eye is. Its own record rather than an inline
    one, because {!Host} resolves it after the world exists and wants to pass it
    about while it does. *)

type t =
  | World of { atmosphere : Atmosphere.t; spawn : string * Vec.t }
      (** the root: the air every room is seen through, and where the player
          starts. Exactly one of these per description. *)
  | Room of { name : string; floor : Room.surface; ceiling : Room.ceiling }
      (** a room in its own coordinate frame, named so that {!Link} can find it
      *)
  | Wall of {
      a : Vec.t;
      b : Vec.t;
      height : float;
      material : Material.t;
      reacts : reacts;
    }  (** one segment of a boundary. Its children are the {!Decal}s on it. *)
  | Decal of Room.decal  (** a picture hung on the wall that contains it *)
  | Threshold of Room.threshold * reacts
      (** a doorway cut into a room's boundary, named so that {!Link} can join
          it to another *)
  | Sprite of Room.sprite * reacts
      (** a billboard standing in the room that holds it *)
  | Camera of camera
      (** where the eye is, when a description would rather say than let the
          runtime walk it *)
  | Hud
      (** the layer drawn over the finished world. A child of {!World}, and its
          own children are the things below. *)
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
  | Crosshair of Color.t
  | Finish
      (** the description saying it is over. Present in a frame, the run ends
          after it. *)
  | Link of { here : string * string; there : string * string }
      (** two thresholds, each named by its room and its own name, that are the
          same doorway seen from either side *)

val deaf : reacts
(** Asking for nothing, which is what most of a world does. *)

val describe : t -> string
(** A short phrase naming this primitive, for a {!Camlcast_loom.Trace}. *)

val inside : t -> string
(** Where this primitive's children are, said as a phrase: ["in a world"],
    ["on a wall"], ["on the hud"]. What a complaint about one of them ends with.
*)

val may_contain : parent:t -> child:t -> bool
(** Whether that nesting means anything.

    One statement of it, because there are two readers and they must not drift:
    {!Host.assemble} raises on the first thing that is out of place, and
    {!Check.report} collects every one of them with the component that wrote it.
    Those are two different jobs and one rule, and the rule is here. *)
