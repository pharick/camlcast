(** The primitives a world is described with — this engine's [div] and [span].

    Every one of these is an inert description: what to build, never a built
    thing that something is holding on to. {!Host.assemble} is what turns a
    frame's worth of them into a {!Camlcast.World.t}, and a game never sees that
    happen.

    They carry parameters rather than assembled pieces wherever a child affects
    how the piece is made — a {!Wall} keeps its endpoints because the decals
    hung on it arrive as its children and have to be there when
    {!Camlcast.Room.val-wall} is finally called. Where nothing nests, the
    assembled value is carried directly, because there is nothing left to
    decide.

    A game does not name these. {!Parts} does that, and its constructors are
    what a world is written with. *)

open Camlcast

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
  | Wall of { a : Vec.t; b : Vec.t; height : float; material : Material.t }
      (** one segment of a boundary. Its children are the {!Decal}s on it. *)
  | Decal of Room.decal  (** a picture hung on the wall that contains it *)
  | Threshold of Room.threshold
      (** a doorway cut into a room's boundary, named so that {!Link} can join
          it to another *)
  | Sprite of Room.sprite  (** a billboard standing in the room that holds it *)
  | Camera of camera
      (** where the eye is, when a description would rather say than let the
          runtime walk it *)
  | Finish
      (** the description saying it is over. Present in a frame, the run ends
          after it. *)
  | Link of { here : string * string; there : string * string }
      (** two thresholds, each named by its room and its own name, that are the
          same doorway seen from either side *)

val describe : t -> string
(** A short phrase naming this primitive, for a {!Camlcast_loom.Trace}. *)
