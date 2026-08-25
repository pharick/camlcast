(** The primitives a world is described with — this engine's [div] and [span].

    Every one of these is an inert description: what to build, never a built
    thing that something is holding on to. {!Host.assemble} turns a frame's
    worth of them into a {!Camlcast_core.World.t}, and a game never sees that
    happen.

    A primitive carries parameters rather than assembled pieces wherever a child
    affects how the piece is made. A {!Wall} keeps its endpoints because the
    decals hung on it arrive as its children, and they have to be present when
    {!Camlcast_core.Room.val-wall} is finally called. Where nothing nests, the
    assembled value is carried directly, because there is nothing left to
    decide.

    A game does not name these. {!P} does that, and its constructors are what a
    world is written with. *)

open Camlcast_core

type reacts = {
  on_gaze : (bool -> unit) option;
  on_use : (Aim.spot -> unit) option;
}
(** What a thing in the world asked to be told about the crosshair. Carried by
    the three primitives {!Camlcast_core.Sight} can land on, and nothing else:
    an eye stops on a wall, a sprite or a doorway, and never on a decal or a
    room. *)

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
(** Where a description says the eye is. Its own record rather than an inline
    one, because {!Host} resolves it after the world exists and passes it about
    while it does. *)

type t =
  | World of { atmosphere : Atmosphere.t; spawn : (string * Vec.t) option }
      (** the root: the air every room is seen through, and where the player
          starts. Exactly one of these per description. *)
  | Room of {
      name : string option;
      floor : surface;
      ceiling : ceiling;
      height : float option;
    }
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
  | Highlight of Color.t
  | Crosshair of Color.t
  | Cursor
      (** the description asking for the pointer. While one is present in a
          frame, the mouse is loose and does not turn the camera. *)
  | Finish
      (** the description saying it is over. When one is present in a frame, the
          run ends after that frame. *)
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
      (** an opening cut into one leg of the room that holds it. [along] names
          that leg by its two corners, in either order — the leg's own winding
          is what the opening takes, which is why it cannot be given backwards.
          [id] is the identity a {!Connect} joins it by. *)
  | Connect of int * int
      (** two {!constructor-Door}s, by [id], that are the same opening seen from
          either side. A child of the world, so that joining two rooms and
          unjoining them touches neither. *)
  | Spawn of Vec.t
      (** where the player starts, in the coordinates of the room that holds it
      *)
  | Link of { here : string * string; there : string * string }
      (** two thresholds, each named by its room and its own name, that are the
          same doorway seen from either side *)

val point : Vec.t -> string
(** A point, as a diagnostic spells one: ["(1,-2)"]. Shared so that two
    complaints about the same coordinates read the same way. *)

val misplaced : child:t -> parent:t -> string
(** What to say about a nesting {!may_contain} refused:
    ["a sprite (0,0) cannot go in a world"]. One phrase names the thing, another
    names the place, and this supplies the one sentence they go in.

    The sentence lives here for the reason the rule below does — the same
    reason, a third time. {!Host.assemble} and {!Check.report} both report this
    offence, and were they to word it separately —
    ["… does not belong in a world"] against ["a … cannot go in a world"] — a
    game developer meeting one and then the other would have no way to tell they
    had been told the same thing twice. Sharing those two phrases is not enough
    to prevent it: the verb is where two wordings of one offence part company,
    and it is the part nobody thinks to share. *)

val not_a_world : t -> string
(** What to say about a description whose one root is something else:
    ["a wall (0,0)-(1,0) is not a world"]. The same sentence shared for the same
    reason; the two readers had come within an article of agreeing on it. *)

val may_contain : parent:t -> child:t -> bool
(** Whether that nesting means anything.

    One statement of the rule, because there are two readers and they must not
    drift. {!Host.assemble} raises on the first thing that is out of place;
    {!Check.report} collects every one of them with the component that wrote it.
    Those are two different jobs and one rule, and the rule is here.

    {b Sharing the rule is not enough on its own.} Both readers also have to ask
    it about the same nodes, with the same parent for each. Once they did not,
    and they came to disagree about descriptions that only [Element] and this
    module can build. They now walk a description through one traversal, which
    is internal to the library and governed by this rule. Asking [may_contain]
    while doing something else, at whichever nodes that something else happens
    to reach, is what produced the drift; do not read the rule that way. *)
