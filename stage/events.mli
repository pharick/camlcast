(** What a component can know about the frame it is being rendered for, and how
    it says something happened.

    A description is rebuilt every frame, so time and input do not have to be
    pushed into it — they can be put in scope and read by whatever cares.
    {!Run.play} binds {!context} around the description before each render, and
    everything below is a way of asking it something.

    {1 When a handler runs}

    Never during a render. A render is meant to be a pure function of props and
    state, and a handler that set state in the middle of one would make what a
    frame shows depend on the order its components happened to be walked in. So
    {!use_frame} and {!use_key_down} put their work in a
    {!Camlcast_loom.Hook.use_effect}, which runs after the scene is assembled.

    The consequence worth knowing: a setter called from a handler shows up on
    the {e next} frame, not this one. At sixty frames a second that is sixteen
    milliseconds, and in exchange nothing can loop. *)

open Camlcast_core

type crossing = {
  from_room : string;
  from_doorway : string;
  to_room : string;
  to_doorway : string;
}
(** One doorway a frame went through, by the names a description gave it.

    By name and not by index for the reason everything else here is: a world is
    rebuilt every frame and an index is what assembling one happened to produce.
*)

type t = {
  dt : float;  (** how long the last frame lasted, in seconds *)
  motion : Input.motion;
      (** what the bindings made of the controls this frame: the walk, the turn
          and the pitch already worked out *)
  actions : Input.actions;
      (** the controls themselves — what is down, what went down this frame, and
          how long it has been held *)
  crossings : crossing list;
      (** the doorways the {e last} frame went through, in the order they were
          gone through.

          A single step can cross several — a leg is clipped at each opening and
          the rest of it carried through — so this is a list and not an option.

          Last frame's, because a description is rendered before the player is
          moved through the world it describes. One frame late, like
          {!type-t.viewport}, and for the same reason. *)
  viewport : int * int;
      (** how many pixels wide and tall the buffer is, which is what a HUD
          places itself in.

          Not the window's size. The engine renders at whatever whole-number
          fraction of the window keeps it under
          {!Camlcast_core.Config.max_render_height} and stretches the result, so
          a thousand-pixel window is commonly a five-hundred-pixel buffer.

          It is the size the {e last} frame was drawn into, because a
          description is rendered before there is a frame to measure. On the
          first frame, and for one frame after the window is resized, it is what
          it was before — which moves a HUD by a frame and has never been
          noticed by anyone. *)
}
(** One frame's worth of time and input. *)

val still : t
(** No time passed and nothing pressed. What {!use} answers outside any
    {!Run.play} — which is what a component rendered by {!Check.report}, or by a
    test that only wants the geometry, gets. *)

val context : t Camlcast_loom.Context.t
(** The context {!Run.play} binds each frame.

    Exposed because a test drives a description without a window, and binding
    this is how it says a frame went by:
    {[
    Camlcast_loom.Element.provide Events.context
      { Events.still with dt = 1. /. 60. }
      [ description ]
    ]} *)

val use : unit -> t
(** Everything about this frame, for a component that wants more than one part
    of it. *)

val use_dt : unit -> float
(** How long the last frame lasted. *)

val use_viewport : unit -> int * int
(** How big the buffer is, for a HUD that would rather sit against an edge than
    at a fixed spot. See {!type-t.viewport} for what it is the size of. *)

val use_actions : unit -> Input.actions
(** The controls as they stand, for a component asking something the hooks below
    do not cover — how long a key has been held, where the pointer is. *)

val use_frame : (dt:float -> unit) -> unit
(** Run this after every frame is drawn, with the length of it.

    Where a game puts what happens on its own: a fuse burning down, a mote
    drifting, a door swinging. Anything reached from here may set state, and the
    frame after this one will show it. *)

val use_key_down : Key.t -> (unit -> unit) -> unit
(** Run this on the frame a key goes down, and not while it is held.

    The tap, not the hold — {!use_key_held} is the other one. Walking is not
    written with either: that is the bindings' job, and it reaches a game as
    {!type-t.motion} without any component asking. *)

val use_key_held : Key.t -> bool
(** Whether a key is down right now, read during the render rather than run
    after it. *)

val use_crossings : unit -> crossing list
(** Every doorway the last frame went through. *)

val use_crossed : (crossing -> unit) -> unit
(** Run this after the frame, once for each doorway it went through.

    A trail of the way home is this and a list; a room that lights when it is
    entered is this and a name. *)
