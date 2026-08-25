(** What a component can know about the frame it is rendered for, and how it
    reports that something happened.

    A description is rebuilt every frame, so time and input do not have to be
    pushed into it. They are put in scope instead, to be read by whatever needs
    them. {!Run.play} binds {!context} around the description before each
    render; everything below is a way of reading it.

    {1 When a handler runs}

    Never during a render. A render is meant to be a pure function of props and
    state. A handler that set state in the middle of one would make a frame's
    output depend on the order its components were walked in. {!use_frame} and
    {!use_pressed} therefore put their work in a
    {!Camlcast_loom.Hook.use_effect}, which runs after the scene is assembled
    and before it is drawn.

    One consequence matters: a setter called from a handler shows up on the
    {e next} frame, not the current one. At sixty frames a second that is a
    sixteen-millisecond delay. In exchange, nothing can loop. *)

open Camlcast_core

type crossing = {
  from_room : string;
  from_doorway : string;
  to_room : string;
  to_doorway : string;
}
(** One doorway a frame went through, by the names a description gave it.

    Names, not indices, for the reason everything else here uses names: a world
    is rebuilt every frame, and an index is whatever assembling one happened to
    produce.

    [from_doorway] and [to_doorway] carry the {e opening}'s name — the one
    {!P.door} was given, which {!P.cut} puts on the gap it cuts. The core calls
    that field a threshold; this module calls it a doorway, the ordinary word
    for it everywhere a game can see. The two terms are only distinct down in
    {!Camlcast_core.Room}, which states the distinction. *)

type t = {
  dt : float;  (** how long the last frame lasted, in seconds *)
  motion : Input.motion;
      (** what the bindings made of the controls this frame: the walk, the turn
          and the pitch, already worked out *)
  actions : Input.actions;
      (** the controls themselves: what is down, what went down this frame, and
          how long it has been held *)
  crossings : crossing list;
      (** the doorways the {e last} frame went through, in the order they were
          crossed.

          A single step can cross several: a leg is clipped at each opening and
          the rest of it carried through. So this is a list, not an option.

          Last frame's, because a description is rendered before the player is
          moved through the world it describes. One frame late, like
          {!type-t.viewport}, and for the same reason.

          {b Empty while a description is placing the camera.} A placed
          {!P.camera} is put where the description says each frame, which is a
          jump and not a walk. There is no path along which it could have
          crossed anything, and any doorway between one frame's position and the
          next would be a guess, not an event. A cutscene that wants to announce
          where it has reached knows where it put the camera and can say so
          directly. *)
  aim : Aim.spot option;
      (** what the crosshair was on when the {e last} frame was drawn, and where
          on it.

          For a description that shows something about whatever is being looked
          at, without the thing itself having to take part — a prompt, a name, a
          colour that changes. Where the thing itself should react, {!P.wall}'s
          [on_gaze] is the better answer: it is notified, while this has to be
          read.

          One frame late, like {!type-t.viewport}, and for the same reason: a
          description is rendered before the player has moved through the world
          it describes. *)
  viewport : int * int;
      (** how many pixels wide and tall the buffer is — the box a HUD places
          itself in.

          Not the window's size. The engine renders at whatever whole-number
          fraction of the window keeps it under
          {!Camlcast_core.Config.max_render_height} and stretches the result, so
          a thousand-pixel window is commonly a five-hundred-pixel buffer.

          It is the size the {e last} frame was drawn into, because a
          description is rendered before there is a frame to measure. On the
          first frame, and for one frame after the window is resized, it is the
          previous value. That shifts a HUD by one frame; the shift has never
          been noticed. *)
}
(** One frame's worth of time and input. *)

val still : t
(** No time passed and nothing pressed. This is what {!use} answers outside any
    {!Run.play} — what a component rendered by {!Check.report}, or by a test
    that only wants the geometry, gets. *)

val context : t Camlcast_loom.Context.t
(** The context {!Run.play} binds each frame.

    Exposed so a test can drive a description without a window. Binding this is
    how the test says a frame went by:
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

val use_aim : unit -> Aim.spot option
(** What the crosshair was on when the last frame was drawn. *)

val use_viewport : unit -> int * int
(** How big the buffer is, for a HUD that sits against an edge rather than at a
    fixed spot. See {!type-t.viewport} for what it is the size of. *)

val use_actions : unit -> Input.actions
(** The controls as they stand, for a question the hooks below do not cover: how
    long a key has been held, where the pointer is. *)

val use_frame : (dt:float -> unit) -> unit
(** Run this once a frame, with the length of the frame before it.

    This is where a game puts what happens on its own: a fuse burning down, a
    mote drifting, a door swinging. Anything reached from here may set state,
    and the frame after this one will show it.

    Once a frame means: after the scene is assembled and before it is drawn.
    That is where every effect runs, and the only moment a component may reach
    outside itself. [dt] is the last frame's length because that is the last
    frame whose length is known; the current one is still being built.

    {b Strictly, once per render, and a render is not always a frame.} This is
    an effect, so it runs whenever the description is rendered, and one render
    happens before the loop starts: {!Run.on} builds the world once to find its
    spawn, and that build runs effects like any other render. A handler is
    therefore called once with [dt = 0.] before the first frame is drawn, and
    the scene from that pass is never shown — the loop asks a game for its next
    state before it draws, so the first thing on screen is the second render. A
    render outside a run fires it once as well: {!Check.report}, {!Mount.build},
    or a test that renders once.

    That extra call costs nothing if the work is scaled by [dt], since no time
    passed. Scaling by [dt] is how the work should be written anyway: a frame's
    length is not a constant, and a game that ignores it runs at the speed of
    the machine. A handler that ticks by a fixed amount instead gets one tick
    before the player exists. Nothing here prevents that: an effect that runs
    when the description is built is exactly what an effect is, and a runtime
    that picked which renders count would have to be right about {!Check.report}
    and about every test as well. *)

val use_pressed : Input.control -> (unit -> unit) -> unit
(** Run this on the frame a control goes down, and not while it is held.

    The tap, not the hold; {!use_down} is the hold. Do not write walking with
    either: walking is the bindings' job, and it reaches a game as
    {!type-t.motion} without any component asking.

    A control and not a key, so that a mouse button can do a component's job as
    well as a key can. [Input.Key Key.e] is the common case, and reads as one.
*)

val use_down : Input.control -> bool
(** Whether a control is down right now, read during the render rather than run
    after it.

    This and {!use_pressed} are named after {!Camlcast_core.Input.pressed} and
    {!Camlcast_core.Input.val-down}. They are those two questions asked of this
    frame without the frame having to be fetched first: the edge and the state,
    exactly as they are there. *)

val use_crossings : unit -> crossing list
(** Every doorway the last frame went through. *)

val use_crossed : (crossing -> unit) -> unit
(** Run this once for each doorway the last frame went through, at the same
    point in a frame that {!use_frame} runs.

    A trail of the way home is this and a list; a room that lights when it is
    entered is this and a name. *)
