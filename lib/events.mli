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
    {!use_frame} and {!use_pressed} put their work in a
    {!Camlcast_loom.Hook.use_effect}, which runs after the scene is assembled
    and before it is drawn.

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

    [from_doorway] and [to_doorway] are the {e opening}'s name, whichever form
    made it — {!P.doorway} and {!P.threshold} both take one and both put it on
    the same place. The core calls that field a threshold and this calls it a
    doorway, which is the ordinary word for it everywhere a game can see; the
    two are only distinct where those two constructors are, and
    {!Camlcast_core.Room} is where that is written down. *)

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
          {!type-t.viewport}, and for the same reason.

          {b Empty while a description is placing the camera.} A placed
          {!P.camera} is put where the description says each frame, which is a
          jump and not a walk — there is no path for it to have crossed anything
          along, and any doorway between one frame's position and the next is a
          guess rather than a thing that happened. A cutscene that wants to
          announce where it has reached knows where it put the camera and can
          say so directly. *)
  aim : Aim.spot option;
      (** what the crosshair was on when the {e last} frame was drawn, and where
          on it.

          For a description that shows something about whatever is being looked
          at without the thing itself having to say so — a prompt, a name, a
          colour that changes. Where the thing itself should react, {!P.wall}'s
          [on_gaze] is the better answer, because it is told and this has to be
          asked.

          One frame late, like {!type-t.viewport}, and for the same reason: a
          description is rendered before the player has moved through the world
          it describes. *)
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

val use_aim : unit -> Aim.spot option
(** What the crosshair was on when the last frame was drawn. *)

val use_viewport : unit -> int * int
(** How big the buffer is, for a HUD that would rather sit against an edge than
    at a fixed spot. See {!type-t.viewport} for what it is the size of. *)

val use_actions : unit -> Input.actions
(** The controls as they stand, for a component asking something the hooks below
    do not cover — how long a key has been held, where the pointer is. *)

val use_frame : (dt:float -> unit) -> unit
(** Run this once a frame, with how long the frame before it lasted.

    Where a game puts what happens on its own: a fuse burning down, a mote
    drifting, a door swinging. Anything reached from here may set state, and the
    frame after this one will show it.

    Once a frame means exactly this: after the scene has been assembled and
    before it is drawn, which is where every effect runs and the only moment a
    component may reach outside itself. And [dt] is the last frame's because
    that is the last one whose length anyone knows — this one is still being
    built.

    {b Once a render, strictly, and a render is not always a frame.} This is an
    effect, so it runs whenever the description is rendered, and one render
    happens before the loop does: {!Run.on} has to build the world once to find
    out where its spawn is, and that build runs what it owes like any other. So
    a handler is called once with [dt = 0.] before the first frame is drawn, and
    the scene that pass belonged to is never shown — the loop asks a game for
    its next state before it draws, so the first thing on the screen is already
    the second render. The same goes for a render outside a run altogether:
    {!Check.report}, {!Mount.build}, or a test that renders once, each fire this
    once.

    Which costs nothing if the work is scaled by [dt], since none passed, and
    that is how the work belongs written anyway — a frame's length is not a
    constant and a game that ignores it runs at the speed of the machine. A
    handler that ticks by a fixed amount instead gets one tick before the player
    exists. Nothing here will stop it: an effect that runs when the description
    is built is the whole of what an effect is, and a runtime picking which
    renders count would have to be right about {!Check.report} and about every
    test as well. *)

val use_pressed : Input.control -> (unit -> unit) -> unit
(** Run this on the frame a control goes down, and not while it is held.

    The tap, not the hold — {!use_down} is the other one. Walking is not written
    with either: that is the bindings' job, and it reaches a game as
    {!type-t.motion} without any component asking.

    A control and not a key, so that a mouse button can do a component's job as
    well as a key can. [Input.Key Key.e] is the common case, and reads as one.
*)

val use_down : Input.control -> bool
(** Whether a control is down right now, read during the render rather than run
    after it.

    These two are named after {!Camlcast_core.Input.pressed} and
    {!Camlcast_core.Input.val-down}, and are those two questions asked of this
    frame without the frame having to be fetched first. The edge and the state,
    exactly as they are there. *)

val use_crossings : unit -> crossing list
(** Every doorway the last frame went through. *)

val use_crossed : (crossing -> unit) -> unit
(** Run this once for each doorway the last frame went through, in the same
    place in a frame that {!use_frame} runs.

    A trail of the way home is this and a list; a room that lights when it is
    entered is this and a name. *)
