(** Timing source for the main loop. Reading the clock requires SDL; the
    arithmetic on its readings does not. Both live here together, away from the
    window code. *)

val now : unit -> float
(** Current time in seconds. Uses SDL's high-resolution counter, not its
    millisecond counter: a frame is only about sixteen milliseconds long, so
    counting in whole milliseconds would quantise it badly.

    Only differences between two readings are meaningful; the counter's origin
    is defined by SDL and unspecified here. *)

val frame_time : previous:float -> now:float -> float
(** Seconds the frame starting at [now] should advance the simulation, given the
    previous frame started at [previous]. Speeds are quoted per second (see
    {!Config}), so measuring the frame keeps player speed identical on slow and
    fast machines.

    A frame longer than {!Config.max_frame_time} is capped at that value. Such
    frames come from the program being stalled (window dragged, machine
    swapped), not from the world moving; honouring one would move the player
    further in a single step than the collision tests handle. If the clock went
    backwards, the result is zero, not a negative frame. *)

val idle_time : spent:float -> float
(** Seconds left of {!Config.frame_budget} for a frame that has spent [spent]
    seconds so far — the time to sleep before starting the next frame. A frame
    that overran its budget gets zero: it is already late, and {!frame_time}
    keeps the simulation in pace with it rather than slowing down. *)
