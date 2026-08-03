(** The clock the loop paces itself by. Reading it needs SDL; the arithmetic
    done to what it reports does not, which is why the two live here together
    and away from the window. *)

val now : unit -> float
(** The time now, in seconds. SDL's high resolution counter, rather than its
    millisecond one: a frame is only some sixteen milliseconds long, so counting
    in whole milliseconds would quantise it badly.

    Only differences between two readings mean anything; where it counts from is
    SDL's business. *)

val frame_time : previous:float -> now:float -> float
(** How long, in seconds, the frame starting at [now] should advance the
    simulation by, given that the previous one started at [previous]. Speeds are
    quoted per second (see {!Config}), so measuring the frame is what keeps the
    player walking at the same pace on a machine that renders slowly as on one
    that races.

    A frame longer than {!Config.max_frame_time} is capped at it. Those come
    from the program being held up rather than from the world moving — the
    window was dragged, the machine swapped — and honouring one would move the
    player further in a single step than any collision test is meant to cope
    with. A clock that went backwards is worth nothing rather than a negative
    frame. *)

val idle_time : spent:float -> float
(** What is left of {!Config.frame_budget} for a frame that has spent [spent]
    seconds getting here — the seconds to sleep before starting the next one. A
    frame that overran its budget gets nothing: it is late already, and
    {!frame_time} has the simulation keep pace with it rather than slow down. *)
