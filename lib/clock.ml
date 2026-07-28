(** The clock the loop paces itself by. Reading it needs SDL; the arithmetic
    done to what it reports does not, which is why the two live here together
    and away from the window. *)

open Tsdl

(** The time now, in seconds. SDL's high resolution counter, rather than its
    millisecond one: a frame is only some sixteen milliseconds long, so counting
    in whole milliseconds would quantise it badly. *)
let now () =
  Int64.to_float (Sdl.get_performance_counter ())
  /. Int64.to_float (Sdl.get_performance_frequency ())

(** How long the frame starting at [now] should advance the simulation by, given
    that the previous one started at [previous]. Speeds are quoted per second
    (see {!Config}), so measuring the frame is what keeps the player walking at
    the same pace on a machine that renders slowly as on one that races.

    A frame longer than {!Config.max_frame_time} is capped at it. Those come
    from the program being held up rather than from the world moving — the
    window was dragged, the machine swapped — and honouring one would move the
    player further in a single step than any collision test is meant to cope
    with. *)
let frame_time ~previous ~now =
  Float.min Config.max_frame_time (Float.max 0. (now -. previous))

(** What is left of {!Config.frame_budget} for a frame that has spent [spent]
    seconds getting here — the time to sleep before starting the next one. A
    frame that overran its budget gets nothing: it is late already, and
    {!frame_time} has the simulation keep pace with it rather than slow down. *)
let idle_time ~spent = Float.max 0. (Config.frame_budget -. spent)
