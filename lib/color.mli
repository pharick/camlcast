(** 8-bit RGB colours. *)

type t = { r : int; g : int; b : int }
(** Concrete, and not clamped by construction. A colour reached by arithmetic is
    allowed to leave 0 .. 255 and be put back with {!clamp} —
    {!Texture.generate} does exactly that with whatever a pattern function hands
    it — so making this abstract would buy an invariant the module deliberately
    does not hold. *)

val rgb : int -> int -> int -> t

val shade : t -> float -> t
(** Multiply every channel by [factor] (0 = black, 1 = unchanged). Used for
    distance fog and for shading walls by orientation. *)

val clamp : t -> t
(** Every channel clamped back into 0 .. 255. A colour arrived at by arithmetic
    can leave the range at either end — {!Texture.generate} clamps what a
    pattern function hands it for exactly that reason. *)

val level : t -> int -> t
(** [c] shown at [level] out of 255: the integer counterpart of {!shade}, where
    255 leaves the colour alone and 0 is black.

    This is what a surface pattern is written in terms of. {!Texture.noise} and
    the hashes beside it speak in 0 .. 255, so a pattern is naturally a function
    saying {e how much} of a surface's colour reaches the eye at each texel —
    and this turns that answer back into a colour. Scaling all three channels
    together moves value without touching hue, which is what makes one pattern
    usable at any colour it is handed. *)

val lerp : t -> t -> float -> t
(** Linear blend between two colours: [t = 0] gives [a], [t = 1] gives [b]. Used
    for the sky gradient and its sun. *)
