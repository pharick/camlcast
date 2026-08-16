(** 8-bit RGB colours. *)

type t = { r : int; g : int; b : int }
(** Concrete, and not clamped by construction. A colour produced by arithmetic
    may leave 0 .. 255 and be brought back with {!clamp}; {!Texture.generate}
    does exactly that with whatever a pattern function returns. Making this
    abstract would enforce an invariant the module deliberately does not hold.
*)

val rgb : int -> int -> int -> t
(** [rgb r g b] is that colour, red green blue, each nominally 0 .. 255. The
    three are stored as given: this is the one constructor that does {e not}
    clamp, for the reason stated on the type above. *)

val shade : t -> float -> t
(** [shade colour factor] multiplies every channel of [colour] by [factor]: [0.]
    is black, [1.] is unchanged, above [1.] brightens. The result is clamped, so
    a large factor saturates to white rather than overflowing.

    Used for shading a wall by how squarely it faces the light, and for taking
    the haze's share of a pixel at a fraction of full strength. {e Not} for
    distance fog: fog fades a surface towards the colour of the air rather than
    towards black, which is a {!lerp} — see {!Atmosphere.fog}. *)

val clamp_channel : int -> int
(** Clamps one channel: an integer brought back into 0 .. 255.

    This is the engine's only byte clamp; it lives here because a colour channel
    is the usual meaning of a byte in this codebase. Alpha is the exception and
    uses it too — {!Image.make} and {!Texture.generate_masked} clamp the alpha
    their function returns exactly as they clamp colour, and a second function
    differing only in name would be a second thing to keep correct.

    This was four copies before it was one: this function, plus a private
    spelling each in {!Texture}, {!Image} and {!Renderer}, two of them with the
    [min] and the [max] the other way round and one written as a pair of [if]s.
    All four agreed, but a divergence would not have been detected. *)

val clamp : t -> t
(** Every channel clamped back into 0 .. 255, via {!clamp_channel}. A colour
    produced by arithmetic can leave the range at either end;
    {!Texture.generate} clamps what a pattern function returns for exactly that
    reason. *)

val level : t -> int -> t
(** [level colour amount] is [colour] scaled to [amount] out of 255: the integer
    counterpart of {!shade}, where 255 is unchanged and 0 is black. [amount] is
    itself clamped into 0 .. 255 first, so a pattern that overshoots saturates
    rather than wrapping.

    Surface patterns are written in terms of this function. {!Texture.noise} and
    the hashes beside it work in 0 .. 255, so a pattern is a function giving
    {e how much} of a surface's colour reaches the eye at each texel; [level]
    turns that answer back into a colour. Scaling all three channels together
    changes value without touching hue, which lets one pattern work at any
    colour it is given. *)

val lerp : t -> t -> float -> t
(** [lerp a b amount] is the linear blend between the two colours: [0.] gives
    [a], [1.] gives [b], and intermediate values mix proportionally. [amount] is
    not clamped, so a value outside 0 .. 1 extrapolates past one end — but the
    resulting channels are clamped, so the result saturates rather than
    overflowing. Used for the sky gradient and its sun, and for distance fog,
    which carries a surface towards the air's own {!Atmosphere.haze} rather than
    towards black. *)
