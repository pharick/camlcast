(** 8-bit RGB colours. *)

type t = { r : int; g : int; b : int }
(** Concrete, and not clamped by construction. A colour reached by arithmetic is
    allowed to leave 0 .. 255 and be put back with {!clamp} —
    {!Texture.generate} does exactly that with whatever a pattern function hands
    it — so making this abstract would buy an invariant the module deliberately
    does not hold. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] is that colour, red green blue, each nominally 0 .. 255. The
    three are stored as given: this is the one constructor that does {e not}
    clamp, for the reason the type above gives. *)

val shade : t -> float -> t
(** [shade colour factor] multiplies every channel of [colour] by [factor] —
    [0.] is black, [1.] leaves it alone, above [1.] brightens. The result is
    clamped, so a large factor saturates to white rather than overflowing.

    Used for shading a wall by how squarely it faces the light, and for taking
    the haze's share of a pixel at a fraction of full strength. {e Not} for
    distance fog: that fades a surface towards the colour of the air rather than
    towards black, which is a {!lerp} — see {!Atmosphere.fog}. *)

val clamp_channel : int -> int
(** One channel's worth of the same: an integer brought back into 0 .. 255.

    The engine's only byte clamp, and it is here because a colour channel is
    what a byte usually is around here. Alpha is the exception and uses it too —
    {!Image.make} and {!Texture.generate_masked} both clamp what their function
    hands back for alpha exactly as they clamp it for colour, and a second
    function differing only in the word would be a second thing to keep right.

    It was four copies before it was one: this, and a private spelling each in
    {!Texture}, {!Image} and {!Renderer}, two of them with the [min] and the
    [max] the other way round and one written as a pair of [if]s. All four
    agreed, which is the point — nothing had gone wrong yet, and there was no
    reason to expect to hear about it if it did. *)

val clamp : t -> t
(** Every channel clamped back into 0 .. 255, by {!clamp_channel}. A colour
    arrived at by arithmetic can leave the range at either end —
    {!Texture.generate} clamps what a pattern function hands it for exactly that
    reason. *)

val level : t -> int -> t
(** [level colour amount] is [colour] shown at [amount] out of 255: the integer
    counterpart of {!shade}, where 255 leaves the colour alone and 0 is black.
    [amount] is itself clamped into 0 .. 255 first, so a pattern that overshoots
    saturates rather than wrapping.

    This is what a surface pattern is written in terms of. {!Texture.noise} and
    the hashes beside it speak in 0 .. 255, so a pattern is naturally a function
    saying {e how much} of a surface's colour reaches the eye at each texel —
    and this turns that answer back into a colour. Scaling all three channels
    together moves value without touching hue, which is what makes one pattern
    usable at any colour it is handed. *)

val lerp : t -> t -> float -> t
(** [lerp a b amount] is the linear blend between the two colours: [0.] gives
    [a], [1.] gives [b], and half-way gives the mixture. [amount] is not
    clamped, so a value outside 0 .. 1 extrapolates past one end — but the
    channels it produces are, so the result saturates rather than running away.
    Used for the sky gradient and its sun, and for distance fog — which carries
    a surface towards the air's own {!Atmosphere.haze} rather than towards
    black. *)
