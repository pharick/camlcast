(** Drawing over a finished frame: the shapes an interface is made of, and the
    clipping that keeps them on the screen.

    {!Framebuffer.set} and {!Framebuffer.blend} write without bounds checks —
    the renderer's own loops have clipped long before calling them, and the
    check would cost more than everything else in the inner loop — so everything
    here clips first. That is the module's purpose: a caller gives a rectangle
    in framebuffer coordinates without knowing how big the framebuffer is, which
    matters because the size changes with the window.

    Shapes only. Text is {!Font}, which draws through {!sub} here. What any of
    it {e means} — a lamp running down, a page of a journal — belongs to the
    game, as with a {!Material}.

    Two conventions hold throughout. Coordinates are framebuffer pixels from a
    top-left origin, with [y] growing downwards. Colour arrives as a {!Color.t},
    since a shape is one colour and its caller usually has the value already.
    (The loose [~r ~g ~b] spelling survives in {!Framebuffer.set} and
    {!Framebuffer.blend}, the per-pixel calls these clip for, where a record per
    pixel would cost in the inner loop.) Everything is clipped, so a shape
    partly or wholly outside the buffer draws what fits and no more, without
    raising.

    {b Everything is also clamped}, for the same reason. {!Color.rgb} does not
    hold its channels to 0 .. 255 — deliberately, so a colour reached by
    arithmetic can be carried around before being clamped — while
    {!Framebuffer.set} takes one already in range and stores it in a byte. A
    channel outside the range therefore does not saturate down there: it wraps,
    so a red brightened to 280 is drawn at 24, darker than it started. Every
    colour and every alpha arriving here is clamped once, outside the loop,
    before reaching a pixel. Saturating is what the rest of the engine does with
    the same overshoot — {!Color.clamp}, {!Color.shade}, {!Image.make},
    {!Texture.generate} — and a game may arrive at a colour by arithmetic
    exactly as they all assume. *)

val rect :
  Framebuffer.t ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  color:Color.t ->
  alpha:int ->
  unit
(** A filled rectangle [w] by [h] pixels with its top-left corner at [(x, y)].
    [alpha] is out of 255; 255 writes the pixel outright rather than blending it
    with itself. Clamped, so an alpha reached by arithmetic cannot wrap the
    pixels underneath: at or below 0 draws nothing, at or above 255 is solid.
    [color] is clamped too, channel by channel. A [w] or [h] of zero or less
    draws nothing. *)

val sub :
  ?tint:Color.t ->
  Framebuffer.t ->
  Image.t ->
  x:int ->
  y:int ->
  sx:int ->
  sy:int ->
  sw:int ->
  sh:int ->
  unit
(** [sub buffer picture ~x ~y ~sx ~sy ~sw ~sh] blits the [sw] by [sh] rectangle
    of [picture] whose top-left corner is [(sx, sy)] onto [buffer] at [(x, y)],
    with per-pixel alpha.

    Clipping is done on the {e destination} and then read back into the source,
    so a picture half off the left edge draws its right half rather than the
    whole thing squashed, or nothing. It is also clipped to the picture at both
    edges: an [sw] or [sh] running past the far edge stops there, and a negative
    [sx] or [sy] starts at the picture's own corner, leaving that much of the
    destination untouched. A rectangle wholly outside the picture draws nothing.

    [tint], if given, multiplies the picture's colour channel by channel; this
    is how {!Font} draws one white atlas in any colour a screen asks for.
    Omitted, the picture's colours are used as-is. [tint] is clamped before it
    multiplies anything, since it is the one number here a game supplies — a
    picture's own channels are already in range, {!Image.make} having clamped
    them, so an in-range tint keeps the product in range.

    A pixel with zero alpha costs a comparison and no write, so a cut-out
    picture — which is most of them — is cheap over the transparent parts. *)

val image : ?tint:Color.t -> Framebuffer.t -> Image.t -> x:int -> y:int -> unit
(** [image buffer picture ~x ~y] is the whole of [picture] with its top-left
    corner at [(x, y)]: {!sub} over all of it, [tint] and clipping included. *)

val bar :
  Framebuffer.t ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  fraction:float ->
  color:Color.t ->
  unit
(** A meter [fraction] full, in a box [w] by [h] with its top-left corner at
    [(x, y)]: a dark trough with a bright fill in [color]. The fill grows
    rightwards from the left edge.

    [fraction] is clamped to 0 .. 1, so a meter cannot overrun its box. The
    trough is drawn one pixel proud on every side, so this paints a box two
    pixels wider and taller than the one given — leave that much room around it.
*)

val line :
  Framebuffer.t -> x0:int -> y0:int -> x1:int -> y1:int -> color:Color.t -> unit
(** A line from [(x0, y0)] to [(x1, y1)], both ends included, walked in whole
    pixels. Two identical endpoints draw the single pixel there. Suitable for
    outlining something; it is not what draws the something.

    Cost is proportional to the part of the segment on the buffer, not the whole
    segment: the walk is narrowed to the range that could land before it starts,
    rather than clipping pixel by pixel. Endpoints far outside are therefore
    cheap, which a caller passing projected coordinates needs — projection
    divides by distance, and something close to the eye projects to a shape
    millions of pixels across. *)

val ring : Framebuffer.t -> (int * int) list -> color:Color.t -> unit
(** The outline of a shape given as its corners in order, each an [(x, y)],
    joined and closed back to the first. A rectangle on the screen and a decal's
    trapezoid are both drawn this way. No corners draws nothing and one corner
    draws that pixel, so neither needs guarding against. *)

val crosshair : Framebuffer.t -> color:Color.t -> unit
(** A cross of two eleven-pixel arms in the middle of the buffer, at whatever
    size the window has been resized to: the overlay is drawn in the buffer's
    own coordinates, which change with the window. Takes no position — the
    middle is the point.

    Which pixel is the middle matters, because {!Sight} answers about the ray
    the crosshair sits on and the player aims by what is drawn. It is
    [width / 2] and [height / 2], the pixel {e containing} the middle of the
    buffer. Under {!Viewport}'s rule that a pixel is its centre, that is also
    the pixel whose centre is nearest the middle: at an odd size exactly the
    straight-ahead ray, and at an even one either of the two half a pixel from
    it, the middle having fallen on a boundary where no pixel is the centre.
    Rounding the other way would make odd sizes inexact for no gain. *)
