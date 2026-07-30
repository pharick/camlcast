(** Drawing over a finished frame: the shapes an interface is made of, and the
    clipping that keeps them on the screen.

    {!Framebuffer.set} and {!Framebuffer.blend} write without checking where —
    the renderer's own loops have clipped long before they call them, and the
    check would cost more than everything else in the inner loop — so everything
    here clips first. That is the whole of what this module is for: a caller
    gives a rectangle in framebuffer coordinates and does not have to know how
    big the framebuffer is, which matters because it changes with the window.

    Shapes only. Text is {!Font}, which draws through {!sub} here. What any of
    it {e means} — a lamp running down, a page of a journal — belongs to the
    game, the same way a {!Material} does.

    Two conventions hold throughout. Coordinates are framebuffer pixels from a
    top-left origin, with [y] growing downwards; and colour arrives as a
    {!Color.t}, since a shape is one colour and its caller usually has the
    value already. (The loose [~r ~g ~b] spelling lives on in
    {!Framebuffer.set} and {!Framebuffer.blend}, the per-pixel calls these
    clip for, where a record per pixel would be paid in the inner loop.)
    Everything is clipped, so a shape that falls partly or wholly outside the
    buffer draws what fits and no more, without raising. *)

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
    [alpha] is out of 255, and 255 writes the pixel outright rather than
    blending it with itself. A [w] or [h] of zero or less draws nothing. *)

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
    per-pixel alpha and all.

    The clipping is done on the {e destination} and then read back into the
    source, so a picture half off the left edge draws its right half rather than
    the whole thing squashed or nothing at all. It is clipped to the picture at
    both edges too: an [sw] or [sh] running past the far one stops there, and a
    negative [sx] or [sy] starts at the picture's own corner instead of before
    it, leaving that much of the destination untouched. A rectangle wholly
    outside the picture draws nothing.

    [tint] multiplies the picture's own colour channel by channel if it is
    given, which is what {!Font} uses to draw one white atlas in any colour a
    screen asks for; omitted, the picture's colours are used as they stand.

    A pixel with zero alpha costs a comparison and no write, so a cut-out
    picture — which is most of them — is cheap over the parts that are not
    there. *)

val image : ?tint:Color.t -> Framebuffer.t -> Image.t -> x:int -> y:int -> unit
(** [image buffer picture ~x ~y] is the whole of [picture] with its top-left
    corner at [(x, y)] — {!sub} over all of it, [tint] and clipping alike. *)

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
    [(x, y)]: a dark trough with a bright fill over it in [color]. The fill
    grows rightwards from the left edge.

    [fraction] is clamped to 0 .. 1, so a meter cannot overrun its box however
    it is arrived at. The trough is drawn one pixel proud on every side, so this
    paints a box two pixels wider and taller than the one it was given — leave
    that much room around it. *)

val line :
  Framebuffer.t ->
  x0:int -> y0:int -> x1:int -> y1:int -> color:Color.t -> unit
(** A line from [(x0, y0)] to [(x1, y1)], both ends included, walked in whole
    pixels. Two identical endpoints draw the single pixel there. Good enough to
    draw round something with; it is not what draws the something.

    Costs the part of the segment that is on the buffer, not the segment: the
    walk is narrowed to the range that could land before it starts, rather than
    clipping pixel by pixel along the whole of it. Endpoints far outside are
    therefore ordinary rather than ruinous, which is what a caller passing
    projected coordinates needs — those divide by distance, and something close
    to the eye projects to a shape millions of pixels across. *)

val ring : Framebuffer.t -> (int * int) list -> color:Color.t -> unit
(** The outline of a shape given as its corners in order, each an [(x, y)],
    joined up and closed back to the first. A rectangle on the screen and a
    decal's trapezoid are both this. No corners draws nothing and one corner
    draws that pixel, so neither is a case to guard against. *)

val crosshair : Framebuffer.t -> color:Color.t -> unit
(** A cross of two eleven-pixel arms in the middle of the buffer, wherever the
    window has been resized to: the overlay is drawn in the buffer's own
    coordinates, and it changes size with the window. Takes no position — the
    middle is the whole point of it.

    Which pixel the middle is matters, because {!Sight} answers about the ray
    the crosshair sits on and a player aims by what is drawn. It is [width / 2]
    and [height / 2], the pixel {e containing} the middle of the buffer, and
    under {!Viewport}'s rule that a pixel is its centre that is also the pixel
    whose centre is nearest to it: at an odd size exactly the straight-ahead
    ray, and at an even one either of the two half a pixel from it, the middle
    having fallen on a boundary where no pixel is the centre. Round it the other
    way and odd sizes stop being exact for nothing gained. *)
