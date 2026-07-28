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
    game, the same way a {!Material} does. *)

val rect :
  Framebuffer.t ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  r:int ->
  g:int ->
  b:int ->
  alpha:int ->
  unit
(** A filled rectangle. [alpha] is out of 255, and 255 writes the pixel outright
    rather than blending it with itself. *)

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
(** Blit the [sw] x [sh] rectangle of [img] whose top-left corner is [(sx, sy)]
    onto the buffer at [(x, y)], per-pixel alpha and all.

    The clipping is done on the {e destination} and then read back into the
    source, so a picture half off the left edge draws its right half rather than
    the whole thing squashed or nothing at all. [tint] multiplies the image's
    own colour if it is given, which is what {!Font} uses to draw one white
    atlas in any colour a screen asks for.

    A pixel with zero alpha costs a comparison and no write, so a cut-out
    picture — which is most of them — is cheap over the parts that are not
    there. *)

val image : ?tint:Color.t -> Framebuffer.t -> Image.t -> x:int -> y:int -> unit
(** The whole of a picture, at [(x, y)]. *)

val bar :
  Framebuffer.t ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  fraction:float ->
  r:int ->
  g:int ->
  b:int ->
  unit
(** A meter [fraction] full: a dark trough with a bright fill over it. *)

val line :
  Framebuffer.t ->
  x0:int ->
  y0:int ->
  x1:int ->
  y1:int ->
  r:int ->
  g:int ->
  b:int ->
  unit
(** A line between two points, walked in whole pixels. Good enough to draw round
    something with; it is not what draws the something. *)

val ring : Framebuffer.t -> (int * int) list -> r:int -> g:int -> b:int -> unit
(** The outline of a shape given as corners, joined up and closed. A rectangle
    on the screen and a decal's trapezoid are both this. *)

val crosshair : Framebuffer.t -> r:int -> g:int -> b:int -> unit
(** A cross in the middle of the buffer, wherever the window has been resized
    to: the overlay is drawn in the buffer's own coordinates, and it changes
    size with the window. *)
