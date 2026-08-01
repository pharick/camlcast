(** A software framebuffer: a CPU buffer of pixels and the streaming SDL texture
    it is uploaded through once per frame.

    The sloped floor and ceiling have a colour that has to be decided
    {e per pixel} (see {!Renderer}), which the old approach of blitting whole
    wall textures on the GPU could not express. A pixel buffer does it
    naturally: the renderer writes every pixel by hand, then the whole buffer is
    handed to the GPU in one upload and scaled up to fill the window.

    The buffer is 8-bit BGRA, written a channel at a time so nothing is boxed in
    the inner loop, and the texture format is picked to match that byte order.

    A game meets one of these through the [overlay] callback of
    {!Engine.type-game}, which is handed the finished frame before it reaches
    the screen. What a game wants from it is almost always {!t.width} and
    {!t.height} — the coordinates its overlay draws in, which are the buffer's
    and not the window's — and then {!Paint} or {!Font} to draw with. *)

type t = private {
  texture : Tsdl.Sdl.texture option;
      (** where the buffer is uploaded to, and [None] for one that is never
          shown — see {!offscreen} *)
  pixels :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
      (** the pixels themselves, four bytes each in the machine's own order *)
  depth : float array;
      (** per-pixel distance of the nearest opaque wall, for occluding sprites
          and see-through walls; [infinity] where only the background shows *)
  width : int;  (** in pixels, and not the window's width *)
  height : int;  (** in pixels, and not the window's height *)
}
(** Private: every field stays readable — {!Paint} reads the extents per call
    and {!Renderer} reads [depth] per pixel — while building one from outside is
    closed off. A buffer owns an SDL texture and a bigarray sized to match its
    extents, and a hand-written record could pair either with the wrong other
    half. {!make} and {!offscreen} are the two ways to get one. *)

val make :
  Tsdl.Sdl.renderer -> width:int -> height:int -> (t, [ `Msg of string ]) result
(** A buffer of that size with a streaming texture behind it, ready to be shown.
    Needs a live SDL renderer, which is why the tests use {!offscreen} instead.
    Free it with {!destroy}.

    The [result] is the renderer's: a texture SDL would not make. A size no
    buffer could have had is the other kind of mistake and raises, on the terms
    {!offscreen} sets out — and raises before the texture is asked for, so there
    is never one left over with nobody holding it to free. *)

val offscreen : width:int -> height:int -> t
(** A buffer with no window behind it: the same pixels and the same depth, drawn
    into by the same {!set} and {!blend}, and never uploaded anywhere.

    It exists so that what is drawn can be {e read back and asserted}.
    Everything downstream of this module — {!Paint}, {!Font}, and the renderer
    itself — used to be testable only through the arithmetic that fed it,
    because a real buffer needs a live SDL renderer to make its streaming
    texture and a test has no window. The texture is the only part of that which
    is true, so it is the only part that is optional.

    Pair it with {!Renderer.draw_frame} and {!pixel} to test drawing without
    opening anything.

    @raise Invalid_argument
      if either extent is not positive, or if [width * height] is longer than a
      [float array] can be. The record keeps the extents it was asked for while
      the pixels and the depth are only as long as their product came out, and
      that product is the whole of the arithmetic {!set} and {!blend} do without
      checking. Two negative extents multiply to a positive product, and a large
      enough pair wraps to a small one; either way the buffer comes back
      claiming a size it never allocated, and every write to it lands outside
      its own memory. *)

val destroy : t -> unit
(** Free the texture behind a buffer, if it has one. An {!offscreen} buffer has
    nothing to free and this is a no-op on one. *)

val clear_depth : t -> unit
(** Reset every pixel's depth to "nothing yet", ready for a new frame. The
    colour buffer needs no clearing — the background pass covers every pixel. *)

val set : t -> x:int -> y:int -> r:int -> g:int -> b:int -> unit
(** Write one opaque pixel. [r], [g] and [b] are assumed already clamped to 0 ..
    255 by the caller; the hot loops do that as part of their arithmetic.

    {b Unchecked.} [x] and [y] must be inside the buffer. The renderer's loops
    have clipped long before they reach here and the test would cost more than
    everything else in the inner loop — which is what {!Paint} exists to do for
    a caller that has not clipped. *)

val blend : t -> x:int -> y:int -> r:int -> g:int -> b:int -> alpha:int -> unit
(** Blend one pixel over what is already there with opacity [alpha] (0 leaves
    the pixel untouched, 255 replaces it). This is how a translucent wall, a
    decal or a sprite lets the background — already painted, since everything is
    drawn back to front — show through. Unchecked, on the same terms as {!set}.
*)

val pixel : t -> x:int -> y:int -> Color.t
(** What is at pixel [(x, y)] now. The counterpart of {!set}, and the only
    reader: the drawing loops all write and never look. It is here for the tests
    an {!offscreen} buffer makes possible — asserting what was drawn means being
    able to ask. The alpha is not reported; every pixel of a finished frame is
    opaque. *)

val present :
  Tsdl.Sdl.renderer ->
  t ->
  dst:Tsdl.Sdl.rect ->
  (unit, [ `Msg of string ]) result
(** Upload the buffer and stretch it over [dst] (the whole window).

    An {!offscreen} buffer has nowhere to go, and asking it to go there is a
    mistake in the calling code rather than a condition to recover from — so it
    says so, as [`Msg], instead of quietly doing nothing. *)
