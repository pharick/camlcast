(** A software framebuffer: a CPU buffer of pixels and the streaming SDL texture
    it is uploaded through once per frame.

    The sloped floor and ceiling need a colour decided {e per pixel} (see
    {!Renderer}), which the old approach of blitting whole wall textures on the
    GPU could not express. A pixel buffer expresses it directly: the renderer
    writes every pixel by hand, then the whole buffer is handed to the GPU in
    one upload and scaled up to fill the window.

    The buffer is 8-bit BGRA, written a channel at a time so nothing is boxed in
    the inner loop; the texture format is chosen to match that byte order.

    A game meets one of these through the [overlay] callback of
    {!Engine.type-game}, which receives the finished frame before it reaches the
    screen. A game usually needs only {!t.width} and {!t.height} — the
    coordinates its overlay draws in, which are the buffer's and not the
    window's — and then {!Paint} or {!Font} to draw with. *)

type t = private {
  texture : Tsdl.Sdl.texture option;
      (** where the buffer is uploaded to; [None] for one that is never shown —
          see {!offscreen} *)
  pixels :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
      (** the pixels, four bytes each in the machine's own byte order *)
  depth : float array;
      (** per-pixel distance of the nearest opaque wall, for occluding sprites
          and see-through walls; [infinity] where only the background shows *)
  width : int;  (** in pixels, and not the window's width *)
  height : int;  (** in pixels, and not the window's height *)
}
(** Private: every field stays readable — {!Paint} reads the extents per call
    and {!Renderer} reads [depth] per pixel — but construction from outside is
    closed off. A buffer owns an SDL texture and a bigarray sized to match its
    extents, and a hand-written record could pair either with the wrong other
    half. {!make} and {!offscreen} are the two constructors. *)

val make :
  Tsdl.Sdl.renderer -> width:int -> height:int -> (t, [ `Msg of string ]) result
(** A buffer of that size with a streaming texture behind it, ready to be shown.
    Needs a live SDL renderer, which is why tests use {!offscreen} instead. Free
    it with {!destroy}.

    The [result] carries the renderer's error: a texture SDL refused to make. A
    size no buffer could have is a different kind of mistake and raises, on the
    terms {!offscreen} states — and it raises before the texture is created, so
    a texture is never left over with no owner to free it. *)

val offscreen : width:int -> height:int -> t
(** A buffer with no window behind it: the same pixels and the same depth, drawn
    into by the same {!set} and {!blend}, never uploaded anywhere.

    It exists so that what is drawn can be read back and asserted. A real buffer
    needs a live SDL renderer for its streaming texture and a test has no
    window, which would leave everything downstream of this module — {!Paint},
    {!Font}, and the renderer itself — testable only through the arithmetic
    feeding it. The texture is the only part that truly needs SDL, so it is the
    only part that is optional.

    Pair it with {!Renderer.draw_frame} and {!pixel} to test drawing without
    opening a window.

    @raise Invalid_argument
      if either extent is not positive, or if [width * height] is longer than a
      [float array] can be. The record keeps the extents it was asked for while
      the pixels and depth arrays are only as long as the product came out, and
      that product is the whole of the arithmetic {!set} and {!blend} do without
      checking. Two negative extents multiply to a positive product, and a large
      enough pair wraps to a small one; either way the buffer would claim a size
      it never allocated, and every write would land outside its own memory. *)

val destroy : t -> unit
(** Free the texture behind a buffer, if it has one. An {!offscreen} buffer has
    nothing to free; this is a no-op on one. *)

val clear_depth : t -> unit
(** Reset every pixel's depth to "nothing yet", ready for a new frame. The
    colour buffer needs no clearing: the background pass covers every pixel. *)

val set : t -> x:int -> y:int -> r:int -> g:int -> b:int -> unit
(** Write one opaque pixel. [r], [g] and [b] are assumed already clamped to 0 ..
    255 by the caller; the hot loops clamp as part of their arithmetic. An
    unclamped value does not saturate here — a bigarray of bytes takes the low
    eight bits — so 280 is stored as 24.

    {b Unchecked}, in both senses. [x] and [y] must also be inside the buffer.
    The renderer's loops have clipped and clamped long before this point, and
    either test would cost more than everything else in the inner loop. {!Paint}
    exists to do both checks for a caller that has done neither. *)

val blend : t -> x:int -> y:int -> r:int -> g:int -> b:int -> alpha:int -> unit
(** Blend one pixel over what is already there with opacity [alpha]: 0 leaves
    the pixel untouched, 255 replaces it. This is how a translucent wall, a
    decal or a sprite lets the background — already painted, since everything is
    drawn back to front — show through. Unchecked, on the same terms as {!set}.
*)

val pixel : t -> x:int -> y:int -> Color.t
(** The current colour at pixel [(x, y)]. The counterpart of {!set}, and the
    only reader: the drawing loops all write and never read. It exists for the
    tests an {!offscreen} buffer makes possible — asserting what was drawn
    requires reading it back. Alpha is not reported; every pixel of a finished
    frame is opaque. *)

val present :
  Tsdl.Sdl.renderer ->
  t ->
  dst:Tsdl.Sdl.rect ->
  (unit, [ `Msg of string ]) result
(** Upload the buffer and stretch it over [dst] (the whole window).

    An {!offscreen} buffer has nowhere to upload to. Presenting one is a mistake
    in the calling code, not a condition to recover from, so it returns [`Msg]
    saying so instead of silently doing nothing. *)
