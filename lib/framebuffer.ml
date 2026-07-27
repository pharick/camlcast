(** A software framebuffer: a CPU buffer of pixels and the streaming SDL texture
    it is uploaded through once per frame.

    The sloped floor and ceiling have a colour that has to be decided
    {e per pixel} (see {!Renderer}), which the old approach of blitting whole
    wall textures on the GPU could not express. A pixel buffer does it
    naturally: the renderer writes every pixel by hand, then the whole buffer is
    handed to the GPU in one upload and scaled up to fill the window.

    The buffer is 8-bit BGRA, written a channel at a time so nothing is boxed in
    the inner loop, and the texture format is picked to match that byte order —
    see {!pixel_format}. *)

open Tsdl
open Result_ext

(** The texture format whose bytes fall in the order {!set} writes them: blue,
    green, red, alpha.

    SDL names a packed format by its channels from the most significant byte
    down, so which byte each lands in depends on the machine. [ARGB8888] puts
    blue in the lowest bits, and so first in memory, on a little-endian machine;
    [BGRA8888] does the same on a big-endian one. Naming the format the byte
    order asks for costs nothing here and saves the whole renderer from caring.
*)
let pixel_format =
  if Sys.big_endian then Sdl.Pixel.format_bgra8888
  else Sdl.Pixel.format_argb8888

type t = {
  texture : Sdl.texture option;
      (** where the buffer is uploaded to, and [None] for one that is never
          shown — see {!offscreen} *)
  pixels :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  depth : float array;
      (** per-pixel distance of the nearest opaque wall, for occluding sprites
          and see-through walls; [infinity] where only the background shows *)
  width : int;
  height : int;
}

(** The buffer itself, zeroed. Rendering a frame covers every pixel before
    anything reads one, so the clearing is not for the renderer's benefit — it
    is so that a buffer nobody has drawn into yet is black rather than whatever
    the allocator had lying there, which is what lets a test say what it
    expected. *)
let buffer ~width ~height =
  let pixels =
    Bigarray.(Array1.create int8_unsigned c_layout (width * height * 4))
  in
  Bigarray.Array1.fill pixels 0;
  {
    texture = None;
    pixels;
    depth = Array.make (width * height) infinity;
    width;
    height;
  }

let create sdl ~width ~height =
  let+ texture =
    Sdl.create_texture sdl pixel_format Sdl.Texture.access_streaming ~w:width
      ~h:height
  in
  { (buffer ~width ~height) with texture = Some texture }

(** A buffer with no window behind it: the same pixels and the same depth, drawn
    into by the same {!set} and {!blend}, and never uploaded anywhere.

    It exists so that what is drawn can be {e read back and asserted}.
    Everything downstream of this module — {!Paint}, {!Font}, and the renderer
    itself — used to be testable only through the arithmetic that fed it,
    because a real buffer needs a live SDL renderer to make its streaming
    texture and a test has no window. The texture is the only part of that which
    is true, so it is the only part that is optional. *)
let offscreen ~width ~height = buffer ~width ~height

let destroy t = Option.iter Sdl.destroy_texture t.texture

(** Reset every pixel's depth to "nothing yet", ready for a new frame. The
    colour buffer needs no clearing — the background pass covers every pixel. *)
let clear_depth t = Array.fill t.depth 0 (Array.length t.depth) infinity

(** Write one opaque pixel. [r], [g], [b] are assumed already clamped to 0 ..
    255 by the caller; the hot loops do that as part of their arithmetic. *)
let set t ~x ~y ~r ~g ~b =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  Bigarray.Array1.unsafe_set p base b;
  Bigarray.Array1.unsafe_set p (base + 1) g;
  Bigarray.Array1.unsafe_set p (base + 2) r;
  Bigarray.Array1.unsafe_set p (base + 3) 255

(** Blend one pixel over what is already there with opacity [a] (0 leaves the
    pixel untouched, 255 replaces it). This is how a translucent wall, a decal
    or a sprite lets the background — already painted, since everything is drawn
    back to front — show through. *)
let blend t ~x ~y ~r ~g ~b ~a =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  let inv = 255 - a in
  let mix dst src = ((src * a) + (dst * inv)) / 255 in
  Bigarray.Array1.unsafe_set p base (mix (Bigarray.Array1.unsafe_get p base) b);
  Bigarray.Array1.unsafe_set p (base + 1)
    (mix (Bigarray.Array1.unsafe_get p (base + 1)) g);
  Bigarray.Array1.unsafe_set p (base + 2)
    (mix (Bigarray.Array1.unsafe_get p (base + 2)) r)

(** What is at pixel [(x, y)] now. The counterpart of {!set}, and the only
    reader: the drawing loops all write and never look. It is here for the tests
    an {!offscreen} buffer makes possible — asserting what was drawn means being
    able to ask. *)
let pixel t ~x ~y =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  Color.rgb
    (Bigarray.Array1.get p (base + 2))
    (Bigarray.Array1.get p (base + 1))
    (Bigarray.Array1.get p base)

(** Upload the buffer and stretch it over [dst] (the whole window). The pitch is
    in bytes here — four per pixel — unlike the element pitch a static texture
    upload takes.

    An {!offscreen} buffer has nowhere to go, and asking it to go there is a
    mistake in the calling code rather than a condition to recover from — so it
    says so instead of quietly doing nothing. *)
let present sdl t ~dst =
  match t.texture with
  | None ->
      Error (`Msg "Framebuffer.present: this buffer has no window behind it")
  | Some texture ->
      let* () = Sdl.update_texture texture None t.pixels (t.width * 4) in
      Sdl.render_copy sdl texture ~dst
