(** A software framebuffer: a CPU buffer of pixels and the streaming SDL texture
    it is uploaded through once per frame.

    The sloped floor and ceiling have a colour that has to be decided
    {e per pixel} (see {!Renderer}), which the old approach of blitting whole
    wall textures on the GPU could not express. A pixel buffer does it
    naturally: the renderer writes every pixel by hand, then the whole buffer is
    handed to the GPU in one upload and scaled up to fill the window.

    The buffer is 8-bit BGRA, which is the byte order an [ARGB8888] texture
    wants on a little-endian machine, and it is written a channel at a time so
    nothing is boxed in the inner loop. *)

open Tsdl
open Result_ext

type t = {
  texture : Sdl.texture;
  pixels :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  depth : float array;
      (** per-pixel distance of the nearest opaque wall, for occluding sprites
          and see-through walls; [infinity] where only the background shows *)
  width : int;
  height : int;
}

let create sdl ~width ~height =
  let+ texture =
    Sdl.create_texture sdl Sdl.Pixel.format_argb8888
      Sdl.Texture.access_streaming ~w:width ~h:height
  in
  {
    texture;
    pixels =
      Bigarray.(Array1.create int8_unsigned c_layout (width * height * 4));
    depth = Array.make (width * height) infinity;
    width;
    height;
  }

let destroy t = Sdl.destroy_texture t.texture

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

(** Upload the buffer and stretch it over [dst] (the whole window). The pitch is
    in bytes here — four per pixel — unlike the element pitch a static texture
    upload takes. *)
let present sdl t ~dst =
  let* () = Sdl.update_texture t.texture None t.pixels (t.width * 4) in
  Sdl.render_copy sdl t.texture ~dst
