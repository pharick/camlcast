(* Implementation of {!Camlcast.Framebuffer}; the interface carries the prose. *)

open Tsdl
open Result_ext

let pixel_format =
  if Sys.big_endian then Sdl.Pixel.format_bgra8888
  else Sdl.Pixel.format_argb8888

type t = {
  texture : Sdl.texture option;
  pixels :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  depth : float array;
  width : int;
  height : int;
}

(* The [float array] bound and not the ordinary one: it is the tighter of the
   two this record has to satisfy. [depth] is one float per pixel, while
   [pixels] is a bigarray, outside the heap, limited only by memory. Four times
   the bound is nowhere near [max_int] at either word size, so a product that
   fits here cannot wrap when the byte count multiplies it by four either.
   [set] and [blend] trust the product this bounds without checking it. *)
let fits = Extent.fits ~limit:Sys.max_floatarray_length

(* Shared by the two ways in, so that each refusal names the function the
   caller wrote rather than the private allocator underneath. The positivity
   check comes first because [fits] only holds for positive extents: a [width]
   of zero divides by zero inside it, and a negative [height] passes it. A pair
   of negatives is the quiet failure: their product is positive, so both
   buffers allocate, and the result reports a size in pixels it has four bytes
   of. *)
let extents who ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg (who ^ ": a buffer must have positive extents");
  if not (fits ~width ~height) then
    invalid_arg (who ^ ": a buffer that size does not fit in an array")

(* The buffer itself, zeroed. Rendering a frame covers every pixel before
    anything reads one, so the clearing is not for the renderer's benefit. It
    makes a buffer nothing has drawn into yet black rather than whatever the
    allocator had lying there, which lets a test state what it expected. *)
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

(* Extents are refused before the texture is created: a raise inside the
   [let+] below would leak a texture that nothing holds and nothing will ever
   [destroy]. *)
let make sdl ~width ~height =
  extents "Framebuffer.make" ~width ~height;
  let+ texture =
    Sdl.create_texture sdl pixel_format Sdl.Texture.access_streaming ~w:width
      ~h:height
  in
  { (buffer ~width ~height) with texture = Some texture }

let offscreen ~width ~height =
  extents "Framebuffer.offscreen" ~width ~height;
  buffer ~width ~height

let destroy t = Option.iter Sdl.destroy_texture t.texture
let clear_depth t = Array.fill t.depth 0 (Array.length t.depth) infinity

let set t ~x ~y ~r ~g ~b =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  Bigarray.Array1.unsafe_set p base b;
  Bigarray.Array1.unsafe_set p (base + 1) g;
  Bigarray.Array1.unsafe_set p (base + 2) r;
  Bigarray.Array1.unsafe_set p (base + 3) 255

let blend t ~x ~y ~r ~g ~b ~alpha =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  let inv = 255 - alpha in
  let mix dst src = ((src * alpha) + (dst * inv)) / 255 in
  Bigarray.Array1.unsafe_set p base (mix (Bigarray.Array1.unsafe_get p base) b);
  Bigarray.Array1.unsafe_set p (base + 1)
    (mix (Bigarray.Array1.unsafe_get p (base + 1)) g);
  Bigarray.Array1.unsafe_set p (base + 2)
    (mix (Bigarray.Array1.unsafe_get p (base + 2)) r)

let pixel t ~x ~y =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  Color.rgb
    (Bigarray.Array1.get p (base + 2))
    (Bigarray.Array1.get p (base + 1))
    (Bigarray.Array1.get p base)

let present sdl t ~dst =
  match t.texture with
  | None ->
      Error (`Msg "Framebuffer.present: this buffer has no window behind it")
  | Some texture ->
      let* () = Sdl.update_texture texture None t.pixels (t.width * 4) in
      Sdl.render_copy sdl texture ~dst
