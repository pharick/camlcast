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

(* The buffer itself, zeroed. Rendering a frame covers every pixel before
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

let offscreen ~width ~height = buffer ~width ~height
let destroy t = Option.iter Sdl.destroy_texture t.texture
let clear_depth t = Array.fill t.depth 0 (Array.length t.depth) infinity

let set t ~x ~y ~r ~g ~b =
  let base = ((y * t.width) + x) * 4 in
  let p = t.pixels in
  Bigarray.Array1.unsafe_set p base b;
  Bigarray.Array1.unsafe_set p (base + 1) g;
  Bigarray.Array1.unsafe_set p (base + 2) r;
  Bigarray.Array1.unsafe_set p (base + 3) 255

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
