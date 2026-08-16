(* Implementation of {!Camlcast.Bitmap}; the interface carries the prose. *)

open Tsdl
open Result_ext

type t = { width : int; height : int; rgba : Bytes.t }

(* The format the file is converted to before its pixels are read, chosen so
    the bytes land in the order [rgba] wants them on either kind of machine.

    This is {!Framebuffer.pixel_format} read the other way round, and for the
    same reason: SDL names a packed format by its channels from the most
    significant byte down. [ABGR8888] therefore puts red in the lowest bits,
    and so first in memory, on a little-endian machine; [RGBA8888] does the
    same on a big-endian one. Choosing the format by byte order means the loops
    below do not depend on which kind of machine they run on. *)
let load_format =
  if Sys.big_endian then Sdl.Pixel.format_rgba8888
  else Sdl.Pixel.format_abgr8888

(* Whether SDL_image's codecs have been started. Starting them twice is
    wasteful. Starting them lazily means a program that loads nothing never
    pays for a decoder it does not use. *)
let started = ref false

let ready () =
  if !started then Ok ()
  else
    let wanted = Tsdl_image.Image.Init.(png + jpg) in
    let got = Tsdl_image.Image.init wanted in
    (* [test] is true when {e any} of the mask is set, which is the right
       check: a build missing one codec should still load the other, and a file
       in the missing format fails later with its own name attached. No codecs
       at all means SDL_image is not working, so that is reported now. *)
    if Tsdl_image.Image.Init.test got wanted then begin
      started := true;
      Ok ()
    end
    else Error (`Msg "SDL_image: neither PNG nor JPEG support is available")

(* Copy a converted surface's pixels out into a fresh [Bytes].

    The copy is required: [Sdl.get_surface_pixels] hands back a view into the
    surface's own memory, which is freed as soon as the surface is. The loop
    goes row by row because [pitch], the distance between the starts of two
    rows, may be larger than the row itself. A surface's rows are therefore not
    necessarily one contiguous block. *)
let pixels path surface =
  let width, height = Sdl.get_surface_size surface in
  let pitch = Sdl.get_surface_pitch surface in
  (* This refusal has to live here, before the allocation it is about. [rgba]
     holds four channels per pixel, so its ceiling is a quarter of
     [Sys.max_string_length]. On a 32-bit machine that is one texel {e under}
     what a [Color.t array] holds. That is why {!Image.load}'s and
     {!Texture.load}'s own checks, downstream of this function, could never be
     reached by a decoded file there: [Bytes.create] raised first, out of a
     [result]. A file is a condition and not an authoring mistake, so a picture
     past the ceiling comes back as an [Error] like any other file a loader
     refuses. *)
  if
    width > 0 && height > 0
    && not (Extent.fits ~limit:(Sys.max_string_length / 4) ~width ~height)
  then
    Error
      (`Msg
         (Printf.sprintf
            "%s: a picture of %dx%d holds more bytes than this machine can" path
            width height))
  else
    with_resource
      (fun () -> Sdl.lock_surface surface)
      (fun () -> Sdl.unlock_surface surface)
      (fun () ->
        let raw = Sdl.get_surface_pixels surface Bigarray.int8_unsigned in
        let row = width * 4 in
        let rgba = Bytes.create (row * height) in
        for y = 0 to height - 1 do
          let src = y * pitch and dst = y * row in
          for i = 0 to row - 1 do
            Bytes.unsafe_set rgba (dst + i)
              (Char.unsafe_chr (Bigarray.Array1.unsafe_get raw (src + i)))
          done
        done;
        Ok { width; height; rgba })

let load path =
  let* () = ready () in
  with_resource
    (fun () -> Tsdl_image.Image.load path)
    Sdl.free_surface
    (fun raw ->
      with_resource
        (fun () -> Sdl.convert_surface_format raw load_format)
        Sdl.free_surface (pixels path))

(* The byte offset of pixel [(x, y)]; its red channel, with green, blue and
    alpha in the three bytes after it. *)
let offset t ~x ~y = ((y * t.width) + x) * 4
let channel t i = Char.code (Bytes.unsafe_get t.rgba i)

let sample t ~u ~v =
  let i = offset t ~x:u ~y:v in
  ( Color.rgb (channel t i) (channel t (i + 1)) (channel t (i + 2)),
    channel t (i + 3) )
