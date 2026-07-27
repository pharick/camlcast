(** Reading a picture file into plain bytes: the one place in the engine that
    knows what a PNG is.

    {!Texture} and {!Image} both want the same thing from a file — a rectangle
    of red, green, blue and alpha — and differ only in what they make of it
    afterwards, so the awkward half is done once here and each of them is a
    short loop over the result. The awkward half is real: SDL hands back a
    surface in whatever format the file happened to be in, which may be
    palettized, may be three bytes per pixel, may have its rows padded, and may
    have no alpha at all. Converting to one known format first turns all of that
    into a single byte order.

    Nothing here is content, and nothing here decides anything: it is a decoder.
    What the picture means — a wall's surface, a poster's paint — belongs to the
    module that asked for it. *)

open Tsdl
open Result_ext

type t = { width : int; height : int; rgba : Bytes.t }
(** A decoded picture: [width * height] pixels, row major, four bytes each in
    the order red, green, blue, alpha. Bytes rather than an [int array] because
    a caller reads every channel exactly once and then throws this away — the
    long-lived arrays are {!Image}'s and {!Texture}'s. *)

(** The format the file is converted to before its pixels are read, chosen so
    the bytes land in the order [rgba] wants them on either kind of machine.

    This is {!Framebuffer.pixel_format} read the other way round, and for the
    same reason: SDL names a packed format by its channels from the most
    significant byte down, so [ABGR8888] puts red in the lowest bits — and so
    first in memory — on a little-endian machine, and [RGBA8888] does the same
    on a big-endian one. Naming the format the byte order asks for saves the
    loops below from caring which one they are on. *)
let load_format =
  if Sys.big_endian then Sdl.Pixel.format_rgba8888
  else Sdl.Pixel.format_abgr8888

(** Whether SDL_image's codecs have been started, since starting them twice is
    wasteful and starting them lazily keeps a program that loads nothing from
    paying for a decoder it never uses. *)
let started = ref false

let ready () =
  if !started then Ok ()
  else
    let wanted = Tsdl_image.Image.Init.(png + jpg) in
    let got = Tsdl_image.Image.init wanted in
    (* [test] is true when {e any} of the mask is set, which is the check worth
       making: a build missing one codec should still load the other, and a file
       in the missing one fails later with its own name attached. Nothing at all
       means SDL_image is not working, and that is worth saying now. *)
    if Tsdl_image.Image.Init.test got wanted then begin
      started := true;
      Ok ()
    end
    else Error (`Msg "SDL_image: neither PNG nor JPEG support is available")

(** Copy a converted surface's pixels out into a fresh [Bytes].

    The copy is not avoidable and not a waste: [Sdl.get_surface_pixels] hands
    back a view into the surface's own memory, which is freed as soon as we are
    done with it. The row-by-row loop is because [pitch] — the distance between
    the starts of two rows — may be larger than the row itself, so a surface's
    rows are not necessarily one contiguous block. *)
let pixels surface =
  let width, height = Sdl.get_surface_size surface in
  let pitch = Sdl.get_surface_pitch surface in
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

(** [read path] decodes the picture at [path]. Both surfaces are freed however
    this returns, including if a caller's loop over the result raises. *)
let read path =
  let* () = ready () in
  with_resource
    (fun () -> Tsdl_image.Image.load path)
    Sdl.free_surface
    (fun raw ->
      with_resource
        (fun () -> Sdl.convert_surface_format raw load_format)
        Sdl.free_surface pixels)

(** The byte offset of pixel [(x, y)]; its red channel, with green, blue and
    alpha in the three bytes after it. *)
let offset t ~x ~y = ((y * t.width) + x) * 4

let channel t i = Char.code (Bytes.unsafe_get t.rgba i)

(** How solid pixel [(x, y)] is. A file with no alpha channel of its own has
    been converted to one that has, and arrives fully solid throughout. *)
let alpha t ~x ~y = channel t (offset t ~x ~y + 3)

(** The colour and alpha of pixel [(x, y)]. *)
let sample t ~x ~y =
  let i = offset t ~x ~y in
  ( Color.rgb (channel t i) (channel t (i + 1)) (channel t (i + 2)),
    channel t (i + 3) )
