(** Small colour images with a per-pixel alpha channel: the pictures that are
    {e not} part of a surface's own material — the decals hung on walls and the
    sprites that stand in the world.

    Both this and a {!Texture} are colour with an alpha, and the difference is
    what they are for rather than what they hold. A texture is {e part of a
    surface}: square, and tiling once per world cell, so its size is a density
    and its shape is not its own. A picture stands on its own — a painting framed
    against the wall, a character cut out from the empty space around it — so it
    keeps whatever shape it was drawn at and is drawn once rather than repeated.

    An image is a rectangle and not a square, because the things it is used for
    are: a poster is wider than it is tall, a standing figure is taller than it
    is wide, and a file on disk is whatever shape it was drawn at. Whoever
    samples one has to keep [width] and [height] apart — {!Room.decal_column}
    against the first, {!Room.decal_row} against the second — since getting them
    the wrong way round is a mirror image and not an error.

    Like the textures, this module is the machinery only: the type, the
    generator, the loader and the samplers. The pictures themselves are content
    and live with whatever draws them. *)

type t = {
  width : int;
  height : int;
  pixels : Color.t array;  (** row major *)
  alpha : int array;  (** row major, 0 (clear) .. 255 (solid) *)
}

(** Build a [width] x [height] image from a function of the pixel coordinates,
    returning the colour and alpha at each. [height] defaults to [width], since
    a generated picture is usually square and saying so twice reads worse than
    not saying it. *)
let make ?height width f =
  let height = Option.value height ~default:width in
  let n = width * height in
  let pixels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
  for v = 0 to height - 1 do
    for u = 0 to width - 1 do
      let color, a = f ~u ~v in
      let i = (v * width) + u in
      pixels.(i) <- color;
      alpha.(i) <- a
    done
  done;
  { width; height; pixels; alpha }

(** The flat array index of pixel [(u, v)]; the caller has already clamped them
    into range. Kept separate so the hot drawing loop can read [pixels] and
    [alpha] directly without allocating. *)
let index t ~u ~v = (v * t.width) + u

(** The colour and alpha of pixel [(u, v)]. *)
let sample t ~u ~v =
  let i = index t ~u ~v in
  (t.pixels.(i), t.alpha.(i))

(** Read a picture from a PNG or JPEG file, colour and alpha both. A file with
    no alpha channel of its own arrives fully solid, which is what a photograph
    or a JPEG means by having none.

    Generating a picture in code is still the other way in and still what every
    demo does. This is for the ones that were drawn rather than derived. *)
let load path =
  Result.map
    (fun (s : Surface.t) ->
      make ~height:s.Surface.height s.Surface.width (fun ~u ~v ->
          Surface.sample s ~x:u ~y:v))
    (Surface.read path)

(** Is [(u, v)] inside the circle of radius [r] about [(cx, cy)]? A generator
    helper, here rather than repeated in every module that draws a round thing.
*)
let disc ~cx ~cy ~r u v =
  let du = float_of_int u -. cx and dv = float_of_int v -. cy in
  (du *. du) +. (dv *. dv) < r *. r

(** Nothing at all: the value a generator returns for a pixel outside the shape
    it is cutting out. *)
let clear = (Color.rgb 0 0 0, 0)
