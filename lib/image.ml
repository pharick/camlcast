(** Small colour images with a per-pixel alpha channel: the pictures that are
    {e not} part of a surface's own material — the decals hung on walls and the
    sprites that stand in the world.

    A {!Texture} is greyscale and tinted at draw time, which suits a wall that
    should take its colour from its {!Material}. A picture instead wants its own
    colours and a transparent background — a painting framed against the wall, a
    character cut out from the empty space around it — so an image carries full
    RGB and an alpha (0 clear, 255 solid) for every pixel.

    Like the textures, this module is the machinery only: the type, the
    generator and the samplers. The pictures themselves are content and live
    with whatever draws them. *)

type t = { size : int; pixels : Color.t array; alpha : int array }

(** Build a [size] x [size] image from a function of the pixel coordinates,
    returning the colour and alpha at each. *)
let make size f =
  let n = size * size in
  let pixels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
  for v = 0 to size - 1 do
    for u = 0 to size - 1 do
      let color, a = f ~u ~v in
      let i = (v * size) + u in
      pixels.(i) <- color;
      alpha.(i) <- a
    done
  done;
  { size; pixels; alpha }

(** The flat array index of pixel [(u, v)]; the caller has already clamped them
    into range. Kept separate so the hot drawing loop can read [pixels] and
    [alpha] directly without allocating. *)
let index t ~u ~v = (v * t.size) + u

(** The colour and alpha of pixel [(u, v)]. *)
let sample t ~u ~v =
  let i = index t ~u ~v in
  (t.pixels.(i), t.alpha.(i))

(** Is [(u, v)] inside the circle of radius [r] about [(cx, cy)]? A generator
    helper, here rather than repeated in every module that draws a round thing.
*)
let disc ~cx ~cy ~r u v =
  let du = float_of_int u -. cx and dv = float_of_int v -. cy in
  (du *. du) +. (dv *. dv) < r *. r

(** Nothing at all: the value a generator returns for a pixel outside the shape
    it is cutting out. *)
let clear = (Color.rgb 0 0 0, 0)
