(** Small colour images with a per-pixel alpha channel: the pictures that are
    {e not} part of a wall's own surface — the decals hung on walls and the
    sprites that stand in the world.

    A {!Texture} is greyscale and tinted at draw time, which suits a wall that
    should take a palette colour. A picture instead wants its own colours and a
    transparent background — a painting framed against the wall, a character cut
    out from the empty space around it — so an image carries full RGB and an
    alpha (0 clear, 255 solid) for every pixel. Like the textures they are
    generated in code, so the project keeps no binary assets. *)

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

(* A couple of geometry helpers the generators share. *)
let disc ~cx ~cy ~r u v =
  let du = float_of_int u -. cx and dv = float_of_int v -. cy in
  (du *. du) +. (dv *. dv) < r *. r

let clear = (Color.rgb 0 0 0, 0)

(** {1 Decals}

    Pictures hung flat on a wall, fully opaque within their frame. *)

(** A small framed landscape: a wooden border around a sky, a sun and grass. *)
let painting =
  make 32 (fun ~u ~v ->
      if u < 2 || u > 29 || v < 2 || v > 29 then (Color.rgb 38 26 14, 255)
      else if u < 4 || u > 27 || v < 4 || v > 27 then (Color.rgb 150 105 55, 255)
      else if disc ~cx:22. ~cy:11. ~r:3.5 u v then (Color.rgb 245 225 120, 255)
      else if v < 18 then (Color.rgb 120 165 210, 255)
      else (Color.rgb 85 140 70, 255))

(** A bold poster: a yellow ring on a red field, in a dark border. *)
let poster =
  make 32 (fun ~u ~v ->
      if u < 2 || u > 29 || v < 2 || v > 29 then (Color.rgb 22 22 26, 255)
      else
        let du = float_of_int u -. 16. and dv = float_of_int v -. 16. in
        let d = Float.hypot du dv in
        if d > 8. && d < 12. then (Color.rgb 232 200 60, 255)
        else (Color.rgb 155 42 42, 255))

(** {1 Sprites}

    Free-standing objects, cut out against a transparent background so only the
    object itself is drawn. *)

(** A wooden barrel: a shaded cylinder with darker hoops, clear to either side.
*)
let barrel =
  make 32 (fun ~u ~v ->
      if u < 7 || u > 24 || v < 3 || v > 30 then clear
      else
        let edge = (float_of_int u -. 16.) /. 9. in
        let round = 1. -. (0.55 *. edge *. edge) in
        let hoop = v mod 9 < 2 || v < 5 || v > 28 in
        let base = if hoop then 70. else 135. in
        let r = int_of_float (base *. round) in
        (Color.rgb r (r * 3 / 5) (r / 3), 255))

(** A standing figure: a head, a shirted torso with arms, and legs. *)
let figure =
  make 32 (fun ~u ~v ->
      if disc ~cx:16. ~cy:7. ~r:4.5 u v then (Color.rgb 226 182 142, 255)
      else if u >= 7 && u <= 25 && v >= 12 && v <= 15 then
        (Color.rgb 58 88 158, 255)
      else if u >= 10 && u <= 22 && v >= 11 && v <= 21 then
        (Color.rgb 72 104 182, 255)
      else if u >= 12 && u <= 20 && v >= 21 && v <= 31 then
        (Color.rgb 44 46 62, 255)
      else clear)
