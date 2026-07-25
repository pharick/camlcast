(** The showcase level's pictures: two decals to hang on walls and two sprites
    to stand in the world.

    Unlike a {!Raycaster.Texture}, which is greyscale and takes its colour from
    the {!Raycaster.Material} it belongs to, an {!Raycaster.Image} carries its
    own colours and its own alpha. The decals are opaque within their frame; the
    sprites are cut out against {!Raycaster.Image.clear} so only the object
    itself is drawn. *)

open Raycaster

(** {1 Decals} *)

(** A small framed landscape: a wooden border around a sky, a sun and grass. *)
let painting =
  Image.make 32 (fun ~u ~v ->
      if u < 2 || u > 29 || v < 2 || v > 29 then (Color.rgb 38 26 14, 255)
      else if u < 4 || u > 27 || v < 4 || v > 27 then (Color.rgb 150 105 55, 255)
      else if Image.disc ~cx:22. ~cy:11. ~r:3.5 u v then
        (Color.rgb 245 225 120, 255)
      else if v < 18 then (Color.rgb 120 165 210, 255)
      else (Color.rgb 85 140 70, 255))

(** A bold poster: a yellow ring on a red field, in a dark border. *)
let poster =
  Image.make 32 (fun ~u ~v ->
      if u < 2 || u > 29 || v < 2 || v > 29 then (Color.rgb 22 22 26, 255)
      else
        let du = float_of_int u -. 16. and dv = float_of_int v -. 16. in
        let d = Float.hypot du dv in
        if d > 8. && d < 12. then (Color.rgb 232 200 60, 255)
        else (Color.rgb 155 42 42, 255))

(** {1 Sprites} *)

(** A wooden barrel: a shaded cylinder with darker hoops, clear to either side.
*)
let barrel =
  Image.make 32 (fun ~u ~v ->
      if u < 7 || u > 24 || v < 3 || v > 30 then Image.clear
      else
        let edge = (float_of_int u -. 16.) /. 9. in
        let round = 1. -. (0.55 *. edge *. edge) in
        let hoop = v mod 9 < 2 || v < 5 || v > 28 in
        let base = if hoop then 70. else 135. in
        let r = int_of_float (base *. round) in
        (Color.rgb r (r * 3 / 5) (r / 3), 255))

(** A standing figure: a head, a shirted torso with arms, and legs. *)
let figure =
  Image.make 32 (fun ~u ~v ->
      if Image.disc ~cx:16. ~cy:7. ~r:4.5 u v then (Color.rgb 226 182 142, 255)
      else if u >= 7 && u <= 25 && v >= 12 && v <= 15 then
        (Color.rgb 58 88 158, 255)
      else if u >= 10 && u <= 22 && v >= 11 && v <= 21 then
        (Color.rgb 72 104 182, 255)
      else if u >= 12 && u <= 20 && v >= 21 && v <= 31 then
        (Color.rgb 44 46 62, 255)
      else Image.clear)
