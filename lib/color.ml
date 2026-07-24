(** 8-bit RGB colours. *)

type t = { r : int; g : int; b : int }

let rgb r g b = { r; g; b }
let clamp_channel v = Int.max 0 (Int.min 255 v)

(** Multiply every channel by [factor] (0 = black, 1 = unchanged). Used for
    distance fog and for shading walls by orientation. *)
let shade c factor =
  let apply v = clamp_channel (int_of_float (float_of_int v *. factor)) in
  { r = apply c.r; g = apply c.g; b = apply c.b }

(** Linear blend between two colours: [t = 0] gives [a], [t = 1] gives [b]. Used
    for the sky gradient and its sun. *)
let lerp a b t =
  let mix x y =
    clamp_channel
      (int_of_float ((float_of_int x *. (1. -. t)) +. (float_of_int y *. t)))
  in
  { r = mix a.r b.r; g = mix a.g b.g; b = mix a.b b.b }
