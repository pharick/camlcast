(** 8-bit RGB colours. *)

type t = { r : int; g : int; b : int }

let rgb r g b = { r; g; b }
let clamp_channel v = Int.max 0 (Int.min 255 v)

(** Multiply every channel by [factor] (0 = black, 1 = unchanged). Used for
    distance fog and for shading walls by orientation. *)
let shade c factor =
  let apply v = clamp_channel (int_of_float (float_of_int v *. factor)) in
  { r = apply c.r; g = apply c.g; b = apply c.b }

(** Every channel clamped back into 0 .. 255. A colour arrived at by arithmetic
    can leave the range at either end — {!Texture.generate} clamps what a
    pattern function hands it for exactly that reason. *)
let clamp c =
  { r = clamp_channel c.r; g = clamp_channel c.g; b = clamp_channel c.b }

(** [c] shown at [level] out of 255: the integer counterpart of {!shade}, where
    255 leaves the colour alone and 0 is black.

    This is what a surface pattern is written in terms of. {!Texture.noise} and
    the hashes beside it speak in 0 .. 255, so a pattern is naturally a function
    saying {e how much} of a surface's colour reaches the eye at each texel —
    and this turns that answer back into a colour. Scaling all three channels
    together moves value without touching hue, which is what makes one pattern
    usable at any colour it is handed. *)
let level c v =
  let v = clamp_channel v in
  { r = c.r * v / 255; g = c.g * v / 255; b = c.b * v / 255 }

(** Linear blend between two colours: [t = 0] gives [a], [t = 1] gives [b]. Used
    for the sky gradient and its sun. *)
let lerp a b t =
  let mix x y =
    clamp_channel
      (int_of_float ((float_of_int x *. (1. -. t)) +. (float_of_int y *. t)))
  in
  { r = mix a.r b.r; g = mix a.g b.g; b = mix a.b b.b }
