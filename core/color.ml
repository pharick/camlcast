type t = { r : int; g : int; b : int }

let rgb r g b = { r; g; b }

(* [@inline always] because the renderer's per-pixel writes go through this — a
   handful of times per pixel of a wall — and this compiler is not flambda, so
   a cross-module call would box the int and cost more than the two comparisons
   do. Same reason as {!Plane.cast}. *)
let[@inline always] clamp_channel v = Int.max 0 (Int.min 255 v)

let shade c factor =
  let apply v = clamp_channel (int_of_float (float_of_int v *. factor)) in
  { r = apply c.r; g = apply c.g; b = apply c.b }

let clamp c =
  { r = clamp_channel c.r; g = clamp_channel c.g; b = clamp_channel c.b }

let level c v =
  let v = clamp_channel v in
  { r = c.r * v / 255; g = c.g * v / 255; b = c.b * v / 255 }

let lerp a b t =
  let mix x y =
    clamp_channel
      (int_of_float ((float_of_int x *. (1. -. t)) +. (float_of_int y *. t)))
  in
  { r = mix a.r b.r; g = mix a.g b.g; b = mix a.b b.b }
