open Raycaster
open Support

let base = Color.rgb 200 100 50

let construction () =
  Alcotest.check color "channels are kept in order" base
    { Color.r = 200; g = 100; b = 50 }

let shading () =
  Alcotest.check color "factor 1 is the identity" base (Color.shade base 1.);
  Alcotest.check color "factor 0 is black" (Color.rgb 0 0 0)
    (Color.shade base 0.);
  Alcotest.check color "factor 0.5 halves every channel" (Color.rgb 100 50 25)
    (Color.shade base 0.5)

(* Brightness is a product of fog and face shading, and rounding can push a
   channel past the byte range; SDL would reject that. *)
let clamping () =
  Alcotest.check color "over-bright channels clamp at 255"
    (Color.rgb 255 200 100) (Color.shade base 2.);
  Alcotest.check color "negative factors clamp at 0" (Color.rgb 0 0 0)
    (Color.shade base (-1.))

let blending () =
  let other = Color.rgb 0 200 100 in
  Alcotest.check color "t = 0 is the first colour" base
    (Color.lerp base other 0.);
  Alcotest.check color "t = 1 is the second" other (Color.lerp base other 1.);
  Alcotest.check color "t = 0.5 is the midpoint" (Color.rgb 100 150 75)
    (Color.lerp base other 0.5)

let () =
  Alcotest.run "Color"
    [
      ("rgb", [ case "construction" construction ]);
      ("shade", [ case "scales channels" shading; case "clamps" clamping ]);
      ("lerp", [ case "blends between colours" blending ]);
    ]
