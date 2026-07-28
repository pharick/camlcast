open Camlcast
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

(* [level] is what every surface pattern is written in terms of, so the two ends
   of its range are the two ends of a pattern's range: a texel at 255 is the
   colour it was given, and one at 0 is black whatever colour that was. *)
let levelling () =
  Alcotest.check color "255 is the colour itself" base (Color.level base 255);
  Alcotest.check color "0 is black" (Color.rgb 0 0 0) (Color.level base 0);
  Alcotest.check color "and half is half of every channel" (Color.rgb 100 50 25)
    (Color.level base 128);
  (* Levelling scales all three channels together, so it cannot turn one colour
     into another — only into a darker version of itself. A pattern that wants a
     second colour has to say so, and this is why. *)
  let dim = Color.level base 90 in
  Alcotest.(check bool)
    "the channels keep their order" true
    (dim.Color.r > dim.Color.g && dim.Color.g > dim.Color.b);
  Alcotest.check color "and a level out of range clamps rather than wrapping"
    base (Color.level base 400)

let clamping_a_colour () =
  Alcotest.check color "channels below zero come back to black"
    (Color.rgb 0 0 12)
    (Color.clamp (Color.rgb (-5) (-200) 12));
  Alcotest.check color "and above 255 back to white" (Color.rgb 255 255 30)
    (Color.clamp (Color.rgb 256 4000 30));
  Alcotest.check color "one already in range is untouched" base
    (Color.clamp base)

let () =
  Alcotest.run "Color"
    [
      ("rgb", [ case "construction" construction ]);
      ("shade", [ case "scales channels" shading; case "clamps" clamping ]);
      ( "level",
        [
          case "scales a colour by a byte" levelling;
          case "clamps a colour" clamping_a_colour;
        ] );
      ("lerp", [ case "blends between colours" blending ]);
    ]
