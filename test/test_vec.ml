open Camlcast_core
open Support

let add () =
  Alcotest.check vec "componentwise sum" (Vec.make 4. (-2.))
    (Vec.add (Vec.make 1. 2.) (Vec.make 3. (-4.)))

let sub () =
  Alcotest.check vec "componentwise difference" (Vec.make (-2.) 6.)
    (Vec.sub (Vec.make 1. 2.) (Vec.make 3. (-4.)))

let scale () =
  Alcotest.check vec "componentwise product" (Vec.make 2.5 5.)
    (Vec.scale (Vec.make 1. 2.) 2.5)

let length () =
  Alcotest.check close "3-4-5 triangle" 5. (Vec.length (Vec.make 3. 4.))

let dot_and_cross () =
  let a = Vec.make 1. 2. and b = Vec.make 3. 4. in
  Alcotest.check close "dot" 11. (Vec.dot a b);
  Alcotest.check close "cross is the signed area" (-2.) (Vec.cross a b);
  Alcotest.check close "cross is zero for parallel vectors" 0.
    (Vec.cross a (Vec.scale a 2.5))

let normalize () =
  Alcotest.check close "a normalised vector is unit length" 1.
    (Vec.length (Vec.normalize (Vec.make 3. 4.)));
  Alcotest.check vec "direction is kept" (Vec.make 0.6 0.8)
    (Vec.normalize (Vec.make 3. 4.));
  Alcotest.check vec "a zero vector is left alone" (Vec.make 0. 0.)
    (Vec.normalize (Vec.make 0. 0.))

(* The gap [is_finite l && l > 0.] leaves, and the reason normalizable exists.

   A length can be finite, and above zero, and still too small to take a
   reciprocal of: below [1. /. Float.max_float] — about [5.6e-309], which is
   subnormal — the reciprocal overflows to infinity and normalize scales by it,
   handing back an infinity on one axis and a nan on the other. That was enough
   to put a nan cos and sin into Transform.t, whose private type exists to say
   it cannot hold one, through Room.across; and a nan normal onto a Room.wall,
   which side_of and the shading read as they stand. *)
let normalizable_covers_the_reciprocal () =
  let yes l =
    Alcotest.(check bool)
      (Printf.sprintf "%g is a length" l)
      true (Vec.normalizable l)
  and no l =
    Alcotest.(check bool)
      (Printf.sprintf "%g is not" l)
      false (Vec.normalizable l)
  in
  List.iter yes [ 1.; 1e-6; 1e-300; 1e-308; Float.min_float ];
  List.iter no
    [ 0.; -1.; Float.nan; Float.infinity; Float.neg_infinity; 1e-320; 1e-310 ];
  (* Where the boundary falls exactly is not worth asserting and not worth
     knowing: [1. /. Float.max_float] is not itself a length this admits,
     because a subnormal has too few bits to invert back and its reciprocal
     rounds past [Float.max_float] again. Which is the argument for testing the
     reciprocal rather than comparing against a constant.

     What is worth asserting is that the predicate and the function agree,
     wherever the boundary is: everything normalizable admits comes back a unit
     vector, and everything it refuses does not. Swept across the region where
     the reciprocal gives out. *)
  List.iter
    (fun l ->
      let n = Vec.normalize (Vec.make l 0.) in
      let is_unit = Float.abs (Vec.length n -. 1.) <= 1e-9 in
      Alcotest.(check bool)
        (Printf.sprintf "%g: normalizable says %b and normalize agrees" l
           (Vec.normalizable l))
        (Vec.normalizable l) is_unit)
    [
      1.;
      1e-6;
      1e-300;
      1e-308;
      Float.min_float;
      1. /. Float.max_float;
      5.6e-309;
      5.5e-309;
      1e-310;
      1e-320;
    ]

let of_angle_axes () =
  Alcotest.check vec "angle 0 points along +x" (Vec.make 1. 0.)
    (Vec.of_angle 0.);
  Alcotest.check vec "a quarter turn points along +y" (Vec.make 0. 1.)
    (Vec.of_angle (Float.pi /. 2.))

(* Ray relies on |dir| = 1 to make its distance fish-eye free, so this has to
   hold for every direction the camera can look at. *)
let of_angle_is_always_a_unit_vector () =
  List.iter
    (fun angle ->
      Alcotest.check close
        (Printf.sprintf "|of_angle %g|" angle)
        1.
        (Vec.length (Vec.of_angle angle)))
    [ 0.; 0.7; 2.; -3.1; 10. ]

let rotate_preserves_length () =
  Alcotest.check close "a rotated 3-4-5 vector is still 5 long" 5.
    (Vec.length (Vec.rotate (Vec.make 3. 4.) 1.234))

let rotate_a_full_turn_is_the_identity () =
  let v = Vec.make 3. 4. in
  Alcotest.check vec "back where we started" v (Vec.rotate v (2. *. Float.pi))

let rotations_compose () =
  let v = Vec.make 3. 4. in
  Alcotest.check vec "two half turns make a whole one" (Vec.rotate v 1.0)
    (Vec.rotate (Vec.rotate v 0.5) 0.5)

let perp_is_orthogonal () =
  let v = Vec.make 3. 4. in
  Alcotest.check close "dot product is zero" 0. (dot v (Vec.perp v));
  Alcotest.check close "and the length is unchanged" (Vec.length v)
    (Vec.length (Vec.perp v))

(* perp is the exact version of a quarter turn, and Player depends on that
   equivalence to build the camera plane. *)
let perp_is_a_quarter_turn () =
  let v = Vec.make 3. 4. in
  Alcotest.check vec "perp = rotate by pi/2"
    (Vec.rotate v (Float.pi /. 2.))
    (Vec.perp v)

(* The figure itself, and not only the way callers scale it. Every test that
   touches Vec.parallel today — Ray's wall far shorter than a pixel, Room's step
   far shorter than a pixel — is about the scaling: that a short pair at a real
   angle is not mistaken for a parallel one. All of them go on passing with this
   number moved ten orders of magnitude in either direction, because a real
   angle stays a real angle. So the value is pinned here, from both sides.

   Below: the smallest angle the engine has to treat as a genuine crossing. A
   tenth of a degree is far finer than anything a level is authored at and far
   coarser than anything float arithmetic gets wrong, and a tolerance above its
   sine would call a real grazing hit parallel — a wall a ray slides past
   instead of meeting, or a doorway a step is not reported as crossing.

   Above: the rounding it exists to swallow. Two unit vectors that are parallel
   in exact arithmetic have a computed cross product of a few times the epsilon
   of one, so a tolerance at or below that catches nothing and the parallel
   branch becomes unreachable. *)
let parallel_is_between_a_grazing_angle_and_rounding () =
  let grazing = sin (0.1 *. Float.pi /. 180.) in
  Alcotest.(check bool)
    (Printf.sprintf "below the sine of a tenth of a degree (%g)" grazing)
    true (Vec.parallel < grazing);
  (* Measured rather than asserted: the worst computed cross product over a
     sweep of exactly-parallel unit pairs is what the figure has to clear. *)
  let worst = ref 0. in
  for i = 0 to 720 do
    let a = Vec.of_angle (float_of_int i *. Float.pi /. 360.) in
    let b = Vec.scale a 3.7 in
    let residue = Float.abs (Vec.cross a b) /. (Vec.length a *. Vec.length b) in
    if residue > !worst then worst := residue
  done;
  Alcotest.(check bool)
    (Printf.sprintf "and above what a parallel pair actually computes to (%g)"
       !worst)
    true (Vec.parallel > !worst)

let () =
  Alcotest.run "Vec"
    [
      ( "arithmetic",
        [
          case "add" add;
          case "sub" sub;
          case "scale" scale;
          case "length" length;
          case "dot and cross" dot_and_cross;
          case "normalize" normalize;
          case "normalizable covers the reciprocal"
            normalizable_covers_the_reciprocal;
          case "parallel is between a grazing angle and rounding"
            parallel_is_between_a_grazing_angle_and_rounding;
        ] );
      ( "of_angle",
        [
          case "points at the axes" of_angle_axes;
          case "is always a unit vector" of_angle_is_always_a_unit_vector;
        ] );
      ( "rotate",
        [
          case "preserves length" rotate_preserves_length;
          case "a full turn is the identity" rotate_a_full_turn_is_the_identity;
          case "rotations compose" rotations_compose;
        ] );
      ( "perp",
        [
          case "is orthogonal" perp_is_orthogonal;
          case "is a quarter turn" perp_is_a_quarter_turn;
        ] );
    ]
