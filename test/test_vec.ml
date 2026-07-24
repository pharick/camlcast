open Raycaster
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
