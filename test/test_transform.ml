open Raycaster
open Support

(* A transform with both a rotation and an offset, so [point] and [direction]
   cannot be mistaken for each other: under the identity they agree, which is
   exactly the case that proves nothing. *)
let turn_and_shift =
  Transform.between ~a1:(Vec.make 0. 0.) ~a2:(Vec.make 0. 2.)
    ~b1:(Vec.make 5. 1.) ~b2:(Vec.make 3. 1.)

let identity_moves_nothing () =
  let p = Vec.make 2. (-3.) in
  Alcotest.check vec "point" p (Transform.point Transform.identity p);
  Alcotest.check vec "direction" p (Transform.direction Transform.identity p)

(* A position is rotated and then translated; a direction carries no position,
   so it is only rotated. *)
let a_direction_ignores_the_offset () =
  let t = turn_and_shift in
  let p = Vec.make 1. 0. in
  Alcotest.check vec "a point picks the offset up"
    (Vec.add (Transform.direction t p) t.Transform.offset)
    (Transform.point t p);
  Alcotest.(check bool)
    "so the two do not agree" true
    (Vec.length (Vec.sub (Transform.point t p) (Transform.direction t p)) > 1e-6);
  (* Two positions differ by the same vector before and after — the offset
     cancels — which is what makes it right to rotate directions alone. *)
  let q = Vec.make 4. 2. in
  Alcotest.check vec "differences only rotate"
    (Transform.direction t (Vec.sub q p))
    (Vec.sub (Transform.point t q) (Transform.point t p))

let inverse_and_compose_round_trip () =
  let t = turn_and_shift in
  let p = Vec.make (-0.2) 0.7 in
  Alcotest.check vec "inverse undoes point" p
    (Transform.point (Transform.inverse t) (Transform.point t p));
  Alcotest.check vec "composing with the inverse moves nothing" p
    (Transform.point (Transform.compose (Transform.inverse t) t) p);
  (* [compose outer inner] applies the inner motion first. *)
  let u =
    Transform.between ~a1:(Vec.make 1. 1.) ~a2:(Vec.make 1. 3.)
      ~b1:(Vec.make (-2.) 0.) ~b2:(Vec.make (-2.) 2.)
  in
  Alcotest.check vec "inner then outer"
    (Transform.point u (Transform.point t p))
    (Transform.point (Transform.compose u t) p)

let between_reverses_endpoints () =
  let a1 = Vec.make 0. 0. and a2 = Vec.make 0. 1. in
  let b1 = Vec.make 0. 0. and b2 = Vec.make 0. 1. in
  let t = Transform.between ~a1 ~a2 ~b1 ~b2 in
  Alcotest.check vec "a1 goes to b2" b2 (Transform.point t a1);
  Alcotest.check vec "a2 goes to b1" b1 (Transform.point t a2);
  (* The whole point of the reversal: a point just inside one room comes out
     just inside its neighbour, not back outside it. *)
  Alcotest.check vec "and the near side crosses over" (Vec.make 0.1 0.5)
    (Transform.point t (Vec.make (-0.1) 0.5))

(* The motion is rigid, so it may turn and shift a shape but never stretch one.
   That is what lets a distance measured three rooms deep be compared directly
   with the depth buffer of the room it was seen from, and what keeps the camera
   basis exact however many doorways have been walked through. *)
let between_preserves_lengths () =
  let t = turn_and_shift in
  let p = Vec.make 1.5 (-2.5) and q = Vec.make (-3.) 4. in
  Alcotest.check close "the distance between two points is unchanged"
    (Vec.length (Vec.sub q p))
    (Vec.length (Vec.sub (Transform.point t q) (Transform.point t p)));
  Alcotest.check close "a unit direction stays unit" 1.
    (Vec.length (Transform.direction t (Vec.make 0.6 0.8)));
  Alcotest.check close "a perpendicular pair stays perpendicular" 0.
    (dot
       (Transform.direction t (Vec.make 1. 0.))
       (Transform.direction t (Vec.make 0. 1.)))

let () =
  Alcotest.run "Transform"
    [
      ( "rigid motions",
        [
          case "identity moves nothing" identity_moves_nothing;
          case "a direction ignores the offset" a_direction_ignores_the_offset;
          case "inverse and compose round trip" inverse_and_compose_round_trip;
          case "between reverses endpoints" between_reverses_endpoints;
          case "between preserves lengths" between_preserves_lengths;
        ] );
    ]
