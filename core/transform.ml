type t = { cos : float; sin : float; offset : Vec.t }

let identity = { cos = 1.; sin = 0.; offset = Vec.make 0. 0. }

let direction t (v : Vec.t) =
  Vec.make ((t.cos *. v.x) -. (t.sin *. v.y)) ((t.sin *. v.x) +. (t.cos *. v.y))

let point t p = Vec.add (direction t p) t.offset

let inverse t =
  let rotation = { cos = t.cos; sin = -.t.sin; offset = Vec.make 0. 0. } in
  { rotation with offset = direction rotation (Vec.scale t.offset (-1.)) }

(* The lengths are measured before either vector is normalised, because
   normalising loses the evidence: {!Vec.normalize} hands a zero vector back
   unchanged, and the [cos = 0., sin = 0.] that follows is the one way past
   this type's invariant. The guard negates what would pass, so a [nan]
   coordinate is refused too rather than answering false to every comparison
   and slipping through.

   The length is enough to check on its own, because [Float.hypot] folds every
   bad coordinate into it: a [nan] coordinate gives a [nan] length, and an
   infinite one gives an infinite length whose reciprocal is [0.], so
   {!Vec.normalize} would scale by zero and reach the same broken value from
   the other side.

   The test is {!Vec.normalizable} and not [is_finite && > 0.]. This function
   asked the latter before, and it let the invariant through in the one case
   the reasoning above does not cover: a length can be finite and positive and
   still be too small to take a reciprocal of. Below about [5.6e-309] the
   reciprocal is [infinity], normalising gives [(infinity, nan)], and the [cos]
   and [sin] built from it are both [nan]. That is a value that rotates
   nothing, in the type whose invariant is that it always is a rotation. Two
   segments [1e-320] long did exactly that, and reached here through
   {!Room.across}. *)
let between ~a1 ~a2 ~b1 ~b2 =
  let u = Vec.sub a2 a1 and w = Vec.sub b1 b2 in
  let lu = Vec.length u and lw = Vec.length w in
  if not (Vec.normalizable lu) then
    invalid_arg "Transform.between: a1 and a2 are the same point";
  if not (Vec.normalizable lw) then
    invalid_arg "Transform.between: b1 and b2 are the same point";
  let u = Vec.normalize u and w = Vec.normalize w in
  let rotation =
    { cos = Vec.dot u w; sin = Vec.cross u w; offset = Vec.make 0. 0. }
  in
  { rotation with offset = Vec.sub b2 (direction rotation a1) }
