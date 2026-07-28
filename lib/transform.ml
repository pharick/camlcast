type t = { cos : float; sin : float; offset : Vec.t }

let identity = { cos = 1.; sin = 0.; offset = Vec.make 0. 0. }

let direction t (v : Vec.t) =
  Vec.make ((t.cos *. v.x) -. (t.sin *. v.y)) ((t.sin *. v.x) +. (t.cos *. v.y))

let point t p = Vec.add (direction t p) t.offset

let inverse t =
  let rotation = { cos = t.cos; sin = -.t.sin; offset = Vec.make 0. 0. } in
  { rotation with offset = direction rotation (Vec.scale t.offset (-1.)) }

let compose outer inner =
  {
    cos = (outer.cos *. inner.cos) -. (outer.sin *. inner.sin);
    sin = (outer.sin *. inner.cos) +. (outer.cos *. inner.sin);
    offset = point outer inner.offset;
  }

(* Measured before either is normalised, because normalising is what loses the
   evidence: {!Vec.normalize} hands a zero vector back unchanged, and the
   [cos = 0., sin = 0.] that follows is the one way past this type's invariant.
   Refused by negating what would pass, so that a [nan] coordinate is refused
   too rather than answering false to every comparison and slipping through. *)
let between ~a1 ~a2 ~b1 ~b2 =
  let u = Vec.sub a2 a1 and w = Vec.sub b1 b2 in
  if not (Vec.length u > 0.) then
    invalid_arg "Transform.between: a1 and a2 are the same point";
  if not (Vec.length w > 0.) then
    invalid_arg "Transform.between: b1 and b2 are the same point";
  let u = Vec.normalize u and w = Vec.normalize w in
  let rotation =
    { cos = Vec.dot u w; sin = Vec.cross u w; offset = Vec.make 0. 0. }
  in
  { rotation with offset = Vec.sub b2 (direction rotation a1) }
