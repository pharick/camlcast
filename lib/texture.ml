(** The machinery for procedurally generated surface patterns.

    The patterns are {e greyscale}: a texel says how bright that point of the
    surface is, not what colour it is. The colour arrives later, from the
    {!Material} the pattern is half of. Splitting it that way means one pattern
    can dress surfaces of any colour, and the distance fog and face shading in
    {!Atmosphere} keep working untouched.

    This module holds no patterns of its own. It is the type, the samplers and
    the two generators; the patterns themselves are content and belong to
    whatever is being drawn. Generating them in code rather than loading images
    keeps the project free of binary assets, and keeps them testable: a pattern
    is a pure function of [u] and [v]. *)

(** Texels per side. A power of two, and small enough that the whole set stays
    comfortably in cache. *)
let size = 64

type t = {
  texels : int array;  (** row major, brightness 0 .. 255 *)
  alpha : int array;  (** row major, opacity 0 (clear) .. 255 (solid) *)
  opaque : bool;  (** whether every texel is fully solid *)
}

let sample t ~u ~v = t.texels.((v * size) + u)
let alpha t ~u ~v = t.alpha.((v * size) + u)

(** The texel column a hit at [offset] across the wall face falls in. [offset]
    reaches 1.0 exactly when a ray strikes a corner, which would index one past
    the end, so the result is clamped. *)
let column_of_offset offset =
  Int.min (size - 1) (Int.max 0 (int_of_float (offset *. float_of_int size)))

let clamp v = Int.min 255 (Int.max 0 v)

(** A solid (fully opaque) pattern from a brightness function. *)
let generate f =
  {
    texels =
      Array.init (size * size) (fun i ->
          clamp (f ~u:(i mod size) ~v:(i / size)));
    alpha = Array.make (size * size) 255;
    opaque = true;
  }

(** A pattern that can see through itself: [f] returns a brightness {e and} an
    alpha for each texel, so a wall wearing it unveils whatever is behind. *)
let generate_masked f =
  let n = size * size in
  let texels = Array.make n 0 and alpha = Array.make n 0 in
  let opaque = ref true in
  for i = 0 to n - 1 do
    let brightness, a = f ~u:(i mod size) ~v:(i / size) in
    texels.(i) <- clamp brightness;
    alpha.(i) <- clamp a;
    if a < 255 then opaque := false
  done;
  { texels; alpha; opaque = !opaque }

(** A cheap deterministic hash. Not good randomness by any standard, but enough
    to stop a surface looking machine-made, and reproducible so the tests can
    pin what it produces. *)
let hash a b =
  let h = (a * 73856093) lxor (b * 19349663) in
  (h lxor (h lsr 13)) land max_int

(** Smooth value noise, 0 .. 255: a hashed value at each corner of a lattice of
    [cell]-texel squares, interpolated between them with a smoothstep so the
    result has no lattice edges in it. [seed] picks an independent field, so
    several octaves can be summed without their features lining up.

    The lattice {b wraps} at {!size}, which is the whole reason this is here
    rather than in a caller. A wall's pattern repeats once per world unit, so a
    field that did not wrap would put a hard seam down every wall in the game,
    one per unit — the very thing noise is being used to avoid.

    [cell] must divide {!size}, or the lattice would not close on itself. *)
let noise ~seed ~cell ~u ~v =
  if cell <= 0 || size mod cell <> 0 then
    invalid_arg "Texture.noise: cell must divide Texture.size";
  let cells = size / cell in
  let corner x y =
    float_of_int (hash ((x mod cells) + (seed * 7919)) ((y mod cells) + seed) land 255)
  in
  (* Smoothstep, so the interpolation arrives at each corner with zero slope and
     the eye cannot pick out the lattice the values hang on. *)
  let smooth t = t *. t *. (3. -. (2. *. t)) in
  let mix a b t = a +. ((b -. a) *. t) in
  let x0 = u / cell and y0 = v / cell in
  let fx = smooth (float_of_int (u mod cell) /. float_of_int cell)
  and fy = smooth (float_of_int (v mod cell) /. float_of_int cell) in
  let top = mix (corner x0 y0) (corner (x0 + 1) y0) fx
  and bottom = mix (corner x0 (y0 + 1)) (corner (x0 + 1) (y0 + 1)) fx in
  int_of_float (mix top bottom fy)
