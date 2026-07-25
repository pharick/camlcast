(** Procedurally generated wall patterns.

    The patterns are {e greyscale}: a texel says how bright that point of the
    wall is, not what colour it is. The colour arrives later, when the renderer
    modulates the whole slice by {!Palette.shaded_wall}. Splitting it that way
    means one pattern can dress walls of any colour, and the distance fog and
    face shading that were already there keep working untouched.

    Generating the patterns in code rather than loading images keeps the project
    free of binary assets, and keeps them testable: every pattern below is a
    pure function of [u] and [v]. *)

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
  let h = a * 73856093 lxor (b * 19349663) in
  h lxor (h lsr 13) land max_int

(** Running bond masonry: courses 16 texels high, every other one shifted by
    half a brick so the vertical joints never line up between courses. *)
let brick =
  generate (fun ~u ~v ->
      let course = v / 16 in
      let u = (u + if course land 1 = 0 then 0 else 16) mod size in
      let in_mortar = v mod 16 < 2 || u mod 32 < 2 in
      if in_mortar then 130 else 225 + (hash (u / 32) course mod 30))

(** Bevelled panels: a lit edge along the top and left, a shadow along the
    bottom and right. The eye reads the pair as depth. *)
let panel =
  generate (fun ~u ~v ->
      let x = u mod 32 and y = v mod 32 in
      if x < 2 || y < 2 then 255 else if x >= 30 || y >= 30 then 140 else 215)

(** Vertical planks crossed by a sturdy horizontal rail. *)
let door =
  generate (fun ~u ~v ->
      if v >= 28 && v < 36 then 150
      else if u mod 16 < 2 then 115
      else 205 + (hash (u / 16) 19 mod 30))

(** Irregular blocks: each course is shifted by a hashed amount, so unlike
    {!brick} the courses do not repeat in step with each other. *)
let stone =
  generate (fun ~u ~v ->
      let course = v / 16 in
      let u = (u + (hash course 7 mod size)) mod size in
      let in_joint = v mod 16 < 2 || u mod 21 < 2 in
      if in_joint then 120 else 200 + (hash (u / 21) course mod 45))

(** A plain check, for walls that should read as tiled rather than built. *)
let checker =
  generate (fun ~u ~v -> if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

(** No pattern at all, for wall ids that have none of their own. *)
let plain = generate (fun ~u:_ ~v:_ -> 235)

(** A metal grille: a lattice of solid bars with clear holes between them, so
    the room behind shows through the gaps. *)
let bars =
  generate_masked (fun ~u ~v ->
      if u mod 16 < 5 || v mod 16 < 5 then
        (70 + (hash (u / 16) (v / 16) mod 30), 255)
      else (0, 0))

(** A leaded window: solid mullions around and across, translucent panes
    between, so the room behind is dimly visible through the glass. *)
let glass =
  generate_masked (fun ~u ~v ->
      let mullion =
        u < 2
        || u >= size - 2
        || v < 2
        || v >= size - 2
        || abs (u - (size / 2)) < 2
        || abs (v - (size / 2)) < 2
      in
      if mullion then (150, 255) else (235, 80))
