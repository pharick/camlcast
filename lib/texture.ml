(** The machinery for surface patterns, generated in code or read from a file.

    The patterns are {e greyscale}: a texel says how bright that point of the
    surface is, not what colour it is. The colour arrives later, from the
    {!Material} the pattern is half of. Splitting it that way means one pattern
    can dress surfaces of any colour, and the distance fog and face shading in
    {!Atmosphere} keep working untouched.

    A pattern is square, and tiles once per world unit — {!Renderer} and
    {!Material.plane_texel} both index it by the fractional part of a world
    coordinate — so its size is a texel density and not a resolution: that many
    texels across every cell of every wall and floor wearing it. That is why the
    size is a property of the pattern rather than of the module. Sixty-four is
    plenty for a generated pattern, whose detail is invented at whatever scale
    it is asked for; art drawn by hand wants more, because at a cell's distance
    one texel is about nine pixels tall.

    This module holds no patterns of its own. It is the type, the samplers, the
    two generators and the loader; the patterns themselves are content and
    belong to whatever is being drawn. A generated one has the advantage of
    being testable — it is a pure function of [u] and [v] — and a loaded one has
    the advantage of having been drawn. *)

open Result_ext

(** Texels per side of a pattern that does not say otherwise: what the
    generators below make when they are not told, and what every pattern in the
    engine's own demos is. A power of two, and small enough that the whole set
    stays comfortably in cache. *)
let size = 64

type t = {
  size : int;  (** texels per side, and so per world unit of surface *)
  texels : int array;  (** row major, brightness 0 .. 255 *)
  alpha : int array;  (** row major, opacity 0 (clear) .. 255 (solid) *)
  opaque : bool;  (** whether every texel is fully solid *)
}

let sample t ~u ~v = t.texels.((v * t.size) + u)
let alpha t ~u ~v = t.alpha.((v * t.size) + u)

(** The texel column of [t] that a hit at [offset] across one tile falls in.
    [offset] reaches 1.0 exactly when a ray strikes a corner, which would index
    one past the end, so the result is clamped. *)
let column_of_offset t offset =
  Int.min (t.size - 1) (Int.max 0 (int_of_float (offset *. float_of_int t.size)))

let clamp v = Int.min 255 (Int.max 0 v)

(** A solid (fully opaque) pattern from a brightness function. *)
let generate ?(size = size) f =
  {
    size;
    texels =
      Array.init (size * size) (fun i ->
          clamp (f ~u:(i mod size) ~v:(i / size)));
    alpha = Array.make (size * size) 255;
    opaque = true;
  }

(** A pattern that can see through itself: [f] returns a brightness {e and} an
    alpha for each texel, so a wall wearing it unveils whatever is behind. *)
let generate_masked ?(size = size) f =
  let n = size * size in
  let texels = Array.make n 0 and alpha = Array.make n 0 in
  let opaque = ref true in
  for i = 0 to n - 1 do
    let brightness, a = f ~u:(i mod size) ~v:(i / size) in
    texels.(i) <- clamp brightness;
    alpha.(i) <- clamp a;
    if a < 255 then opaque := false
  done;
  { size; texels; alpha; opaque = !opaque }

(** Read a pattern from a PNG or JPEG file, its colours reduced to the one
    brightness channel a pattern keeps and its alpha taken as it stands. A file
    with no alpha of its own arrives solid.

    The file must be {e square}, because a pattern tiles a square world cell and
    a rectangle would be silently stretched across it — but it may be square at
    any size, since nothing in sampling one cares which. *)
let load path =
  let* s = Surface.read path in
  let w = s.Surface.width and h = s.Surface.height in
  if w <> h then
    Error
      (`Msg
         (Printf.sprintf "%s: a pattern must be square, and this one is %dx%d"
            path w h))
  else begin
    let n = w * w in
    let texels = Array.make n 0 and alpha = Array.make n 0 in
    let opaque = ref true in
    for v = 0 to w - 1 do
      for u = 0 to w - 1 do
        let i = (v * w) + u in
        texels.(i) <- Surface.luminance s ~x:u ~y:v;
        let a = Surface.alpha s ~x:u ~y:v in
        alpha.(i) <- a;
        if a < 255 then opaque := false
      done
    done;
    Ok { size = w; texels; alpha; opaque = !opaque }
  end

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

    The lattice {b wraps} at [size], which is the whole reason this is here
    rather than in a caller. A wall's pattern repeats once per world unit, so a
    field that did not wrap would put a hard seam down every wall in the game,
    one per unit — the very thing noise is being used to avoid.

    [size] is therefore the size of the pattern being built, and it is required
    rather than defaulted on purpose. A default would be right until the first
    time someone wrote [generate ~size:128] over a [noise] left at 64, which
    wraps twice inside each tile and puts back exactly the seam this wrapping
    exists to remove — silently, and only every other 64 texels. Naming it at
    both ends costs a few characters and makes that unwritable.

    [cell] must divide [size], or the lattice would not close on itself. *)
let noise ~size ~seed ~cell ~u ~v =
  if cell <= 0 || size mod cell <> 0 then
    invalid_arg "Texture.noise: cell must divide the pattern size";
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
