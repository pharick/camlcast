(** The machinery for surface patterns, generated in code or read from a file.

    A texel is a {!Color.t}, so a pattern says what a surface looks like and not
    merely how bright it is at each point. A wall can therefore have more than
    one colour in it — rust on iron, a painted band, tile grout a different
    colour from the tile — which is the thing a pattern that carried only a
    brightness could never express, however it was dressed afterwards.

    A word on the word. A [t] is a {e built} pattern: two arrays, fixed at the
    size it was made at. The function handed to {!generate} is a {e recipe} for
    one — a colour at every point, at any size asked for, in as many colours as
    it has arguments. Both get called patterns, here and in a game's own
    modules, and which is meant is always the type: a recipe cannot be sampled
    and a built one cannot be applied to anything.

    {1 One pattern at many colours}

    Brightness-only patterns bought one real thing: the same masonry could be
    red brick in one room and grey stone in the next, because the colour came
    from elsewhere. That survives here, by writing the pattern as a function of
    its colour and applying it partially:

    {[
      let brick ~color ~u ~v =
        Color.level color (if in_mortar ~u ~v then 130 else 225)

      let red = generate (brick ~color:(Color.rgb 200 70 70))
      and grey = generate (brick ~color:(Color.rgb 150 146 140))
    ]}

    {!Color.level} is what makes that read as one line: it takes the 0 .. 255 a
    pattern naturally computes — {!noise} and {!hash} both speak in it — and
    scales a colour by it, moving value without touching hue. A pattern that
    wants two colours in it simply does not go through [level].

    The reuse is now explicit rather than free, and it costs an array per
    colour where before two materials shared one. That is the trade: a pattern
    is three times the memory and cannot be re-dressed after the fact, in
    exchange for being able to say what it actually looks like.

    {1 Size}

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
  texels : Color.t array;  (** row major *)
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

(** A solid (fully opaque) pattern from a colour function. [f] is clamped rather
    than trusted, because a pattern is usually arithmetic about a base value and
    the ends of its range are exactly where that arithmetic leaves 0 .. 255. *)
let generate ?(size = size) f =
  {
    size;
    texels =
      Array.init (size * size) (fun i ->
          Color.clamp (f ~u:(i mod size) ~v:(i / size)));
    alpha = Array.make (size * size) 255;
    opaque = true;
  }

(** A pattern that can see through itself: [f] returns a colour {e and} an alpha
    for each texel, so a wall wearing it unveils whatever is behind. *)
let generate_masked ?(size = size) f =
  let n = size * size in
  let texels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
  let opaque = ref true in
  for i = 0 to n - 1 do
    let color, a = f ~u:(i mod size) ~v:(i / size) in
    texels.(i) <- Color.clamp color;
    alpha.(i) <- clamp a;
    if a < 255 then opaque := false
  done;
  { size; texels; alpha; opaque = !opaque }

(** Read a pattern from a PNG or JPEG file, colour and alpha both, exactly as
    they were drawn. A file with no alpha of its own arrives solid.

    Nothing is reduced or reinterpreted on the way in, so what a painting
    program showed is what a wall wearing this will show, under whatever the
    {!Atmosphere} does to it. That is the whole reason to read a file rather
    than write a function: a generated pattern is testable, and a drawn one is
    drawn.

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
    let texels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
    let opaque = ref true in
    for v = 0 to w - 1 do
      for u = 0 to w - 1 do
        let i = (v * w) + u in
        let color, a = Surface.sample s ~x:u ~y:v in
        texels.(i) <- color;
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
