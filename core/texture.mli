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

    The reuse is now explicit rather than free, and it costs an array per colour
    where before two materials shared one. That is the trade: a pattern is three
    times the memory and cannot be re-dressed after the fact, in exchange for
    being able to say what it actually looks like.

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

val default_size : int
(** Texels per side of a pattern that does not say otherwise: what the
    generators below make when they are not told, and what every pattern in the
    engine's own demos is. A power of two, and small enough that the whole set
    stays comfortably in cache. *)

type t
(** A built pattern: how densely it is written down, its colour and its opacity
    at every texel of it, and whether any of that opacity is worth asking about.

    Abstract, because two of the four things it holds are arrays, and a private
    record would hand those out. A private record cannot be built by hand, which
    is what makes {!generate}, {!generate_masked} and {!load} the only ways to
    arrive at a pattern; it cannot stop a caller writing into an array it can
    read. That is the difference between an invariant that holds of every
    pattern and one that holds until somebody indexes it.

    The invariants are these. [size] is positive. The colour and the opacity are
    both exactly [size * size] long and both row major, which is the one piece
    of arithmetic {!sample} and {!alpha} do and the reason neither bounds-checks
    what it is handed. And {!opaque} agrees with the opacity it is worked out
    from, so a wall that says it is solid is — which is the one a writable array
    would break in silence, leaving {!Renderer} painting a see-through wall
    straight over its column and never reaching the translucent pass at all.

    What it costs is a call where a field read used to be: {!Renderer} asks
    {!val-size} to work out how far down a strip each pixel of a wall falls, and
    {!Material.opaque} asks {!opaque} whether the wall is painted over the
    column or held back, both once per wall per column. That is a call per
    column of every frame and no allocation, which is affordable, and a pattern
    that lies about being solid is not. *)

val size : t -> int
(** Texels per side, and so per world unit of surface. *)

val opaque : t -> bool
(** Whether every texel is fully solid — worked out when the pattern is built,
    not asked of the texels here. *)

val sample : t -> u:int -> v:int -> Color.t
(** The colour of texel [(u, v)]. The caller has already clamped both into
    range; on the drawing path {!column_of_offset} is what does it. *)

val alpha : t -> u:int -> v:int -> int
(** How solid texel [(u, v)] is, 0 (clear) .. 255 (solid), on the same terms. *)

val column_of_offset : t -> float -> int
(** The texel column of [t] that a hit at [offset] across one tile falls in.
    [offset] reaches 1.0 exactly when a ray strikes a corner, which would index
    one past the end, so the result is clamped. *)

val row_of_height : t -> float -> int
(** The texel row of [t] that a point [height] above the foot of a surface falls
    in. The pattern tiles every world unit, so only the fractional part of the
    height comes into it, and the row is measured {e down} from the top: a
    height just over the foot is the bottom row and one just under the next cell
    is the top. That flip is what makes a wall's pattern stand up the right way,
    and it is written here so that {!Renderer} and {!Sight} cannot each have
    their own idea of it.

    It is [column_of_offset]'s rule turned on its side: both scale the fraction
    by [size], so every one of a pattern's rows owns the same band of a cell as
    every one of its columns, and a wall tiles up its height on the same terms
    it tiles along its length. Scaling by [size - 1] instead — as this did until
    the rows were measured — leaves the last row reachable only from a height
    that is exactly a whole number of cells, so the bottom row of every pattern
    has no area at all and the rest stretch to cover for it.

    This is also the arithmetic the wall is {e drawn} with, and anything asking
    what a surface is at a point has to agree with what was drawn there — which
    is why the two questions share this function rather than each spelling it
    out. *)

val generate : ?size:int -> (u:int -> v:int -> Color.t) -> t
(** A solid (fully opaque) pattern from a colour function. [f] is clamped rather
    than trusted, because a pattern is usually arithmetic about a base value and
    the ends of its range are exactly where that arithmetic leaves 0 .. 255.

    @raise Invalid_argument
      if [size] is not positive. A pattern of no size is an authoring mistake
      and not a condition to handle: there is nothing in it to sample, and
      everything that reads one indexes by the side it says it has.
    @raise Invalid_argument
      if [size * size] is longer than an array can be. That product is the
      invariant above, and past the square root of [Sys.max_array_length] it
      wraps rather than growing — a size of [max_int] squares to [1] — so a
      texture would come back claiming a side its two arrays are far too short
      to answer for, and the first {!sample} past the first row would raise from
      inside the stdlib instead of from here. *)

val generate_masked : ?size:int -> (u:int -> v:int -> Color.t * int) -> t
(** A pattern that can see through itself: [f] returns a colour {e and} an alpha
    for each texel, so a wall wearing it unveils whatever is behind. The alpha
    is clamped alongside the colour, and {!opaque} is worked out from what
    arrives rather than taken on trust.

    @raise Invalid_argument
      if [size] is not positive, or if its square is longer than an array can be
      — on both of the terms {!generate} refuses them. *)

val load : string -> (t, [ `Msg of string ]) result
(** Read a pattern from a PNG or JPEG file, colour and alpha both, exactly as
    they were drawn. A file with no alpha of its own arrives solid.

    The [string] is a path on disk, taken as given. A game that ships assets
    wants {!of_asset}, which asks {!Asset} where they live first.

    Nothing is reduced or reinterpreted on the way in, so what a painting
    program showed is what a wall wearing this will show, under whatever the
    {!Atmosphere} does to it. That is the whole reason to read a file rather
    than write a function: a generated pattern is testable, and a drawn one is
    drawn.

    The file must be {e square}, because a pattern tiles a square world cell and
    a rectangle would be silently stretched across it — but it may be square at
    any size, since nothing in sampling one cares which.

    A result and not an exception, because a file is a run-time failure: it may
    be missing, or not be a picture, or be the wrong shape, or — the one
    {!generate} would raise on — decode to no texels at all. *)

val of_asset : string -> (t, [ `Msg of string ]) result
(** {!load}, given an asset's name instead of a path: {!Asset.path} finds where
    the file lives relative to the running executable, and the pattern is read
    from there. The error is whichever step's it was — the roots that were
    searched, or what was wrong with the file they turned up.

    This composition is two lines, and every game was writing them. *)

val hash : int -> int -> int
(** A cheap deterministic hash. Not good randomness by any standard, but enough
    to stop a surface looking machine-made, and reproducible so the tests can
    pin what it produces. *)

val noise : size:int -> seed:int -> cell:int -> u:int -> v:int -> int
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

    Nothing in the engine calls this, and nothing is going to: the engine holds
    no content, so every pattern there is belongs to a game. It is here rather
    than in each of them because value noise is the one generator that is more
    arithmetic than taste — two games wanting a mottled wall want the same
    function, and the wrapping above is the part that is easy to leave out and
    slow to see the absence of.

    @raise Invalid_argument
      if [size] is not positive, or if [cell] does not divide it — the lattice
      would not close on itself, which is the same seam by another route. *)
