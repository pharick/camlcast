(** Small colour images with a per-pixel alpha channel: the pictures that are
    {e not} part of a surface's own material — the decals hung on walls and the
    sprites that stand in the world.

    Both this and a {!Texture} are colour with an alpha, and the difference is
    what they are for rather than what they hold. A texture is
    {e part of a surface}: square, and tiling once per world cell, so its size
    is a density and its shape is not its own. A picture stands on its own — a
    painting framed against the wall, a character cut out from the empty space
    around it — so it keeps whatever shape it was drawn at and is drawn once
    rather than repeated.

    An image is a rectangle and not a square, because the things it is used for
    are: a poster is wider than it is tall, a standing figure is taller than it
    is wide, and a file on disk is whatever shape it was drawn at. Whoever
    samples one has to keep [width] and [height] apart — {!Room.decal_column}
    against the first, {!Room.decal_row} against the second — since getting them
    the wrong way round is a mirror image and not an error.

    Like the textures, this module is the machinery only: the type, the
    generator, the loader and the samplers. The pictures themselves are content
    and live with whatever draws them. *)

type t = private {
  width : int;
  height : int;
  pixels : Color.t array;  (** row major *)
  alpha : int array;  (** row major, 0 (clear) .. 255 (solid) *)
}
(** A picture: how big it is, and its colour and opacity at every pixel of it.

    Private rather than abstract, where {!Texture.t} and {!Room.t} are abstract,
    and the difference is what a picture costs to read. {!Paint.sub} and
    {!Renderer} index [pixels] and [alpha] directly in their inner loops — once
    per pixel of every sprite and every decal on the screen — and an accessor
    call there would be paid on all of them, which is the whole reason {!index}
    exists apart from {!sample}. A pattern is asked its size once per wall per
    column and a room is walked a part at a time; a picture is read per pixel,
    and that is the loop the frame is spent in.

    What private buys is the other half: {!make} and {!load} become the only
    ways to arrive at one, so the invariants below hold of every picture that
    exists rather than of every picture built the recommended way. [pixels] and
    [alpha] are both exactly [width * height] long and both row major, so
    {!index} is the one place that arithmetic is written down; and [width] and
    [height] are both positive, which is what lets anything divide by them. A
    hand-written record with an array of the wrong length reads off the end of
    it, and one of no size divides by zero somewhere in a frame — see
    {!Room.sprite_half_width}, which divides by [height].

    {b What it does not buy.} Private stops a picture being built by hand; it
    cannot stop a caller writing into the two arrays it can read, so
    [img.Image.pixels.(0) <- c] type-checks and the sentence above about every
    picture that exists is a sentence about how every picture {e arrives}. That
    is affordable here and it is not everywhere: the worst a written-into
    picture can do is draw wrong wherever it is drawn, since nothing is derived
    from the pixels and nothing else holds a fact about them. A {!Texture.t}
    carries [opaque], which a write would make a lie and the renderer would act
    on; a {!Room.t} is held by a {!World.t} in a row parallel to its thresholds,
    shared with every world grown from it. Both are abstract for that reason,
    and neither is read per pixel. *)

val make : width:int -> ?height:int -> (u:int -> v:int -> Color.t * int) -> t
(** Build a [width] x [height] image from a function of the pixel coordinates,
    returning the colour and alpha at each. [height] defaults to [width], since
    a generated picture is usually square and saying so twice reads worse than
    not saying it.

    [width] first, as it is in {!Framebuffer.offscreen}, {!Viewport.make} and
    {!Extent.fits}. It used to come second, behind the optional [height], which
    is the one order an extent is never written in and was not forced by
    anything: an optional argument only has to be followed by something, and the
    generator is something.

    Both are clamped into 0 .. 255 rather than trusted, the same way
    {!Texture.generate_masked} clamps what a pattern hands it: a picture is
    usually arithmetic about a base value, and the ends of its range are exactly
    where that arithmetic leaves the byte. It is what makes the [alpha] range
    above true of every picture rather than of every carefully written one —
    {!Renderer} passes that alpha to {!Framebuffer.blend} unchecked, and one
    above 255 there weighs the destination negatively and wraps.

    @raise Invalid_argument
      if [width] or [height] is not positive, or if [width * height] is longer
      than an array can be. A picture of no size is an authoring mistake and not
      a condition to handle: nothing can be drawn from it, and everything that
      measures one divides by a side of it. One too large is the same mistake
      wearing a plausible number — past the longest array the product wraps
      rather than growing, and the picture would keep the size it was written
      down with while holding a fraction of the pixels, which {!index} works out
      and {!sample} reads without checking. *)

val index : t -> u:int -> v:int -> int
(** The flat array index of pixel [(u, v)]; the caller has already clamped them
    into range. Kept separate so the hot drawing loop can read [pixels] and
    [alpha] directly without allocating. *)

val sample : t -> u:int -> v:int -> Color.t * int
(** The colour and alpha of pixel [(u, v)]. *)

val load : string -> (t, [ `Msg of string ]) result
(** Read a picture from a PNG or JPEG file, colour and alpha both. A file with
    no alpha channel of its own arrives fully solid, which is what a photograph
    or a JPEG means by having none.

    Generating a picture in code is still the other way in and still what every
    demo does. This is for the ones that were drawn rather than derived.

    A result and not an exception, because a file is a run-time failure: it may
    be missing, or not be a picture, or — the two {!make} would otherwise raise
    on — decode to no pixels at all, or to more of them than an array can hold.

    {b Both of {!make}'s refusals and not just the first}, which is worth saying
    because for a while it was only the first. A size {!make} will not take,
    reached from here, is an [Invalid_argument] coming out of a function whose
    type says a bad file is a condition — the promise broken from the inside,
    and by the one input a caller has least control over. The second is out of
    reach of any file a 64-bit build can decode and is an ordinary texture at 32
    bits, where an array holds 2^22 entries and a 2048 by 2048 picture is one
    past it; see {!Extent.fits} on why that difference is real. *)

val of_asset : string -> (t, [ `Msg of string ]) result
(** {!load}, given an asset's name instead of a path: {!Asset.path} finds where
    the file lives relative to the running executable, and the picture is read
    from there. The error is whichever step's it was — the roots that were
    searched, or what was wrong with the file they turned up. The twin of
    {!Texture.of_asset}, for the same reason. *)

val disc : cx:float -> cy:float -> r:float -> u:int -> v:int -> bool
(** Is pixel [(u, v)] inside the circle of radius [r] about [(cx, cy)]? A
    generator helper, here rather than repeated in every module that draws a
    round thing — labeled as a generator's own coordinates are. *)

val clear : Color.t * int
(** Nothing at all: the value a generator returns for a pixel outside the shape
    it is cutting out. *)
