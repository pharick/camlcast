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

    Private rather than abstract, because the fields have to stay readable.
    {!Paint.sub} and {!Renderer} index [pixels] and [alpha] directly in their
    inner loops — once per pixel of every sprite and every decal on the screen —
    and an accessor call there would be paid on all of them, which is the whole
    reason {!index} exists apart from {!sample}.

    What private buys is the other half: {!make} and {!load} become the only
    ways to arrive at one, so the invariants below hold of every picture that
    exists rather than of every picture built the recommended way. [pixels] and
    [alpha] are both exactly [width * height] long and both row major, so
    {!index} is the one place that arithmetic is written down; and [width] and
    [height] are both positive, which is what lets anything divide by them. A
    hand-written record with an array of the wrong length reads off the end of
    it, and one of no size divides by zero somewhere in a frame — see
    {!Room.sprite_half_width}, which divides by [height]. *)

val make : ?height:int -> int -> (u:int -> v:int -> Color.t * int) -> t
(** Build a [width] x [height] image from a function of the pixel coordinates,
    returning the colour and alpha at each. [height] defaults to [width], since
    a generated picture is usually square and saying so twice reads worse than
    not saying it.

    @raise Invalid_argument
      if [width] or [height] is not positive. A picture of no size is an
      authoring mistake and not a condition to handle: nothing can be drawn from
      it, and everything that measures one divides by a side of it. *)

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
    be missing, or not be a picture, or — the one {!make} would otherwise raise
    on — decode to no pixels at all. *)

val disc : cx:float -> cy:float -> r:float -> int -> int -> bool
(** Is [(u, v)] inside the circle of radius [r] about [(cx, cy)]? A generator
    helper, here rather than repeated in every module that draws a round thing.
*)

val clear : Color.t * int
(** Nothing at all: the value a generator returns for a pixel outside the shape
    it is cutting out. *)
