(** Reading a picture file into plain bytes: the one place in the engine that
    knows what a PNG is.

    {!Texture} and {!Image} both want the same thing from a file — a rectangle
    of red, green, blue and alpha — and differ only in what they make of it
    afterwards, so the awkward half is done once here and each of them is a
    short loop over the result. The awkward half is real: SDL hands back a
    surface in whatever format the file happened to be in, which may be
    palettized, may be three bytes per pixel, may have its rows padded, and may
    have no alpha at all. Converting to one known format first turns all of that
    into a single byte order.

    Nothing here is content, and nothing here decides anything: it is a decoder.
    What the picture means — a wall's surface, a poster's paint — belongs to the
    module that asked for it.

    This is an engine seam rather than a game's: a game reaches a picture
    through {!Image.load} or {!Texture.load}, both of which are a short loop
    over what {!load} returns. Nothing below takes or returns an SDL value, and
    the codecs are started on the first {!load} and never again. *)

type t = private { width : int; height : int; rgba : Bytes.t }
(** A decoded picture: [width * height] pixels, row major, four bytes each in
    the order red, green, blue, alpha. Bytes rather than an [int array] because
    a caller reads every channel exactly once and then throws this away — the
    long-lived arrays are {!Image}'s and {!Texture}'s.

    Private: the three fields stay readable, and {!load} is the only thing that
    can put a picture together — a hand-written record with [rgba] the wrong
    length for its [width] and [height] would read off the end of it. *)

val load : string -> (t, [ `Msg of string ]) result
(** [load path] decodes the picture at [path], converting whatever format it was
    stored in to one known byte order. Both SDL surfaces are freed however this
    returns, including if a caller's loop over the result raises.

    The error is [`Msg] carrying SDL_image's own message — a missing file and a
    file that is not a picture both arrive this way, since neither is something
    the engine can tell apart before trying. One more file arrives as this
    module's own: a picture with more pixels than [rgba] can hold bytes, which
    at four channels per pixel is a quarter of [Sys.max_string_length] — a
    ceiling a 32-bit build meets in an ordinary texture, and one texel {e under}
    the array ceiling {!Image.load} and {!Texture.load} check for themselves, so
    the refusal has to be made here or their [result] types are broken from
    underneath by [Bytes.create]. See {!Extent.fits} for the shape of the check.
*)

val sample : t -> u:int -> v:int -> Color.t * int
(** [sample picture ~u ~v] is the colour of that pixel and how solid it is, the
    alpha being 0 for fully clear and 255 for fully solid. A file with no alpha
    channel of its own has been converted to one that has, and arrives solid
    throughout.

    Unchecked: [u] must be below [width] and [v] below [height]. A caller is
    walking the whole rectangle it just read, and the bounds test would be paid
    once per pixel of every picture the game loads. *)
