(** Decodes a picture file into plain RGBA bytes. This is the only module in the
    engine that knows what a PNG is.

    {!Texture} and {!Image} both need the same thing from a file — a rectangle
    of red, green, blue and alpha — and differ only in what they do with it
    afterwards. The decoding work is done once here, and each of them is a short
    loop over the result. SDL returns a surface in whatever format the file
    used: possibly palettized, possibly three bytes per pixel, possibly with
    padded rows, possibly without alpha. Converting to one known format first
    reduces all of that to a single byte order.

    This module carries no content and makes no decisions; it is a decoder. What
    a picture means (a wall's surface, a poster's paint) belongs to the module
    that asked for it.

    This is an engine seam, not a game API: a game reaches a picture through
    {!Image.load} or {!Texture.load}, both of which are a short loop over what
    {!load} returns. Nothing below takes or returns an SDL value. The codecs are
    initialised on the first {!load} and never again. *)

type t = private { width : int; height : int; rgba : Bytes.t }
(** A decoded picture: [width * height] pixels, row major, four bytes each in
    the order red, green, blue, alpha. [Bytes.t] rather than an [int array]
    because a caller reads every channel exactly once and then discards this;
    the long-lived arrays are {!Image}'s and {!Texture}'s.

    Private: the three fields stay readable, but {!load} is the only
    constructor. A hand-written record with [rgba] the wrong length for its
    [width] and [height] would read off the end of the buffer. *)

val load : string -> (t, [ `Msg of string ]) result
(** [load path] decodes the picture at [path], converting from its stored format
    to the one known byte order. Both SDL surfaces are freed on every return
    path, including when a caller's loop over the result raises.

    The error is [`Msg] carrying SDL_image's own message; a missing file and a
    file that is not a picture both arrive this way, since the engine cannot
    distinguish them before trying. One further error is produced by this module
    itself: a picture with more pixels than [rgba] can hold bytes. At four
    channels per pixel that limit is a quarter of [Sys.max_string_length]. A
    32-bit build reaches that ceiling with an ordinary texture, and it is one
    texel {e under} the array ceiling {!Image.load} and {!Texture.load} check
    for themselves; the refusal must happen here, or [Bytes.create] would raise
    and break their [result] types. See {!Extent.fits} for the shape of the
    check. *)

val sample : t -> u:int -> v:int -> Color.t * int
(** [sample picture ~u ~v] is the colour of that pixel and its alpha: 0 for
    fully clear, 255 for fully solid. A file with no alpha channel has been
    converted to one that has, and arrives solid throughout.

    Unchecked: [u] must be below [width] and [v] below [height]. Callers walk
    the whole rectangle they just read, so a bounds test would run once per
    pixel of every picture the game loads. *)
