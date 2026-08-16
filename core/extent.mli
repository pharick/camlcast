(** Checks whether a rectangle of pixels is a length an array can have.

    One line of arithmetic. It is a module because three records each carried
    their own copy with its own explanatory paragraph, and because two of those
    copies were not quite the same check — a difference invisible while each was
    a private [fits] in its own file. *)

val fits : limit:int -> width:int -> height:int -> bool
(** Whether [width * height] is at most [limit], for positive [width] and
    [height].

    {b Divided rather than multiplied}; that is the reason this is written down.
    Multiplied, the check overflows in exactly the case it exists to catch: past
    the bound the product wraps instead of growing — [max_int] squared is [1] —
    so [width * height <= limit] comes out true. The caller then allocates an
    array shorter than the size it goes on to report, leaving a record claiming
    extents its storage cannot honour, read by arithmetic every sampler trusts
    without checking. Division cannot wrap.

    {b [limit] is the caller's} because it is not the same number for everyone.
    {!Texture} and {!Image} hold [Color.t array]s and pass
    [Sys.max_array_length]. {!Framebuffer} passes [Sys.max_floatarray_length]:
    its depth buffer is a [float array], the tighter of its two bounds — its
    pixels are a bigarray outside the heap that only memory limits. The two
    limits are equal at 64 bits and differ at 32, where a float array holds half
    as many, so the difference is real portability rather than pedantry; it is
    now a word at each call site instead of a number buried in three
    similar-looking functions.

    Positivity is also the caller's, and must be checked first: a [width] of
    zero divides by zero inside this test, and a negative [height] passes it.
    Each caller checks it itself, in its own words, so the error message can
    name the thing being built — a pattern, a picture, a buffer. *)
