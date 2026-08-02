(** Whether a rectangle of pixels is a length an array can have.

    One line of arithmetic, and it is a module because three records were each
    carrying their own copy of it with their own paragraph explaining it, and
    because two of those copies were not quite the same check — a difference
    invisible while each was a private [fits] in its own file. *)

val fits : limit:int -> width:int -> height:int -> bool
(** Whether [width * height] is at most [limit], for positive [width] and
    [height].

    {b Divided rather than multiplied}, which is the whole of why this is worth
    writing down. Multiplied out, the check would overflow in exactly the case
    it exists to catch: past the bound the product wraps rather than growing —
    [max_int] squared is [1] — so [width * height <= limit] comes out true and
    the caller allocates an array shorter than the size it goes on to report.
    What follows is a record claiming extents its own storage cannot answer for,
    read by the one piece of arithmetic every sampler trusts without checking.
    Dividing, there is nothing to wrap.

    {b [limit] is the caller's} because it is not the same number for everyone.
    {!Texture} and {!Image} hold [Color.t array]s and pass
    [Sys.max_array_length]. {!Framebuffer} passes [Sys.max_floatarray_length]:
    its depth buffer is a [float array], which is the tighter of the two bounds
    it has to satisfy, its pixels being a bigarray outside the heap that only
    memory limits. The two are equal at 64 bits and are not at 32, where a float
    array holds half as many — so the difference is real, is portability rather
    than pedantry, and is now a word at each call site instead of a number
    buried in three functions that looked alike.

    Positivity is the caller's too, and has to be asked first: a [width] of zero
    divides by zero inside the test meant to explain itself, and a negative
    [height] sails straight through it. Each caller already asks, in its own
    words, because the message wants to name the thing the caller was building —
    a pattern, a picture, a buffer. *)
