(** A bitmap font: one picture of every glyph, laid out on a fixed grid.

    The grid is the whole design. A glyph's place in the atlas is arithmetic on
    its code point, and its advance is the cell width, so a font is four numbers
    and a picture — there is no metrics table to author beside the PNG, to
    parse, or to let drift out of step with it. The cost is that the text is
    monospaced, which for a journal, a death screen and a lamp warning at this
    resolution is not much of a cost.

    {b A character here is a byte.} The cell is the byte less {!t.first}, and
    {!measure}, {!wrap} and {!draw} all step a string one byte at a time — so in
    ASCII, or in Latin-1 with an atlas that reaches that far, a byte is a
    character and everything here reads as written. In UTF-8 it does not: a
    two-byte letter is measured two cells wide, drawn as two glyphs, and may be
    wrapped between its own halves. Nothing raises and nothing is inconsistent —
    the three agree with each other, which is why it comes out as a tidy layout
    of something that is not the text. More than one byte to a character wants
    an atlas and an index built for it, and this module is the grid.

    {b The atlas's colour is multiplied by the one {!draw} is given}, so a
    typeface drawn white comes out in whatever colour a screen asks for. That is
    the same split {!Texture} makes between a pattern and the {!Material}
    wearing it, for the same reason: one file, dressed at the point of use,
    rather than one file per colour.

    Sizes are in framebuffer pixels, and the framebuffer is the internal one —
    {!Renderer} caps it at {!Config.max_render_height} rows and the GPU
    stretches the result to the window. So a glyph is drawn once at whatever
    size the atlas says and scaled with everything else, which is why a font for
    this engine is designed small and legible rather than large and smooth.

    The typeface itself is content and lives outside the engine, like every
    other picture. This module is the grid, the layout and the drawing. *)

type t = private {
  atlas : Image.t;  (** the picture every glyph is cut from *)
  width : int;  (** one cell, and so one glyph's advance, in pixels *)
  height : int;  (** one cell, and so one line's height *)
  columns : int;  (** cells across the atlas *)
  first : int;  (** the code point of the top-left cell *)
  fallback : char option;
      (** what to draw for a character the grid has no cell for *)
}
(** A typeface: the picture, and the four numbers that say how to read it.

    Private rather than abstract, because the fields have to stay readable. A
    screen laying text out asks for [height] to know where its next line goes —
    the demos' journal and menu both do, once per line — and reading it as a
    field is what keeps a layout calculation looking like arithmetic instead of
    a series of calls.

    What private buys is the other half: {!make} becomes the only way to arrive
    at one, so the invariants below hold of every font that exists rather than
    of every font built the recommended way. [width] and [height] are positive
    and [columns] is at least one, which is what everything that reads a font
    divides by — {!capacity} by [height], {!cell} by [columns], {!wrap} by
    [width]. None of the three guards itself, because there is nothing sensible
    to do at that point: a hand-written record of no size raises
    [Division_by_zero] in the middle of drawing a frame, which is a long way
    from the mistake that caused it. *)

val make :
  ?fallback:char ->
  atlas:Image.t ->
  width:int ->
  height:int ->
  first:int ->
  unit ->
  t
(** [make ~atlas ~width ~height ~first ()] describes the grid [atlas] is laid
    out on. [columns] is not asked for: it is the atlas's own width over the
    cell's, so the only description of a font that can be written is one that
    agrees with its picture.

    [fallback] is drawn in place of any character outside the grid. Giving one
    is worth it wherever the text is not entirely the game's own — a
    player-authored journal entry with something unexpected in it should show a
    box saying so rather than a gap that reads as a bug in the layout. Without
    one, such a character takes its space and draws nothing.

    @raise Invalid_argument
      if the cell has no positive size, or if it is larger than the atlas in
      either direction. A grid of one cell is a font; a grid of none is a
      description that disagrees with its picture, and disagreeing quietly is
      the worst of the options: too wide and every glyph is the clipped left
      edge of the atlas, too tall and {!capacity} is zero, so every character
      draws nothing at all. Both look like a bug in the text rather than in the
      font it was asked for. *)

val capacity : t -> int
(** How many cells the atlas actually holds. *)

val cell : t -> char -> (int * int) option
(** Where [c]'s glyph starts in the atlas, or [None] if the grid does not reach
    that far. Honest about what the typeface actually has: the substitution for
    what it does not is {!glyph}'s. *)

val glyph : t -> char -> (int * int) option
(** What {!draw} will actually put in [c]'s place: its own cell, the fallback's,
    or nothing. *)

(** {1 Layout}

    All of it a pure function of a string and the two cell dimensions, which is
    where the tests are — nothing below this point needs a framebuffer, or SDL,
    or a window. *)

val measure : t -> string -> int * int
(** The pixel width and height [text] would take: the longest line by the widest
    it could be, and a line's height for each. Empty text is nothing wide and
    one line tall, because a caret still has somewhere to sit. *)

val wrap : t -> string -> width:int -> string list
(** [wrap t text ~width] breaks [text] into lines no wider than [width] pixels,
    on spaces where it can.

    Greedy, which is what a fixed-width font wants — the clever algorithms buy
    an evenness that monospaced text does not have to begin with. A word longer
    than the whole line is broken rather than allowed to overflow, since a
    journal that runs off its own page is worse than one that hyphenates
    nothing. Newlines already in [text] are kept as breaks, including the ones
    that break onto nothing: the blank line two in a row make, and the empty
    last line a trailing one opens. That is the rule {!measure} already counts
    by, so text wrapped and then measured is the height it is drawn at — a
    paragraph break survives wrapping instead of closing up. *)

(** {1 Drawing} *)

val draw :
  Framebuffer.t -> t -> string -> x:int -> y:int -> color:Color.t -> unit
(** Draw [text] with its top-left corner at [(x, y)], in the colour given.

    Every glyph goes through {!Paint.sub}, so it is clipped against the buffer
    like everything else drawn over a frame and a line running off the edge is
    cut rather than wrapped or refused. A character the atlas has no cell for
    takes its space either way — see {!glyph} — because a substitution that
    changed the width would move everything after it and turn one wrong
    character into a wrong line. *)
