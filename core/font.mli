(** A bitmap font: one picture of every glyph, laid out on a fixed grid.

    The grid is the whole design. A glyph's place in the atlas is arithmetic on
    its code point, and its advance is the cell width, so a font is four numbers
    and a picture — no metrics table to author beside the PNG, to parse, or to
    drift out of step with it. The cost is that text is monospaced, which at
    this resolution is a small cost for a journal, a death screen and a lamp
    warning.

    {b A character here is a byte.} The cell is the byte less {!t.first}, and
    {!measure}, {!wrap} and {!draw} all step a string one byte at a time. In
    ASCII, or in Latin-1 with an atlas that reaches that far, a byte is a
    character and everything reads as written. In UTF-8 it does not: a two-byte
    letter is measured two cells wide, drawn as two glyphs, and may be wrapped
    between its own halves. Nothing raises and the three functions agree with
    each other, so the output is a tidy layout of something that is not the
    text. Multi-byte characters need an atlas and an index built for them; this
    module is the grid.

    {b The atlas's colour is multiplied by the one {!draw} is given}, so a
    typeface drawn white comes out in whatever colour a screen asks for. This is
    the same split {!Texture} makes between a pattern and the {!Material}
    wearing it, for the same reason: one file, coloured at the point of use,
    rather than one file per colour.

    Sizes are in framebuffer pixels, and the framebuffer is the internal one:
    {!Renderer} caps it at {!Config.max_render_height} rows and the GPU
    stretches the result to the window. A glyph is therefore drawn once at the
    atlas's size and scaled with everything else, which is why a font for this
    engine is designed small and legible rather than large and smooth.

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

    Private rather than abstract, because the fields must stay readable. A
    screen laying out text asks for [height] to know where its next line goes —
    the demos' journal and menu both do, once per line — and reading it as a
    field keeps a layout calculation looking like arithmetic instead of a series
    of calls.

    Private also makes {!make} the only constructor, so the invariants below
    hold of every font that exists, not just every font built the recommended
    way. [width] and [height] are positive and [columns] is at least one, which
    is what everything that reads a font divides by — {!capacity} by [height],
    {!cell} by [columns], {!wrap} by [width]. None of the three guards itself,
    because there is nothing sensible to do at that point: a hand-written record
    of no size would raise [Division_by_zero] in the middle of drawing a frame,
    far from the mistake that caused it. *)

val make :
  ?fallback:char ->
  atlas:Image.t ->
  width:int ->
  height:int ->
  first:int ->
  unit ->
  t
(** [make ~atlas ~width ~height ~first ()] describes the grid [atlas] is laid
    out on. [columns] is not a parameter: it is the atlas's width over the
    cell's, so the only writable description of a font is one that agrees with
    its picture.

    [fallback] is drawn in place of any character outside the grid. Give one
    wherever the text is not entirely the game's own: a player-authored journal
    entry with something unexpected in it should show a box saying so, rather
    than a gap that reads as a layout bug. Without one, such a character takes
    its space and draws nothing.

    @raise Invalid_argument
      if the cell has no positive size, or if it is larger than the atlas in
      either direction. A grid of one cell is a font; a grid of none is a
      description that disagrees with its picture, and disagreeing quietly is
      the worst option: too wide and every glyph is the clipped left edge of the
      atlas, too tall and {!capacity} is zero, so every character draws nothing.
      Both look like a bug in the text rather than in the font. *)

(** The three functions below are queries, as opposed to commands. They are the
    arithmetic {!draw} already runs — it reaches a glyph through {!glyph}, which
    asks {!cell}, which asks {!capacity} — so nothing outside this module needs
    them to put text on a screen, and nothing outside it calls them.

    They are exported for a game checking its own typeface: that the atlas
    reaches every character a journal can come to hold, or that a fallback is
    catching the ones it does not. That is worth asking before shipping a
    picture, and {!draw} will not answer it — a character with no cell and no
    fallback draws nothing and reports nothing. *)

val capacity : t -> int
(** How many cells the atlas actually holds. *)

val cell : t -> char -> (int * int) option
(** Where [c]'s glyph starts in the atlas, or [None] if the grid does not reach
    that far. Reports what the typeface actually has; substituting for what it
    does not is {!glyph}'s job. *)

val glyph : t -> char -> (int * int) option
(** What {!draw} will actually put in [c]'s place: its own cell, the fallback's,
    or nothing. *)

(** {1 Layout}

    All of it is a pure function of a string and the two cell dimensions, which
    is where the tests are — nothing below this point needs a framebuffer, SDL,
    or a window. *)

val measure : t -> string -> int * int
(** The pixel width and height [text] would take: the longest line at its
    widest, and a line's height for each line. Empty text is zero wide and one
    line tall, because a caret still needs somewhere to sit. *)

val wrap : t -> string -> width:int -> string list
(** [wrap t text ~width] breaks [text] into lines no wider than [width] pixels,
    on spaces where possible.

    Greedy, which suits a fixed-width font: cleverer algorithms buy an evenness
    monospaced text does not have to begin with. A word longer than the whole
    line is broken rather than allowed to overflow, since a journal running off
    its own page is worse than one that hyphenates nothing. Newlines already in
    [text] are kept as breaks, including those that break onto nothing: the
    blank line two in a row make, and the empty last line a trailing one opens.
    That is the rule {!measure} counts by, so text wrapped and then measured is
    the height it is drawn at — a paragraph break survives wrapping instead of
    closing up. *)

(** {1 Drawing} *)

val draw :
  Framebuffer.t -> t -> string -> x:int -> y:int -> color:Color.t -> unit
(** Draw [text] with its top-left corner at [(x, y)], in the given colour.

    Every glyph goes through {!Paint.sub}, so it is clipped against the buffer
    like everything else drawn over a frame; a line running off the edge is cut
    rather than wrapped or refused. A character the atlas has no cell for still
    takes its space — see {!glyph} — because a substitution that changed the
    width would shift everything after it and turn one wrong character into a
    wrong line. *)
