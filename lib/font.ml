(** A bitmap font: one picture of every glyph, laid out on a fixed grid.

    The grid is the whole design. A glyph's place in the atlas is arithmetic on
    its code point, and its advance is the cell width, so a font is four numbers
    and a picture — there is no metrics table to author beside the PNG, to
    parse, or to let drift out of step with it. The cost is that the text is
    monospaced, which for a journal, a death screen and a lamp warning at this
    resolution is not much of a cost.

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

type t = {
  atlas : Image.t;
  width : int;  (** one cell, and so one glyph's advance, in pixels *)
  height : int;  (** one cell, and so one line's height *)
  columns : int;  (** cells across the atlas *)
  first : int;  (** the code point of the top-left cell *)
  fallback : char option;
      (** what to draw for a character the grid has no cell for *)
}

(** [make ~atlas ~width ~height ~first] describes the grid [atlas] is laid out
    on. [columns] is not asked for: it is the atlas's own width over the cell's,
    so the only description of a font that can be written is one that agrees
    with its picture.

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
let make ?fallback ~atlas ~width ~height ~first () =
  if width <= 0 || height <= 0 then
    invalid_arg "Font.make: a cell must have a positive size";
  if width > atlas.Image.width || height > atlas.Image.height then
    invalid_arg "Font.make: a cell must fit in the atlas";
  (* No [Int.max 1] here: the check above is what makes the division at least
     one, and so what keeps [cell]'s [mod columns] from dividing by zero. *)
  { atlas; width; height; columns = atlas.Image.width / width; first; fallback }

(** How many cells the atlas actually holds. *)
let capacity t = t.columns * (t.atlas.Image.height / t.height)

(** Where [c]'s glyph starts in the atlas, or [None] if the grid does not reach
    that far. Honest about what the typeface actually has: the substitution for
    what it does not is {!glyph}'s. *)
let cell t c =
  let n = Char.code c - t.first in
  if n < 0 || n >= capacity t then None
  else Some (n mod t.columns * t.width, n / t.columns * t.height)

(** What {!draw} will actually put in [c]'s place: its own cell, the fallback's,
    or nothing. *)
let glyph t c =
  match cell t c with
  | Some _ as found -> found
  | None -> Option.bind t.fallback (cell t)

(** {1 Layout}

    All of it a pure function of a string and the two cell dimensions, which is
    where the tests are — nothing below this point needs a framebuffer, or SDL,
    or a window. *)

(** The lines of [text], split on newlines. *)
let lines text = String.split_on_char '\n' text

(** The pixel width and height [text] would take: the longest line by the widest
    it could be, and a line's height for each. Empty text is nothing wide and
    one line tall, because a caret still has somewhere to sit. *)
let measure t text =
  let ls = lines text in
  let longest =
    List.fold_left (fun acc l -> Int.max acc (String.length l)) 0 ls
  in
  (longest * t.width, List.length ls * t.height)

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
let wrap t text ~width =
  let per_line = Int.max 1 (width / t.width) in
  let rec split_word word acc =
    if String.length word <= per_line then (word, acc)
    else
      split_word
        (String.sub word per_line (String.length word - per_line))
        (String.sub word 0 per_line :: acc)
  in
  let wrap_line line =
    let words = String.split_on_char ' ' line in
    let flush current out = match current with "" -> out | c -> c :: out in
    let current, out =
      List.fold_left
        (fun (current, out) word ->
          (* A word too long for any line is cut into full lines first; what is
             left of it then joins the flow like any other word. Cutting it
             flushes what was accumulated, so [current] is emptied with it —
             left as it was, the line just flushed would be written out a
             second time below. *)
          let word, current, out =
            if String.length word <= per_line then (word, current, out)
            else
              let rest, out = split_word word (flush current out) in
              (rest, "", out)
          in
          if current = "" then (word, out)
          else if String.length current + 1 + String.length word <= per_line
          then (current ^ " " ^ word, out)
          else (word, current :: out))
        ("", []) words
    in
    (* A line with nothing on it wraps to one empty line and not to none: the
       break was in the text, and dropping it here would leave {!measure} and
       {!draw} counting a line this does not return. *)
    match List.rev (flush current out) with
    | [] -> [ "" ]
    | wrapped -> wrapped
  in
  List.concat_map wrap_line (lines text)

(** {1 Drawing} *)

(** Draw [text] with its top-left corner at [(x, y)], in the colour given.

    Every glyph goes through {!Paint.sub}, so it is clipped against the buffer
    like everything else drawn over a frame and a line running off the edge is
    cut rather than wrapped or refused. A character the atlas has no cell for
    takes its space either way — see {!glyph} — because a substitution that
    changed the width would move everything after it and turn one wrong
    character into a wrong line. *)
let draw fb t text ~x ~y ~color =
  List.iteri
    (fun row line ->
      let y = y + (row * t.height) in
      String.iteri
        (fun col c ->
          match glyph t c with
          | None -> ()
          | Some (sx, sy) ->
              Paint.sub ~tint:color fb t.atlas
                ~x:(x + (col * t.width))
                ~y ~sx ~sy ~sw:t.width ~sh:t.height)
        line)
    (lines text)
