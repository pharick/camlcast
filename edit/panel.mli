(** A box of text drawn over a finished frame.

    Both of this library's views are lists of lines in a rectangle, so the
    rectangle is here and the lines are theirs. Nothing decides what to say.

    {1 How much room there is}

    Less than a window suggests, and this is where that is worked out rather
    than discovered. The engine renders at whatever whole-number fraction of the
    window keeps it under {!Camlcast.Config.max_render_height}, which is 480,
    and a HUD is drawn in the framebuffer's pixels rather than the window's. So
    a panel down the side of a maximised window has
    {b 480 pixels of height at most} however large the display is, and at the
    six-by-ten typeface the demos use that is forty-eight lines for everything —
    title, tree, slots and whatever else.

    {!fits} is that arithmetic, exported so a view can decide what to leave out
    before it has drawn anything. A view with more to say than room to say it
    should say the part that matters: {!Tree.remounting} exists for exactly that
    reason.

    {1 What it is drawn with}

    A font and two colours, all given. The engine holds no content and neither
    does this: an overlay over a game should look like it belongs to that game.
*)

type line = { text : string; color : Camlcast.Color.t }
(** One line, and what colour to draw it. *)

val square : across:int -> down:int -> int * int * int * int
(** A square box in the corner of a buffer this size: [(x, y, side, side)].

    What a view of the world wants, as against a list of text. A room drawn
    from above has two dimensions that matter and a column down one side gives
    it one of them; a plan in a third of a five-hundred-pixel buffer is a
    letterbox.

    The same shape {!Camlcast.Debug_map.panel} places its map in, and for the
    same reason — it is not shared with that one because this library sits
    above camlcast and what two libraries share has to sit below both, which a
    number this small does not earn. *)

val fits : font:Camlcast.Font.t -> height:int -> int
(** How many lines of this font fit in a box this tall, the title included.

    Never negative. A box too short for one line fits none. *)

val draw :
  font:Camlcast.Font.t ->
  backing:Camlcast.Color.t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  line list ->
  Camlcast.P.t
(** A backing rectangle with these lines over it, clipped to what {!fits}.

    Lines past that are dropped rather than drawn outside the box: a panel that
    spilled would be drawn over the game and be unreadable against it, and one
    that overran the buffer would be clipped by {!Camlcast.P} anyway and look
    like a bug in the view. Deciding what to drop belongs to the view, which
    knows which of its lines matter; this only refuses to lie about the room. *)
