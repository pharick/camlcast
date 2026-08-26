(** The parts of a call an editor can change, one at a time.

    {1 One rule instead of five}

    A sprite's size, a decal's height up the wall, a floor's plane, the sky a
    room is open to, the air a world is seen through — the plan calls these
    five different things and they are one thing here. {!Span} has already
    said, per argument, whether it was written as a number, as a name, or as
    something worked out; this flattens that answer into a list of places a
    value sits, and offers to write a new one.

    Nothing here knows what a sprite is. That is the point: a constructor
    added to {!Camlcast.P} is editable the day it exists, without this module
    learning about it.

    {1 What a label can honestly say}

    A labelled argument is called what the description called it — [height],
    [glow], [along]. A positional one has no name to give: [P.wall]'s two
    points are [a] and [b] in its signature and nothing in the source says so,
    so they are numbered. Where one argument holds several numbers — a point
    is two, a pair of points is four — they are numbered within it.

    Guessing the names would read better and would be a guess, wrong the first
    time an argument list changed. *)

type value =
  | Number of float
  | Name of string
      (** What is written there now. A number can be typed over or dragged; a name
    can be swapped for another name. *)

type t = {
  label : string;  (** [height], [#2.0], [along.1] *)
  span : Span.span;
  value : value;
}

val of_call : Span.t -> Span.call -> t list
(** Every place in this call that holds a value which could be another.

    Arguments {!Span.Computed} contribute nothing — an expression worked out
    from state is not a place a value sits, it is a place a rule sits — so a
    call with none of them comes back empty and a call with some comes back
    shorter than its argument list. That is the one predicate again, per
    argument. *)

val write : t -> value -> (Span.span * string, [ `Msg of string ]) result
(** The edit that puts this value there, ready for {!Span.splice}.

    A number is written through {!Span.number}, so a negative one arrives
    bracketed and parses where it lands. A name is written as given and is
    {b not checked}: this module cannot know what is in scope at that point in
    someone's file, and pretending to would refuse valid edits as often as it
    caught invalid ones. What catches a name that is not in scope is the
    compiler, on the next build, saying so about the line the editor just
    wrote.

    Writing a name where a number is, or the reverse, is refused. That is not
    a scope question but a shape one, and the shape is known. *)
