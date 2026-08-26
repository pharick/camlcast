(** Writing a new component, and moving part of one into a new file.

    {1 What it is for}

    Not for generating levels. Nothing here owns a file after it has written
    it: what comes out is ordinary hand-owned code that happens to have been
    typed for you, with no marker comment, no ownership, and nothing the editor
    will later refuse to let you change by hand.

    What it is for is the boilerplate that has to be right and is easy to get
    wrong. Two things about a component are correctness rules rather than
    style, and both are invisible when broken:

    - {b declared at the top level.} A component built inside another
      component's render is a fresh closure every frame, so it is never the
      same component as last frame's; it is torn down and rebuilt along with
      everything under it, and the symptom is that state appears not to work.
      {!Camlcast.Element} states the rule and
      {!Camlcast_edit.Tree.remounting} is how you find out you broke it.
    - {b written as a partial application.} [Element.declare ~name render] is
      not a value, so its type variable is weak and fixes to the first props
      type it is used at. Writing the constructor out as a function instead —
      [let torch ?key p = declare ... ?key p] — hands the variable back, and
      with it the unsound cast the rule exists to prevent.

    A scaffolder holding both by construction is worth more than a page saying
    so.

    {1 Splitting a file is a decision about meaning}

    {!extract} is the same writer pointed at a selection: it puts the chosen
    elements in a component of their own and leaves a call where they were.
    There is no size at which it offers to do this. A twelve-sided plaza with
    six pillars is legitimately long, and a file is split when part of it has a
    name — "the gallery wall" — rather than when it passes a threshold nobody
    chose. *)

type file = { path : string; source : string }
(** A file to be written, and what to write in it. Nothing here touches a
    disk; what to do with this is the caller's. *)

val component : name:string -> body:string -> file
(** A new component called [name], returning [body].

    [name] is both the module's file name and the name the component reports
    itself by — in a trace, in a {!Camlcast.Check} diagnostic, and in the
    editor's own tree. They are the same name because two would be one more
    thing to keep in step.

    @raise Invalid_argument
      if [name] could not be an OCaml module: empty, or holding anything but
      lowercase letters, digits and underscores. Written as a raise rather than
      a result because the caller chose the name a moment ago and can only have
      chosen a legal one; this is a mistake in the program, not a condition it
      can be in. *)

val extract :
  Span.t ->
  name:string ->
  Span.span ->
  (file * (Span.span * string), [ `Msg of string ]) result
(** Move the text this span covers into a component called [name], and hand
    back both the file to write and the edit that replaces the span with a call
    to it.

    The two go together and are returned together: writing one without the
    other leaves a file that does not compile, in one direction or the other.

    Refused if the span is empty, or covers nothing but blank space.

    {b The move is textual, and a selection that reads its surroundings will
       not compile where it lands.} Elements written out in full travel
    perfectly; one mentioning a [let] from the function around it, or the
    argument of the component it was in, arrives somewhere that has neither.
    Working out which names a selection depends on means resolving scopes, and
    a wrong answer either refuses a legal extraction or produces a file that
    does not build — while the compiler gives the right answer on the next
    build, naming the line. So this does the move and lets that happen, which
    is the same choice {!Camlcast_edit.Field} makes about a name it cannot know
    is in scope. *)
