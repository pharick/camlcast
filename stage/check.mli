(** Reading a description for the mistakes a compiler cannot see.

    A world that type-checks can still be wrong in ways only a player finds: two
    doorways joined to the same third one, a room nothing leads to, a spawn
    point inside a wall, a step in the floor where two rooms meet. The engine
    refuses some of these already — but from deep inside {!Camlcast.World.make},
    in a message that names a room and a threshold and has no idea which part of
    a game wrote them.

    A description is data, so it can be read before it is built. That is what
    this is: {!report} takes the same description a frame is drawn from, and
    answers with what is wrong with it and where — [plaza / gallery_wall] rather
    than [World.make: threshold linked twice: plaza.east].

    {1 Where to run it}

    {!report} is a pure function of a description, so the natural home is a
    test: a game gets [dune runtest] coverage of its own levels for the cost of
    one case per level. Until now that has existed only as [test_demos.ml],
    hard-coded in this repository for this repository's demos.

    It is also what a dev build should run whenever the shape of a world
    changes, and what a release build should not run at all.

    {1 What it does not check}

    Two things are missing on purpose rather than by oversight, and both for the
    same reason: the geometry does not say which walls are a room's boundary and
    which are furniture.

    A {b closed boundary} cannot be checked, because a pillar, a partition or a
    bench you see over all have ends that meet nothing, and so does a wall left
    out by mistake. What {e is} checked is the case that is unambiguous: a
    doorway whose ends meet no wall, which is a gap beside the opening rather
    than an opening in a wall.

    A {b spawn inside its room} cannot be checked either — an even-odd crossing
    test counts a free-standing partition as a boundary and gets the answer
    backwards. What is checked is that the spawn is not inside a wall, which is
    {!Camlcast.Room.blocked} and is what the demo suite has always asked.

    {1 No source locations}

    A diagnostic names the component, not the line. OCaml has [__POS__], but it
    would have to be written at every call site to be useful, and capturing it
    automatically needs a ppx — which this engine's dependency list does not
    have and is not worth acquiring for this. A component's name is a name you
    can grep for, and in practice that is the same journey one step longer. *)

open Camlcast

type severity =
  | Error  (** this world is wrong, and something will be visibly broken *)
  | Warning
      (** this world will run, and is probably not what was meant. A seam in the
          floor is the case in point: it draws, it is walkable, and it is a step
          the player falls down. *)

type t = {
  severity : severity;
  where : string;
      (** the component path for a description, the room's name for a bare world
      *)
  summary : string;  (** one line, the whole of the complaint *)
  detail : string list;  (** further lines, each already a sentence *)
  spot : (int * Vec.t) option;
      (** the room and the point this is about, where there is one.

          What {!Debug_map} puts a mark on. A complaint about a link or a name
          is about no particular place and leaves this empty; one about a corner
          or a spawn knows exactly where it is, and saying so is the difference
          between a description of the problem and a look at it. *)
}
(** One thing wrong. *)

val report : Parts.t -> t list
(** Everything wrong with this description, in the order it was written.

    Structural and naming mistakes are found first and stop the rest: there is
    nothing useful to say about the links of a description that has two rooms
    called [plaza], and a great deal that would be wrong.

    {b This renders the description}, because the components in it have to run
    before there is anything to read. Their effects run too. A check is a frame
    that is not drawn, and a component whose effect does something drastic will
    find that out here. *)

val world : World.t -> t list
(** Everything wrong with an assembled world.

    The part of {!report} that needs no description, so it can be pointed at a
    world built any other way — which is how the checker is tested against the
    twenty-odd worlds in [demo/] that are known to be right. *)

val to_string : t -> string
(** One diagnostic, as several lines: the place, the summary, then the detail
    indented under it. *)

val format : t list -> string
(** Every diagnostic, blank-line separated, or a single line saying there were
    none. *)
