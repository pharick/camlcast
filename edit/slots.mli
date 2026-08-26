(** What a component is holding, as lines to read.

    {!Camlcast_loom.Hook} says what can be known about a slot and what cannot: a
    row is an untyped array, so which hook made each slot and whether its value
    moved come free, and what a slot {e holds} is readable only where the
    component said how with [~show].

    This writes that down without inventing the missing half. A slot with no
    printer reads as opaque rather than as a guess, which is the same choice the
    engine makes everywhere else: it holds no content, and how to write a value
    down is content. *)

val line : Camlcast_loom.Hook.slot -> string
(** One slot on one line: which hook made it, whether it has moved since the
    last look, and what it holds.

    {v
    state   * 3
    ref       -
    memo      "memo3"
    effect    -
    v}

    The [*] is {!Camlcast_loom.Hook.slot.changed} and the [-] is a slot that
    said nothing about its value — either because no [~show] was given, or
    because it is a {!Camlcast_loom.Hook.use_memo} that has not computed. *)

val lines : Camlcast_loom.Hook.slot array -> string list
(** Every slot, in the order the component's hooks are called.

    [["(no hooks)"]] for a component that holds nothing, rather than nothing at
    all: a panel showing an empty list and a panel showing a component with no
    state look the same, and they are not the same. *)
