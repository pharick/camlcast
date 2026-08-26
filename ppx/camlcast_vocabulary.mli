(** Which of [Camlcast.P]'s constructors describe an element.

    The rewriter runs before types exist, so the only thing it can tell a wall
    from a corner by is the constructor's name. These are those names, and they
    are a library rather than a list inside the rewriter because two readers
    need them: the rewriter, to know what to wrap, and a test, to hold them
    against what [lib/p.mli] actually exports.

    A second copy is the failure worth designing against. A constructor added to
    [P] and not to these would be wrapped by nothing, carry no position, and the
    element it describes would stop being editable — with no error, no warning
    and nothing different on screen. [test/test_ppx.ml] asks that every [val] P
    exports appears in exactly one of these two lists or in its own list of the
    ones that describe no element, so a new constructor fails there until
    somebody says which it is. *)

val functions : string list
(** Those met as an application: [wall ~height ~material a b]. *)

val values : string list
(** Those met as a bare identifier, having no arguments to take: [cursor] and
    [finish]. Two nodes rather than one because an application and an identifier
    are two different things to match on, and a rewriter that looked only for
    applications would pass straight over these. *)
