(** A value handed down a subtree without being passed through it.

    Some things every part of a world needs and no part wants as a prop: the
    atmosphere it is lit by, the key bindings in force, the font a label is
    drawn in. Threading those through every intervening component as props is
    how a codebase acquires arguments that exist only to be passed on, and a
    context is the way out — {!Element.provide} a value over a subtree, and
    anything inside it can ask.

    {1 Type safety}

    This is where OCaml does better than the thing it is modelled on. A context
    is created with {!make}, which mints a fresh [Type.Id.t] for it, and that
    identity is what a lookup matches on — so finding a binding also proves its
    type, and the value comes back at the type the context was declared with.
    There is no cast here, sound or otherwise.

    It is worth contrasting with {!Hook}, which does need [Obj]. The difference
    is that a context is a value the game creates explicitly, so there is
    somewhere to hang a type witness; a hook has only its position in a row, and
    there is no per-call-site value to attach anything to. Where a witness can
    be had, it is had. *)

type 'a t
(** A context and the value to use where nobody has provided one.

    Made once, at the top level, exactly as a component is — it is an identity,
    and one made afresh each frame would match nothing. *)

val make : 'a -> 'a t
(** [make default] is a new context. [default] is what {!Hook.use_context}
    answers with outside any {!Element.provide} for it.

    A default that is a sensible neutral value is worth some thought: it is what
    a component gets when someone renders it on its own, in a test or a
    launcher, without the surroundings it usually has. *)

val default : 'a t -> 'a
(** What this context was made with. *)

type binding
(** A context together with a value for it — one entry in what a subtree can
    see. Existential in the value's type, so bindings for different contexts sit
    in one list. *)

val bind : 'a t -> 'a -> binding
(** [bind context value] is the entry {!Element.provide} puts in scope. *)

val find : binding list -> 'a t -> 'a option
(** [find bindings context] is the value bound nearest to hand, or [None] if
    this context is not bound at all.

    Bindings are innermost first, so the first match wins and an inner
    {!Element.provide} shadows an outer one. *)
