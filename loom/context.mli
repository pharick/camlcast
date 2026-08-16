(** A value handed down a subtree without being passed through it.

    Some values are needed throughout a subtree but wanted as a prop by no
    component in it:

    - the atmosphere a world is lit by;
    - the key bindings in force;
    - the font a label is drawn in.

    Threading such a value through every intervening component as a prop creates
    arguments that exist only to be passed on. A context avoids that:
    {!Element.provide} a value over a subtree, and any component inside it can
    read it.

    {1 Type safety}

    Here OCaml does better than React, which this is modelled on. {!make} mints
    a fresh [Type.Id.t] for each context, and a lookup matches on that identity.
    Finding a binding therefore also proves its type, so the value comes back at
    the type the context was declared with. No cast is involved, sound or
    otherwise.

    Contrast {!Hook}, which does need [Obj]. A context is a value the game
    creates explicitly, so a type witness can be attached to it. A hook has only
    its position in a row; there is no per-call-site value to attach a witness
    to. *)

type 'a t
(** A context and the value to use where none has been provided.

    Make one once, at the top level, exactly as a component is made. A context
    is an identity; one made afresh each frame would match nothing. *)

val make : 'a -> 'a t
(** [make default] is a new context. [default] is what {!Hook.use_context}
    answers with outside any {!Element.provide} for it.

    Choose a sensible neutral default. It is what a component gets when rendered
    on its own — in a test or a launcher — without its usual surroundings. *)

val default : 'a t -> 'a
(** What this context was made with. *)

type binding
(** A context together with a value for it: one entry in what a subtree can see.
    Existential in the value's type, so bindings for different contexts sit in
    one list. *)

val bind : 'a t -> 'a -> binding
(** [bind context value] is the entry {!Element.provide} puts in scope. *)

val find : binding list -> 'a t -> 'a option
(** [find bindings context] is the innermost value bound for [context], or
    [None] if it is not bound at all.

    Bindings are ordered innermost first, so the first match wins and an inner
    {!Element.provide} shadows an outer one. *)
