(** One place for the state a whole game shares.

    Component state belongs to a component: a door's own open-ness, a torch's
    own flicker. Some state belongs to nobody in particular — a score, an
    inventory, which chapter the player is in, whether the game is paused — and
    threading it down as props or up as callbacks is how that ends badly. A
    store holds it in one place, changes it in one way, and lets any component
    read the part it cares about.

    The shape is a reducer, as Redux and OCaml both like it: state is immutable,
    an action describes what happened, and [reducer state action] is the state
    afterwards. Nothing else writes. That means every change a game can undergo
    is one variant of one type, which is worth as much for reading the game
    later as it is for testing it now.

    {1 Why there is no provider and no [use_dispatch]}

    react-redux passes the store through context because in JavaScript a
    module-level store is a genuine hazard — bundling and server rendering can
    give you two of them, or one shared between requests that should not share.
    In OCaml a store is a top-level value of a known type, and a component that
    wants it refers to it. So {!use_selector} takes the store as an argument and
    [use_dispatch] would be a hook that returns something the caller already
    had.

    {!Context} is still here and is still the right answer for what it is for —
    a value that legitimately differs between two subtrees, like the atmosphere
    a room is lit by. That is a different question from where the score lives.

    {1 What a subscription costs}

    {!use_selector} takes one out on mount and drops it on unmount, through
    {!Hook.use_effect}, so it is exactly as long-lived as the component. On
    dispatch, each subscriber compares the slice it last rendered against the
    slice now, and only one that differs asks for a new frame. An action that
    does not touch what a component reads costs that component one comparison
    and nothing else. *)

type ('state, 'action) t
(** A store: the state, the one function allowed to change it, and whoever is
    listening. *)

val create :
  reducer:('state -> 'action -> 'state) -> initial:'state -> ('state, 'action) t
(** [create ~reducer ~initial] is a store holding [initial].

    Made once, at the top level. A store made inside a render would be a new
    store every frame, holding [initial] forever. *)

val state : ('state, 'action) t -> 'state
(** What the store holds right now.

    For a component, prefer {!use_selector}, which also arranges to be told when
    the answer changes. This is for the outside: a test, a save file, the loop.
*)

val dispatch : ('state, 'action) t -> 'action -> unit
(** Run the action through the reducer, keep the result, and tell every
    subscriber.

    Safe from anywhere — an event handler, an effect, a timer. Not from inside a
    render: a render is meant to be a pure function of props and state, and a
    dispatch during one would make what a frame shows depend on the order its
    components happened to be walked in. *)

val subscribe : ('state, 'action) t -> (unit -> unit) -> unit -> unit
(** [subscribe store notify] calls [notify] after every {!dispatch}, and returns
    the function that stops it.

    The plumbing under {!use_selector}, exposed because something outside the
    tree may want it too — a save-on-change, a log. Whoever subscribes owns the
    unsubscribe. *)

val subscriber_count : ('state, 'action) t -> int
(** How many subscriptions are outstanding.

    Here so that "the tree does not leak subscriptions" can be a test rather
    than a hope: mount a subtree, unmount it, and this is back where it was. *)

val use_selector :
  ?equal:('a -> 'a -> bool) -> ('state, 'action) t -> ('state -> 'a) -> 'a
(** [use_selector store select] is [select (state store)], and arranges for this
    component's root to be asked for a new frame whenever that answer changes.

    Select the smallest thing that will do. [select] is what decides whether an
    action concerns this component at all, so a component reading
    [fun s -> s.score] is untouched by an action that only moves the player,
    while one reading [fun s -> s] is woken by every action there is.

    [equal] decides what changing means and defaults to structural equality,
    which raises on functional values as [( = )] always does.

    [store] is what the subscription depends on, compared by identity, so a
    component rendered against a different store drops the first one's
    subscription and takes one out on the second. Two stores are the same store
    when they are the same store; nothing here compares what they hold.

    [select] and [equal] are the ones this component last rendered with, not the
    ones it first subscribed with, so a component free to vary either between
    renders — a selector built from its props, say — is compared with the
    question it is currently asking. Only [store] decides when the subscription
    itself is retaken; the rest is read afresh on every notification.

    The subscription compares once on the way in as well as on every dispatch
    after it. A component reads the store during its render but is not listening
    until its effect runs, and between the two lies a whole flush that another
    component's effect is free to dispatch from. That dispatch is not heard by a
    subscription that does not exist yet, so the slice is read again the moment
    one does, and a frame asked for if it has moved. That first comparison
    happens before the subscription rather than after, so a [select] or [equal]
    that raises on it leaves none: the raise comes back out of the render that
    flushed it, and this component is listening to nothing rather than listening
    to something nothing can stop. *)
