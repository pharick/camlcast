(** Edits held against a running world, and the transform that applies them.

    {1 What this is for}

    A drag has to show before the file it changes has been rebuilt, or editing
    a level means waiting for a compiler between every movement of the mouse.
    So an edit is held here and applied to each frame on its way to being
    built, while the same edit is written to the source in parallel. Once the
    file is rebuilt the description says what the patch was saying and the
    patch is dropped.

    {1 Why it works on a committed frame}

    What a patch is handed is the forest the description came to, not the
    description. That is the whole reason it lands whatever wrote the thing: a
    wall inside a component that holds state, a wall inside one that does not,
    a wall a loop produced — all of them are nodes with a
    {!Camlcast_loom.Path.t} by the time they reach here, and a path is what an
    edit names.

    A patch is not a lie the runtime has to live with. It changes what one
    frame assembles and nothing else: no description is rewritten, no state is
    touched, and dropping every edit puts the world back exactly as the game
    describes it.

    {1 What it will not do}

    It moves things; it does not invent them. Nothing here adds or removes a
    node, because the editor's answer for a wall that does not exist yet is to
    write one into the file and rebuild — an added wall has no path until the
    description that names it has been run, and an edit with no path is an edit
    with nothing to hold on to. *)

type t
(** The edits outstanding. *)

val create : unit -> t
(** Nothing outstanding. *)

val count : t -> int
(** How many edits are being held, for a view that should say so. A number
    above zero means the screen and the source agree and the {e built} program
    does not yet. *)

val clear : t -> unit
(** Drop every edit.

    What a successful write and rebuild ends with: from that moment the
    description says it, and a patch still saying it would be saying it twice.
*)

val replace : t -> Camlcast_loom.Path.t -> Camlcast.Prim.t -> unit
(** Put this primitive at this path.

    The general form, for changing something the two below do not cover — a
    sprite's size, a decal's height up its wall, a doorway's clearance. The
    caller builds the primitive it wants, which it can: what it is changing is
    a value it can already see.

    {b There is a limit here worth knowing, and it is not this function's.} A
    number can be previewed because a number is a number. A {e name} cannot: an
    edit swapping [Surfaces.stone] for [Surfaces.brick] needs the
    {!Camlcast.Material.t} that second name stands for, and only the game has
    it — this library holds no content, and a name in someone's source is not a
    value until their program is built. So a name is written to the file and
    seen on the next build, while a number is seen in the next frame. *)

val move_wall :
  t ->
  Camlcast_loom.Path.t ->
  a:Camlcast_core.Vec.t ->
  b:Camlcast_core.Vec.t ->
  unit
(** Put the wall at this path between these two points.

    Replaces any edit already held for that path: a drag is a stream of these,
    and keeping every step of one would be keeping a history nothing reads. *)

val move_sprite : t -> Camlcast_loom.Path.t -> Camlcast_core.Vec.t -> unit
(** Put the sprite at this path here. *)

val apply : t -> Camlcast.Watch.node list -> Camlcast.Watch.node list
(** The forest with every edit in it. This is {!Camlcast.Watch.patch}.

    A node with no edit against it comes back as it was — the same value, not a
    copy — so a frame with nothing outstanding costs a walk and no allocation
    beyond it. *)
