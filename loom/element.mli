(** What a component returns: a description of what should exist right now.

    An element is a value and nothing more. It holds no state, refers to nothing
    that survives the frame it was built in, and describing the same world twice
    produces two equal descriptions. Everything that persists — hook slots,
    effects, the room an assembler built last time — hangs off the instance tree
    the reconciler keeps beside these, and is reached through the {!Path.t} that
    reconciling a description against that tree works out.

    That separation is the whole trick, and it is worth saying plainly: a game
    rebuilds the entire description every frame, cheerfully and without
    bookkeeping, and the reconciler works out what actually changed.

    {1 What identifies a component}

    Two elements at the same place are {b the same component} when their
    [render] functions are physically the same closure. Nothing else would do: a
    function is the only thing a component is, so function identity is component
    identity.

    The consequence is a rule, and it is React's rule for React's reason:
    {b define components at the top level.} A component built inside another
    render — [let torch props = ...] written in the body of a room — is a fresh
    closure every frame, is therefore never the same component as last frame,
    and so is torn down and rebuilt every frame along with everything under it.
    It will look as though state does not work. {!declare} exists partly to make
    the right thing the easy thing: the closure it captures is made once, when
    the module is initialised.

    {1 Keys}

    Anything that can be rearranged should carry a [key]. Among a parent's
    children, a keyed element is matched to last frame's by its key alone and an
    unkeyed one by its position — so a list of enemies that sorts itself by
    distance keeps each enemy's state only if the enemies are keyed. See {!Path}
    for the same rule stated from the identity end.

    {b A parent's keys have to be unique}, and a repeat raises {!Duplicate_key}.
    Two children under one key are two elements with one path between them, and
    a path is how everything outside the runtime refers to a part of a
    description — what the crosshair is on, what a diagnostic names, what a
    trace reports. Which of the two an ambiguous path meant is not a question
    with an answer, so it is not one this asks.

    A {!Fragment} can be keyed like anything else. That is what a helper
    returning several primitives at once needs: there is no one primitive to put
    the key on, and without it a list of such things cannot be rearranged
    without losing what is inside them. *)

exception Duplicate_key of { at : string; key : string }
(** Two of one parent's children were given the same key. [at] is the parent's
    path and [key] is what they both said.

    Raised while reconciling, which makes it a frame the host never assembled —
    so the tree from the frame before is still standing and nothing has been
    committed. See {!Reconcile}. *)

(** A description of a subtree.

    Concrete rather than abstract, because the reconciler is a fold over these
    and there is nothing here to protect: every combination is meaningful and
    none of them can be malformed. *)
type 'prim t =
  | Empty  (** nothing at all; what a component returns to describe absence *)
  | Fragment of { key : string option; children : 'prim t list }
      (** several elements where one is expected, with no primitive of their
          own. They flatten away entirely when the frame is committed — but the
          key does not flatten away with them: it is how the fragment as a whole
          is matched against last frame's, and so how everything under it keeps
          its state through a rearrangement. *)
  | Prim of { prim : 'prim; key : string option; children : 'prim t list }
      (** one of the host's own primitives — a wall, a sprite, a run of text —
          and whatever it contains *)
  | Provide of { binding : Context.binding; children : 'prim t list }
      (** a {!Context} value in force over these children, and no primitive of
          its own. It flattens away when the frame is committed, exactly as a
          fragment does; what it leaves behind is what its children could see
          while they were being rendered. *)
  | Component : {
      render : 'props -> 'prim t;
      props : 'props;
      key : string option;
      name : string;
    }
      -> 'prim t
      (** a component and the props to call it with. ['props] is existential:
          the whole point is that a parent may hold children of many different
          prop types in one list, and only the component itself ever sees its
          own. *)

val empty : 'prim t
(** {!Empty}, named so a game need not reach for the constructor. *)

val fragment : ?key:string -> 'prim t list -> 'prim t
(** {!Fragment}. Key it if the list it sits in can be rearranged. *)

val provide : 'a Context.t -> 'a -> 'prim t list -> 'prim t
(** [provide context value children] renders [children] with [context] bound to
    [value], shadowing any binding further out.

    A component reads it back with {!Hook.use_context}. Nothing subscribes and
    nothing needs to: a description is rebuilt every frame, so a changed value
    is simply what the next render sees. *)

val prim : ?key:string -> ?children:'prim t list -> 'prim -> 'prim t
(** [prim p] is the host primitive [p], with [children] under it and nothing by
    default. *)

val component :
  ?key:string -> name:string -> ('props -> 'prim t) -> 'props -> 'prim t
(** [component ~name render props] is [render] waiting to be called on [props].

    Note it is not called here. A description of a subtree is built lazily, one
    level at a time, as the reconciler walks into it — which is what lets it
    stop walking when it reaches something that cannot have changed. *)

val declare :
  name:string -> ('props -> 'prim t) -> ?key:string -> 'props -> 'prim t
(** [declare ~name render] is the way a game should write a component:

    {[
      let torch = Element.declare ~name:"torch" @@ fun (pos : Vec.t) -> ...

      (* and then, wherever one is wanted *)
      torch here
      torch ~key:"north" there
    ]}

    What comes back is a constructor for that component, and the closure it
    captures is made once — here, at module initialisation — rather than once
    per frame. That is what makes the identity rule at the top of this page hold
    by construction instead of by remembering it. *)

val key : 'prim t -> string option
(** The key this element was given, if it was given one. {!Empty} never has one,
    having nothing to keep, and neither does {!Provide}: a context binding is
    not a thing a game rearranges, and nothing has asked to key one. *)

val name : 'prim t -> string option
(** What to call this in a diagnostic — a component's own name, and nothing for
    anything else. *)
