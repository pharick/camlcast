(* Implementation of {!Camlcast_loom.Reconcile}; the interface carries the
   prose. *)

module Make (H : Host.HOST) = struct
  type element = H.prim Element.t

  (* The tree that survives between frames. It mirrors the description that
     built it, minus the props: nothing here needs them, because a component is
     re-rendered from the new props and the old instance contributes its
     identity and its slots.

     [render_id] is that identity, and it is an [Obj.t] for a reason worth
     stating. Two components' [render] functions have different types whenever
     their props differ, so [==] cannot be applied to them directly; it would
     not typecheck. [Obj.repr] erases the type and nothing else, and physical
     equality on the result asks exactly the question meant — are these the same
     closure? — without assuming anything about what is behind the pointer.
     Nothing is ever cast back, which is what makes this a different and much
     weaker use of [Obj] than the one in {!Hook}. *)
  type instance =
    | Nothing
    | Fragment of {
        path : Path.t;
        key : string option;
        children : instance list;
      }
    | Provided of { path : Path.t; children : instance list }
    | Primitive of {
        path : Path.t;
        prim : H.prim;
        key : string option;
        children : instance list;
      }
    | Component of {
        path : Path.t;
        render_id : Obj.t;
        key : string option;
        name : string;
        slots : Hook.slots;
        child : instance;
      }

  (* What every step of a reconciliation needs to hand along: where to report
     what it did, where to queue work that has to wait for the commit, and how
     to say that a setter has been called. *)
  type context = {
    trace : (H.prim Trace.event -> unit) option;
    pending : Hook.pending;
    invalidate : unit -> unit;
  }

  type t = {
    mutable tree : instance option;
    pending : Hook.pending;
    mutable dirty : bool;
  }

  let create () = { tree = None; pending = Hook.pending (); dirty = false }
  let dirty root = root.dirty

  let emit context event =
    match context.trace with None -> () | Some f -> f event

  let key_of = function
    | Fragment { key; _ } | Primitive { key; _ } | Component { key; _ } -> key
    | Nothing | Provided _ -> None

  let path_for ~parent ~index element =
    Path.child parent ?key:(Element.key element) ?name:(Element.name element)
      index

  (* Children first, then the node itself: the deepest thing goes first, which
     is also the order its cleanups are owed in. *)
  let rec unmount ~context instance =
    match instance with
    | Nothing -> ()
    | Fragment { children; _ } | Provided { children; _ } ->
        List.iter (unmount ~context) children
    | Primitive { path; prim; children; _ } ->
        List.iter (unmount ~context) children;
        emit context (Trace.Unmounted (path, Trace.Primitive prim))
    | Component { path; name; child; slots; _ } ->
        unmount ~context child;
        Hook.on_unmount context.pending slots;
        emit context (Trace.Unmounted (path, Trace.Component name))

  let unmount_opt ~context = function
    | None -> ()
    | Some instance -> unmount ~context instance

  let same_key a b = Option.equal String.equal a b

  let rec reconcile ~context ~env ~path old element =
    match (old, element) with
    | _, Element.Empty ->
        unmount_opt ~context old;
        Nothing
    | Some (Fragment previous), Element.Fragment { key; children }
      when same_key previous.key key ->
        Fragment
          {
            path;
            key;
            children =
              reconcile_children ~context ~env ~parent:path previous.children
                children;
          }
    | _, Element.Fragment { key; children } ->
        unmount_opt ~context old;
        Fragment
          {
            path;
            key;
            children = reconcile_children ~context ~env ~parent:path [] children;
          }
    (* A binding is in force for the children only, so it is pushed on the way
       down and gone on the way back up. Nothing subscribes to a context: the
       description is rebuilt every frame, so a changed value is simply what the
       next render reads. *)
    | Some (Provided previous), Element.Provide { binding; children } ->
        Provided
          {
            path;
            children =
              reconcile_children ~context ~env:(binding :: env) ~parent:path
                previous.children children;
          }
    | _, Element.Provide { binding; children } ->
        unmount_opt ~context old;
        Provided
          {
            path;
            children =
              reconcile_children ~context ~env:(binding :: env) ~parent:path []
                children;
          }
    | Some (Primitive previous), Element.Prim { prim; key; children }
      when same_key previous.key key ->
        emit context (Trace.Updated (path, Trace.Primitive prim));
        Primitive
          {
            path;
            prim;
            key;
            children =
              reconcile_children ~context ~env ~parent:path previous.children
                children;
          }
    | _, Element.Prim { prim; key; children } ->
        unmount_opt ~context old;
        emit context (Trace.Mounted (path, Trace.Primitive prim));
        Primitive
          {
            path;
            prim;
            key;
            children = reconcile_children ~context ~env ~parent:path [] children;
          }
    (* The two component branches call [render] where they stand rather than
       through a shared helper. They cannot do otherwise: ['props] is
       existential, bound by the pattern, and a helper in this recursive group
       would be monomorphic and let it escape. *)
    | Some (Component previous), Element.Component { render; props; key; name }
      when previous.render_id == Obj.repr render && same_key previous.key key ->
        emit context (Trace.Updated (path, Trace.Component name));
        let slots = previous.slots in
        let described =
          render_with_hooks ~context ~env ~path ~slots render props
        in
        Component
          {
            path;
            render_id = previous.render_id;
            key;
            name;
            slots;
            child =
              reconcile ~context ~env
                ~path:(path_for ~parent:path ~index:0 described)
                (Some previous.child) described;
          }
    | _, Element.Component { render; props; key; name } ->
        unmount_opt ~context old;
        emit context (Trace.Mounted (path, Trace.Component name));
        let slots = Hook.slots () in
        let described =
          render_with_hooks ~context ~env ~path ~slots render props
        in
        Component
          {
            path;
            render_id = Obj.repr render;
            key;
            name;
            slots;
            child =
              reconcile ~context ~env
                ~path:(path_for ~parent:path ~index:0 described)
                None described;
          }

  (* The one place a component's own function is called, and so the one place
     the hook effects are handled. *)
  and render_with_hooks :
      'props.
      context:context ->
      env:Context.binding list ->
      path:Path.t ->
      slots:Hook.slots ->
      ('props -> element) ->
      'props ->
      element =
   fun ~context ~env ~path ~slots render props ->
    let at = Path.to_debug_string path in
    Hook.run ~slots ~pending:context.pending ~at ~env
      ~invalidate:context.invalidate (fun () ->
        (* Where a refusal picks up the name of what was refused. A primitive
           says invalid_arg when it will not take what it was handed, and it
           cannot say more than that: it is a function that knows a width and a
           wall and has never heard of the tree. This is the one place a
           component's function is called, and so the only frame in which the
           path is that component's rather than whatever is walked next — hence
           the translation happens here and not in the handler at the top of
           {!render}, which by then has lost it.

           Only Invalid_argument, and see {!Element.Render_refused}: it is the
           channel a primitive refuses through, and re-raising anything else
           would be this claiming to explain exceptions that are not its own. *)
        try render props
        with Invalid_argument message ->
          let backtrace = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Element.Render_refused { at; message })
            backtrace)

  and reconcile_children ~context ~env ~parent olds news =
    let olds = Array.of_list olds in
    (* [live] is what has not yet been claimed. Claiming empties a slot rather
       than removing it, so the leftovers at the end are still in the order they
       were declared in — which is what makes a trace of a frame reproducible.
       A Hashtbl walked at the end would not be. *)
    let live = Array.map Option.some olds in
    let keyed = Hashtbl.create (Array.length olds) in
    Array.iteri
      (fun index instance ->
        match key_of instance with
        | Some key -> Hashtbl.replace keyed key index
        | None -> ())
      olds;
    (* An unkeyed child is claimed where it stands and nowhere else, which is
       {!Path}'s rule seen from this end: a keyed step is its key and an unkeyed
       one is its index. Compacting past the keyed siblings instead — taking the
       next unkeyed one wherever it had got to — would carry state across a move
       that the path does not survive, so the reconciler and the path would
       disagree about which child this is. *)
    let take_at index =
      if index >= Array.length live then None
      else
        match live.(index) with
        | Some instance when Option.is_none (key_of instance) ->
            live.(index) <- None;
            Some instance
        | _ -> None
    in
    let take_keyed key =
      match Hashtbl.find_opt keyed key with
      | None -> None
      | Some index ->
          Hashtbl.remove keyed key;
          let claimed = live.(index) in
          live.(index) <- None;
          claimed
    in
    (* Two siblings under one key would be two elements with one path, and a
       path is what everything outside the runtime refers to a part of a
       description by. Refused rather than resolved: whichever of the two the
       matching happened to pick would be an answer nobody wrote down. *)
    let claimed_keys = Hashtbl.create 8 in
    let refuse_duplicate key =
      if Hashtbl.mem claimed_keys key then
        raise (Element.Duplicate_key { at = Path.to_debug_string parent; key });
      Hashtbl.add claimed_keys key ()
    in
    let matched =
      List.mapi
        (fun index element ->
          let old =
            match Element.key element with
            | Some key ->
                refuse_duplicate key;
                take_keyed key
            | None -> take_at index
          in
          reconcile ~context ~env
            ~path:(path_for ~parent ~index element)
            old element)
        news
    in
    Array.iter (unmount_opt ~context) live;
    matched

  (* Fragments and empties flatten away here: what a host is handed is only ever
     primitives, in the order the game wrote them, each carrying whatever
     primitives were nested inside it. *)
  let rec collect instance =
    match instance with
    | Nothing -> []
    | Fragment { children; _ } | Provided { children; _ } ->
        List.concat_map collect children
    | Component { child; _ } -> collect child
    | Primitive { path; prim; children; _ } ->
        [ { Host.path; prim; children = List.concat_map collect children } ]

  let render ?trace root element =
    let owed = root.dirty in
    (* Cleared first, so that a setter called from an effect below marks the
       tree for the frame after this one rather than being wiped by it. *)
    root.dirty <- false;
    let context =
      {
        trace;
        pending = root.pending;
        invalidate = (fun () -> root.dirty <- true);
      }
    in
    let path = path_for ~parent:Path.root ~index:0 element in
    match
      let tree = reconcile ~context ~env:[] ~path root.tree element in
      (* Assembled before anything is committed, because this is the last thing
         that can refuse the frame and a refused frame has to leave no trace. *)
      (tree, H.assemble (collect tree))
    with
    | tree, scene ->
        root.tree <- Some tree;
        (* After the scene, never during a render: this is the seam where a
           component is allowed to reach outside itself. *)
        Hook.flush root.pending;
        scene
    | exception refused ->
        (* Caught to tidy up and not to handle, so it goes back out with the
           backtrace it arrived with: this is the frame a game debugs from. *)
        let backtrace = Printexc.get_raw_backtrace () in
        (* Nothing here happened. The tree that queued this work was never
           committed, so its setups are owed to no one, and the cleanups are
           owed by components still standing in the tree that was kept. *)
        Hook.discard root.pending;
        (* And a frame that was asked for before this one is still asked for. *)
        root.dirty <- owed || root.dirty;
        (* Said, because the mounts and unmounts already reported cannot be. A
           reader left without this has a trace that does not merely stop short:
           a component this render said it unmounted is one the next render says
           it updated, and updating something that was unmounted is the one
           thing the reconciler promises never to do. Both lines are honest
           about the walk and only the second is honest about the tree, and this
           is where that parts. *)
        emit context Trace.Refused;
        Printexc.raise_with_backtrace refused backtrace

  let destroy ?trace root =
    let context =
      { trace; pending = root.pending; invalidate = (fun () -> ()) }
    in
    unmount_opt ~context root.tree;
    (* Emptied before the flush rather than after it, so that a second destroy
       has nothing to walk: {!Hook.on_unmount} reads a cleanup out of its cell
       without clearing it, and a row walked twice would owe it twice. *)
    root.tree <- None;
    Hook.flush root.pending;
    (* A frame asked for before this, by a tree that is now gone. Nothing can
       ask for one after it: the flush above put every row out of the tree, and
       a setter on a row that has left says nothing to the root — which is what
       keeps this [false] from being set back to [true] by a timer that outlived
       the mount and left {!dirty} answering yes with nothing left to render. *)
    root.dirty <- false
end
