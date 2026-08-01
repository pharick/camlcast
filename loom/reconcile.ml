(* Implementation of {!Camlcast_loom.Reconcile}; the interface carries the
   prose. *)

module Make (H : Host.HOST) = struct
  type element = H.prim Element.t

  (* The tree that survives between frames. It mirrors the description that
     built it, minus the props: nothing here needs them, because a component is
     re-rendered from the new props and the old instance contributes only its
     identity and — from the next step on — its state.

     [render_id] is that identity, and it is an [Obj.t] for a reason worth
     stating. Two components' [render] functions have different types whenever
     their props differ, so [==] cannot be applied to them directly; it would
     not typecheck. [Obj.repr] erases the type and nothing else, and physical
     equality on the result asks exactly the question meant — are these the same
     closure? — without assuming anything about what is behind the pointer.
     Nothing is ever cast back. *)
  type instance =
    | Nothing
    | Fragment of { path : Path.t; children : instance list }
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
        child : instance;
      }

  type t = { mutable tree : instance option }

  let create () = { tree = None }
  let emit trace event = match trace with None -> () | Some f -> f event

  let key_of = function
    | Primitive { key; _ } | Component { key; _ } -> key
    | Nothing | Fragment _ -> None

  let path_for ~parent ~index element =
    Path.child parent ?key:(Element.key element) ?name:(Element.name element)
      index

  (* Children first, then the node itself: the deepest thing goes first, which
     is the order a cleanup will have to run in once there are effects. *)
  let rec unmount ~trace instance =
    match instance with
    | Nothing -> ()
    | Fragment { children; _ } -> List.iter (unmount ~trace) children
    | Primitive { path; prim; children; _ } ->
        List.iter (unmount ~trace) children;
        emit trace (Trace.Unmounted (path, Trace.Primitive prim))
    | Component { path; name; child; _ } ->
        unmount ~trace child;
        emit trace (Trace.Unmounted (path, Trace.Component name))

  let unmount_opt ~trace = function
    | None -> ()
    | Some instance -> unmount ~trace instance

  let same_key a b = Option.equal String.equal a b

  let rec reconcile ~trace ~path old element =
    match (old, element) with
    | _, Element.Empty ->
        unmount_opt ~trace old;
        Nothing
    | Some (Fragment previous), Element.Fragment children ->
        Fragment
          {
            path;
            children =
              reconcile_children ~trace ~parent:path previous.children children;
          }
    | _, Element.Fragment children ->
        unmount_opt ~trace old;
        Fragment
          {
            path;
            children = reconcile_children ~trace ~parent:path [] children;
          }
    | Some (Primitive previous), Element.Prim { prim; key; children }
      when same_key previous.key key ->
        emit trace (Trace.Updated (path, Trace.Primitive prim));
        Primitive
          {
            path;
            prim;
            key;
            children =
              reconcile_children ~trace ~parent:path previous.children children;
          }
    | _, Element.Prim { prim; key; children } ->
        unmount_opt ~trace old;
        emit trace (Trace.Mounted (path, Trace.Primitive prim));
        Primitive
          {
            path;
            prim;
            key;
            children = reconcile_children ~trace ~parent:path [] children;
          }
    | Some (Component previous), Element.Component { render; props; key; name }
      when previous.render_id == Obj.repr render && same_key previous.key key ->
        emit trace (Trace.Updated (path, Trace.Component name));
        Component
          {
            path;
            render_id = previous.render_id;
            key;
            name;
            child =
              reconcile_child ~trace ~parent:path (Some previous.child)
                (render props);
          }
    | _, Element.Component { render; props; key; name } ->
        unmount_opt ~trace old;
        emit trace (Trace.Mounted (path, Trace.Component name));
        Component
          {
            path;
            render_id = Obj.repr render;
            key;
            name;
            child = reconcile_child ~trace ~parent:path None (render props);
          }

  (* A component has exactly one child, so it is the zeroth of its parent and
     needs none of the matching below. *)
  and reconcile_child ~trace ~parent old element =
    reconcile ~trace ~path:(path_for ~parent ~index:0 element) old element

  and reconcile_children ~trace ~parent olds news =
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
    let next_unkeyed = ref 0 in
    let rec take_unkeyed () =
      if !next_unkeyed >= Array.length live then None
      else
        let index = !next_unkeyed in
        incr next_unkeyed;
        match live.(index) with
        | Some instance when Option.is_none (key_of instance) ->
            live.(index) <- None;
            Some instance
        | _ -> take_unkeyed ()
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
    let matched =
      List.mapi
        (fun index element ->
          let old =
            match Element.key element with
            | Some key -> take_keyed key
            | None -> take_unkeyed ()
          in
          reconcile ~trace ~path:(path_for ~parent ~index element) old element)
        news
    in
    Array.iter (unmount_opt ~trace) live;
    matched

  (* Fragments and empties flatten away here: what a host is handed is only ever
     primitives, in the order the game wrote them, each carrying whatever
     primitives were nested inside it. *)
  let rec collect instance =
    match instance with
    | Nothing -> []
    | Fragment { children; _ } -> List.concat_map collect children
    | Component { child; _ } -> collect child
    | Primitive { path; prim; children; _ } ->
        [ { Host.path; prim; children = List.concat_map collect children } ]

  let render ?trace root element =
    let path = path_for ~parent:Path.root ~index:0 element in
    let tree = reconcile ~trace ~path root.tree element in
    root.tree <- Some tree;
    H.assemble (collect tree)
end
