(* Implementation of {!Camlcast_loom.Store}; the interface carries the prose. *)

type ('state, 'action) t = {
  reducer : 'state -> 'action -> 'state;
  mutable state : 'state;
  (* Numbered so that unsubscribing removes exactly the subscription it was
     given, not another that happens to be an equal closure. Stored newest
     first and reversed to notify, so subscribers are notified in the order
     they subscribed. *)
  mutable listeners : (int * (unit -> unit)) list;
  mutable next_id : int;
}

let create ~reducer ~initial =
  { reducer; state = initial; listeners = []; next_id = 0 }

let state t = t.state
let subscriber_count t = List.length t.listeners

let subscribe t notify =
  let id = t.next_id in
  t.next_id <- id + 1;
  t.listeners <- (id, notify) :: t.listeners;
  fun () ->
    t.listeners <- List.filter (fun (other, _) -> other <> id) t.listeners

let dispatch t action =
  t.state <- t.reducer t.state action;
  (* Every subscriber is notified, whatever the action was. Deciding that the
     action changed nothing it reads is each subscriber's own comparison,
     because only the subscriber knows what it reads. *)
  List.iter (fun (_, notify) -> notify ()) (List.rev t.listeners)

let use_selector ?(equal = ( = )) store select =
  let invalidate = Hook.use_invalidate () in
  let selected = select store.state in
  (* The slice this component last rendered, the selector that produced it,
     and the comparison it was given. The slice is kept because a notification
     has to compare against it rather than against the previous store state,
     which nobody kept. The other two are kept because a slice is only
     meaningful beside the selector it came out of. One ref holds all three so
     they cannot be from different renders: a subscription closing over the
     first render's selector would keep asking the first render's question of
     every state after it, and compare the answer against a slice that now
     belongs to some other selector. *)
  let latest = Hook.use_ref (selected, select, equal) in
  latest := (selected, select, equal);
  (* The store is the dependency: a component handed a different store drops
     the first store's subscription and takes one out on the second. Compared
     physically, because the default equality would walk a record of closures
     and raise. It is also the only dependency there can be: a selector is a
     fresh closure every render, and under [==] would resubscribe on every
     one. *)
  Hook.use_effect ~deps:store ~equal:( == ) (fun () ->
      let notify () =
        let rendered, select, equal = !latest in
        if not (equal rendered (select store.state)) then invalidate ()
      in
      (* Called once before subscribing. Between the render above and this
         setup runs a flush: every cleanup the frame owed and every setup
         declared before this one, any of which may dispatch — to a list this
         component was not yet on. The notification that would have been
         missed is therefore made here, against the same slice, and does
         nothing when nothing changed.

         Called before [subscribe] rather than after, so that a [select] or
         [equal] that raises here leaves nothing behind. A setup that raises
         is treated as having taken nothing, so the cleanup it never returned
         is never run. A subscription taken first would outlive every
         component that could drop it, and the store would keep waking a root
         that had stopped listening. *)
      notify ();
      Some (subscribe store notify));
  selected
