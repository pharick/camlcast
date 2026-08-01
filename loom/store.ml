(* Implementation of {!Camlcast_loom.Store}; the interface carries the prose. *)

type ('state, 'action) t = {
  reducer : 'state -> 'action -> 'state;
  mutable state : 'state;
  (* Numbered so that unsubscribing removes the one subscription it was given
     and not another that happens to be an equal closure. Newest first, and
     reversed to notify, so subscribers hear in the order they arrived. *)
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
  (* Every subscriber is told, whatever the action was; deciding that it changed
     nothing they read is their own comparison to make, and only they know what
     they read. *)
  List.iter (fun (_, notify) -> notify ()) (List.rev t.listeners)

let use_selector ?(equal = ( = )) store select =
  let invalidate = Hook.use_invalidate () in
  let selected = select store.state in
  (* What this component last rendered, which is what a notification has to
     compare against — not the previous store state, which nobody kept. *)
  let rendered = Hook.use_ref selected in
  rendered := selected;
  (* The store is the dependency, so a component handed a different one lets go
     of the first's subscription and takes one out on the second. Physically
     compared: two stores are the same store when they are the same store, and
     the default equality would walk a record of closures and raise. *)
  Hook.use_effect ~deps:store ~equal:( == ) (fun () ->
      let notify () =
        if not (equal !rendered (select store.state)) then invalidate ()
      in
      let unsubscribe = subscribe store notify in
      (* Asked once more, now that there is something here to ask. Between the
         render above and this setup runs a flush: every cleanup the frame owed
         and every setup described before this one, any of which may dispatch —
         to a list this component was not yet on. So the notification that would
         have come is made here instead, against the same slice, and is silent
         when nothing moved. *)
      notify ();
      Some unsubscribe);
  selected
