(* Implementation of {!Camlcast_loom.Hook}; the interface carries the prose. *)

exception Hook_outside_render

exception
  Hook_order_changed of { at : string; expected : string; found : string }

(* The effects are private to this module. A game performs them only through
   the use_* functions below, and only Reconcile installs a handler, so
   nothing else needs to name them. *)
type _ Effect.t +=
  | Use_state : 'a -> ('a * ('a -> unit)) Effect.t
  | Use_ref : 'a -> 'a ref Effect.t
  | Use_memo : {
      compute : unit -> 'a;
      deps : 'd;
      equal : 'd -> 'd -> bool;
    }
      -> 'a Effect.t
  | Use_effect : {
      start : unit -> (unit -> unit) option;
      deps : 'd;
      equal : 'd -> 'd -> bool;
    }
      -> unit Effect.t
  | Use_context : 'a Context.t -> 'a Effect.t
  | Use_invalidate : (unit -> unit) Effect.t

let perform request =
  try Effect.perform request
  with Effect.Unhandled _ -> raise Hook_outside_render

let use_state initial = perform (Use_state initial)
let use_ref initial = perform (Use_ref initial)

let use_memo ?(equal = ( = )) ~deps compute =
  perform (Use_memo { compute; deps; equal })

let use_effect ?(equal = ( = )) ~deps start =
  perform (Use_effect { start; deps; equal })

let use_context context = perform (Use_context context)
let use_invalidate () = perform Use_invalidate

(* Slots. A tag records which hook made a cell, which turns most broken hook
   orders into an exception instead of a bad cast. See the interface for
   exactly which case still gets through and why. *)
let tag_state = 0
let tag_ref = 1
let tag_memo = 2
let tag_effect = 3
let tag_names = [| "use_state"; "use_ref"; "use_memo"; "use_effect" |]
let tag_name tag = tag_names.(tag)

type cell = { tag : int; mutable value : Obj.t }

module Runtime = struct
  type slots = {
    mutable cells : cell array;
    mutable count : int;
    mutable cursor : int;
    (* Set once the first render has finished. Until then the row is still
       being discovered and every hook is new. After it, a hook not seen
       before means the hook-order rule was broken, not that the row is still
       filling. *)
    mutable settled : bool;
    (* Whether this component is still in the tree. Cleared when the row is
       unmounted, and read by the setters below. A game may keep a setter
       indefinitely — in a timer, a subscription, or a callback handed to
       something outside the runtime — and so may call it after the component
       that made it is gone. Such a call writes a cell nothing will read
       again, which is harmless. Requesting a frame would not be: the frame is
       unwanted, and on a destroyed root it would never be rendered, leaving
       {!Reconcile.S.dirty} answering true for good. *)
    mutable live : bool;
  }

  let slots () =
    { cells = [||]; count = 0; cursor = 0; settled = false; live = true }

  let append slots cell =
    if slots.count = Array.length slots.cells then begin
      let grown = Array.make (Int.max 4 (2 * slots.count)) cell in
      Array.blit slots.cells 0 grown 0 slots.count;
      slots.cells <- grown
    end;
    slots.cells.(slots.count) <- cell;
    slots.count <- slots.count + 1

  (* Claim the next slot. Returns the cell and whether it had to be made, since
     every hook does something different on the render it first appears in. *)
  let claim slots ~at ~tag ~initial =
    if slots.cursor < slots.count then begin
      let cell = slots.cells.(slots.cursor) in
      if cell.tag <> tag then
        raise
          (Hook_order_changed
             { at; expected = tag_name cell.tag; found = tag_name tag });
      slots.cursor <- slots.cursor + 1;
      (cell, false)
    end
    else begin
      if slots.settled then
        raise
          (Hook_order_changed { at; expected = "nothing"; found = tag_name tag });
      let cell = { tag; value = initial () } in
      append slots cell;
      slots.cursor <- slots.cursor + 1;
      (cell, true)
    end

  type pending = {
    mutable cleanups : (unit -> unit) list;
    mutable setups : (unit -> unit) list;
  }

  let pending () = { cleanups = []; setups = [] }

  (* Everything queued runs, whatever any of it raises. The queue is emptied
     before any entry is called, so a flush that stopped at the first raise
     would leave the remaining entries owed with nothing still holding them,
     and the tree that owes them has already been committed. Each raise is
     therefore caught and held, and the first is re-raised once everything has
     run. *)
  let flush pending =
    let cleanups = List.rev pending.cleanups
    and setups = List.rev pending.setups in
    pending.cleanups <- [];
    pending.setups <- [];
    let first = ref None in
    let run f =
      try f ()
      with raised ->
        (* Captured here, not at the re-raise, where the current backtrace
           would no longer belong to the held exception. *)
        let backtrace = Printexc.get_raw_backtrace () in
        if Option.is_none !first then first := Some (raised, backtrace)
    in
    List.iter run cleanups;
    List.iter run setups;
    match !first with
    | None -> ()
    | Some (raised, backtrace) -> Printexc.raise_with_backtrace raised backtrace

  let discard pending =
    pending.cleanups <- [];
    pending.setups <- []

  (* An effect cell holds its deps beside the cleanup its last run returned.
     The deps' type belongs to the component and is never read here, only
     compared by the equality that came with them. Reading the pair back with
     its first component at [Obj.t] is therefore sound whatever the deps
     really are. *)
  let on_unmount pending slots =
    (* Queued rather than done here, so liveness is part of the same
       transaction as the cleanups. A refused render throws this away with
       {!discard}, and the row, still in the tree, can still request a frame.
       A committed render runs it, and the row is gone for good.

       Queued first, so it runs before this row's own cleanups. A cleanup
       that calls its own component's setter is a departing component
       requesting a frame it will not be in. A setter from some other
       component — a parent's, handed down as a prop — is on that component's
       row and unaffected unless that component is leaving too. A single flag
       on the root could not draw that distinction. *)
    pending.cleanups <- (fun () -> slots.live <- false) :: pending.cleanups;
    (* Pushed front-first onto a list that {!flush} reverses, so they come out in
       the order the effects were declared. *)
    for index = 0 to slots.count - 1 do
      let cell = slots.cells.(index) in
      if cell.tag = tag_effect then
        let (_, cleanup) : Obj.t * (unit -> unit) option = Obj.obj cell.value in
        match cleanup with
        | Some cleanup -> pending.cleanups <- cleanup :: pending.cleanups
        | None -> ()
    done

  let run ~slots ~pending ~at ~env ~invalidate render =
    slots.cursor <- 0;
    (* Every path from a component back to the root goes through here, so this
       is the one place the row's liveness has to be checked. A setter and
       {!use_invalidate} are the same mechanism under two names: a value the
       component hands to arbitrary holders, which may keep it longer than the
       component lasts. *)
    let invalidate () = if slots.live then invalidate () in
    (* Compute the hook's answer, then hand it back into the component. A
       failure is handed back the same way, at the point the hook was called.

       A handler runs {e outside} the fiber the component is suspended in, so
       a raise from one is not a raise from the component. It comes out of
       [match_with] below, past the [try] the component wrote around its own
       call, past the wrapper {!Reconcile} puts around every render to name
       what refused, and past any {!Fun.protect} in between — whose finaliser
       never runs, because the fiber is abandoned rather than unwound. That
       would make a failing hook behave unlike every other expression in a
       render, and there is no reason for hooks to differ. So the failure is
       discontinued back into the fiber instead, with the backtrace it arrived
       with, and a hook raises where it was written.

       Only the work before the hand-back is covered. Continuing runs the rest
       of the component, and a raise from {e there} is the component's own.
       Caught here it would be pushed into a continuation already used up. *)
    let answer k work =
      match work () with
      | value -> Effect.Deep.continue k value
      | exception failed ->
          Effect.Deep.discontinue_with_backtrace k failed
            (Printexc.get_raw_backtrace ())
    in
    let result =
      Effect.Deep.match_with render ()
        {
          retc = (fun value -> value);
          exnc = raise;
          effc =
            (fun (type a) (request : a Effect.t) ->
              match request with
              | Use_state initial ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      answer k @@ fun () ->
                      let cell, _ =
                        claim slots ~at ~tag:tag_state ~initial:(fun () ->
                            Obj.repr initial)
                      in
                      let set value =
                        cell.value <- Obj.repr value;
                        invalidate ()
                      in
                      (Obj.obj cell.value, set))
              | Use_ref initial ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      answer k @@ fun () ->
                      let cell, _ =
                        claim slots ~at ~tag:tag_ref ~initial:(fun () ->
                            Obj.repr (ref initial))
                      in
                      Obj.obj cell.value)
              | Use_memo { compute; deps; equal } ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      answer k @@ fun () ->
                      (* The slot is claimed empty and filled afterwards,
                         though one step would read better, because [compute]
                         is game code and can raise. Run inside {!claim}'s
                         [initial], a raise left no slot behind, and a
                         component that caught it (which {!answer} exists to
                         allow) settled a row one slot short of its own hook
                         calls: every render after was one hook too many, and
                         a same-tagged neighbour one slot early could read a
                         value that was never at its type. Claimed first, the
                         row's shape is settled whatever [compute] does. A
                         slot whose compute raised is merely still empty and
                         is computed again on the next render, like any
                         expression that failed. *)
                      let cell, _ =
                        claim slots ~at ~tag:tag_memo ~initial:(fun () ->
                            Obj.repr None)
                      in
                      let remember () =
                        let value = compute () in
                        cell.value <- Obj.repr (Some (deps, value));
                        value
                      in
                      match Obj.obj cell.value with
                      | None -> remember ()
                      | Some (was, remembered) ->
                          if equal was deps then remembered else remember ())
              | Use_effect { start; deps; equal } ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      answer k @@ fun () ->
                      (* Queued rather than run: an effect belongs after the
                         scene is assembled, and a render must stay pure.
                         [equal] is not queued and runs here, so this branch
                         needs the same treatment as the memo above. *)
                      let schedule cell =
                        pending.setups <-
                          (fun () ->
                            (* Written before [start] runs, not only after it.
                               A setup that raises has taken nothing, and at
                               that moment the slot still holds the previous
                               run's cleanup — already called, at the top of
                               this same flush. Left there it would be handed
                               out again by the next unmount or the next
                               change of deps, and a cleanup that closes one
                               open thing would close it twice. So the slot is
                               first set to (deps, None) — attempted, nothing
                               owed — and only a setup that returns records
                               what it took. *)
                            cell.value <-
                              Obj.repr (deps, (None : (unit -> unit) option));
                            cell.value <- Obj.repr (deps, start ()))
                          :: pending.setups
                      in
                      let cell, fresh =
                        claim slots ~at ~tag:tag_effect ~initial:(fun () ->
                            Obj.repr (deps, (None : (unit -> unit) option)))
                      in
                      if fresh then schedule cell
                      else begin
                        let was, cleanup = Obj.obj cell.value in
                        if not (equal was deps) then begin
                          (match cleanup with
                          | Some cleanup ->
                              pending.cleanups <- cleanup :: pending.cleanups
                          | None -> ());
                          schedule cell
                        end
                      end)
              (* Neither of these claims a slot. Both answer the same thing every
                 render of a given component, so there is nothing to remember and
                 nothing a changed hook order could misalign. *)
              | Use_context context ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      answer k @@ fun () ->
                      match Context.find env context with
                      | Some bound -> bound
                      | None -> Context.default context)
              | Use_invalidate ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      Effect.Deep.continue k invalidate)
              | _ -> None);
        }
    in
    (* A render that stopped early left slots unclaimed. That is the same
       hook-order rule broken from the other end and just as unsafe to carry
       forward. *)
    if slots.cursor <> slots.count then
      raise
        (Hook_order_changed
           {
             at;
             expected = tag_name slots.cells.(slots.cursor).tag;
             found = "nothing";
           });
    slots.settled <- true;
    result
end
