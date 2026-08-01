(* Implementation of {!Camlcast_loom.Hook}; the interface carries the prose. *)

exception Hook_outside_render

exception
  Hook_order_changed of { at : string; expected : string; found : string }

(* The effects are private to this module. A game performs them only through the
   use_* functions below, and only Reconcile installs a handler, so there is no
   reason for anyone else to be able to name them. *)
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

(* Slots. A tag says which hook made a cell, which is what turns most broken
   hook orders into an exception instead of a bad cast. See the interface for
   exactly which one still gets through and why. *)
let tag_state = 0
let tag_ref = 1
let tag_memo = 2
let tag_effect = 3
let tag_names = [| "use_state"; "use_ref"; "use_memo"; "use_effect" |]
let tag_name tag = tag_names.(tag)

type cell = { tag : int; mutable value : Obj.t }

type slots = {
  mutable cells : cell array;
  mutable count : int;
  mutable cursor : int;
  (* Set once the first render has finished. Until then the row is being
     discovered and every hook is new; after it, a hook that was never asked
     for before is the rule broken rather than the row still filling up. *)
  mutable settled : bool;
}

let slots () = { cells = [||]; count = 0; cursor = 0; settled = false }

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

let flush pending =
  let cleanups = List.rev pending.cleanups
  and setups = List.rev pending.setups in
  pending.cleanups <- [];
  pending.setups <- [];
  List.iter (fun f -> f ()) cleanups;
  List.iter (fun f -> f ()) setups

(* An effect cell holds its deps beside the cleanup its last run returned. The
   deps' type is the component's own business and is never read here — only
   compared, by the equality that came with them — so reading the pair back at
   [Obj.t] for the first component is sound whatever it really is. *)
let on_unmount pending slots =
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
                    let cell, _ =
                      claim slots ~at ~tag:tag_state ~initial:(fun () ->
                          Obj.repr initial)
                    in
                    let set value =
                      cell.value <- Obj.repr value;
                      invalidate ()
                    in
                    Effect.Deep.continue k (Obj.obj cell.value, set))
            | Use_ref initial ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let cell, _ =
                      claim slots ~at ~tag:tag_ref ~initial:(fun () ->
                          Obj.repr (ref initial))
                    in
                    Effect.Deep.continue k (Obj.obj cell.value))
            | Use_memo { compute; deps; equal } ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let cell, fresh =
                      claim slots ~at ~tag:tag_memo ~initial:(fun () ->
                          Obj.repr (deps, compute ()))
                    in
                    if not fresh then begin
                      let was, remembered = Obj.obj cell.value in
                      if not (equal was deps) then
                        cell.value <- Obj.repr (deps, compute ())
                      else ignore remembered
                    end;
                    let _, value = Obj.obj cell.value in
                    Effect.Deep.continue k value)
            | Use_effect { start; deps; equal } ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    (* Queued rather than run: an effect belongs after the scene
                       is assembled, and a render must stay pure. *)
                    let schedule cell =
                      pending.setups <-
                        (fun () -> cell.value <- Obj.repr (deps, start ()))
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
                    end;
                    Effect.Deep.continue k ())
            (* Neither of these claims a slot. Both answer the same thing every
               render of a given component, so there is nothing to remember and
               nothing a changed hook order could misalign. *)
            | Use_context context ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let value =
                      match Context.find env context with
                      | Some bound -> bound
                      | None -> Context.default context
                    in
                    Effect.Deep.continue k value)
            | Use_invalidate ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    Effect.Deep.continue k invalidate)
            | _ -> None);
      }
  in
  (* A render that stopped early left slots unclaimed, which is the same broken
     rule seen from the other end and just as unsafe to carry forward. *)
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
