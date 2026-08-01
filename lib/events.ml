(* Implementation of {!Camlcast_stage.Events}; the interface carries the prose. *)

open Camlcast_core
module Loom = Camlcast_loom

type crossing = {
  from_room : string;
  from_doorway : string;
  to_room : string;
  to_doorway : string;
}

type t = {
  dt : float;
  motion : Input.motion;
  actions : Input.actions;
  crossings : crossing list;
  viewport : int * int;
}

let still =
  {
    dt = 0.;
    motion = Input.still;
    actions = Input.untouched;
    crossings = [];
    (* The engine's own initial window, put through the same rule the loop
       would, so a description rendered outside a run measures itself against
       something real rather than against nothing. *)
    viewport =
      Renderer.internal_size ~width:Config.initial_width
        ~height:Config.initial_height;
  }

let context = Loom.Context.make still
let use () = Loom.Hook.use_context context
let use_dt () = (use ()).dt
let use_actions () = (use ()).actions
let use_viewport () = (use ()).viewport

(* Deps that are never equal, so the effect is torn down and set up again every
   frame. That is the plain way to say "after each frame" in terms of the one
   hook that runs after a frame at all, and it costs one comparison that always
   answers the same way. *)
let after_every_frame work =
  Loom.Hook.use_effect ~equal:(fun _ _ -> false) ~deps:() work

let use_frame handler =
  let dt = use_dt () in
  after_every_frame (fun () ->
      handler ~dt;
      None)

let use_key_down key handler =
  let went_down = Input.pressed (use_actions ()) (Input.Key key) in
  after_every_frame (fun () ->
      if went_down then handler ();
      None)

let use_key_held key = Input.down (use_actions ()) (Input.Key key)
let use_crossings () = (use ()).crossings

let use_crossed handler =
  let crossings = use_crossings () in
  after_every_frame (fun () ->
      List.iter handler crossings;
      None)
