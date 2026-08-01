(* Implementation of {!Camlcast_stage.Run}; the interface carries the prose. *)

open Camlcast

(* The scene is kept beside the player because the loop asks for the next state
   before it asks what to draw, and moving the player needs a world to be
   refused by. So a frame renders the description in [update] and [view] only
   hands over what it found — which also means the world collision is resolved
   against is the world the frame is drawn from, rather than the one before it.

   The map and what it has to say live here for the same reason: the loop hands
   this state to the overlay, so a frame draws the map of the world it decided
   on rather than of whatever a ref happened to be holding. *)
type frame = {
  player : Player.t;
  scene : Scene.t;
  map : bool;
  found : Check.t list;
}

let play ?title ?width ?height ?(debug = true)
    ?(bindings = Binding.make ~leave:[ Input.Key Key.escape ] ()) description =
  Engine.with_window ?title ?width ?height @@ fun window ->
  let mount = Mount.create () in
  (* The frame is bound around the description rather than pushed into it, so a
     component reads time and input by asking rather than by having them handed
     down through every parent between it and here. *)
  let render frame =
    Mount.render mount
      (Camlcast_loom.Element.provide Events.context frame [ description ])
  in
  let first = render Events.still in
  let update state ~dt ~motion ~actions =
    let scene = render { Events.dt; motion; actions } in
    let map = debug && state.map <> Input.pressed actions (Input.Key Key.f3) in
    {
      scene;
      map;
      (* Only while the map is up. Walking every wall of every room is nothing
         beside drawing one, but it is also nothing anybody asked for when the
         map is down. *)
      found = (if map then Check.world scene.Scene.world else []);
      (* Controlled or not, exactly as a text input is: a description that says
         where the eye is gets it there, and one that does not is walked. The
         controls are not applied to a camera the description is placing, since
         a walk it never asked for would fight it every frame. *)
      player =
        (match scene.Scene.camera with
        | Some placed -> placed
        | None -> Engine.step scene.Scene.world state.player motion);
    }
  in
  let view state = (state.scene.Scene.world, state.player) in
  let finished state = state.scene.Scene.finished in
  let overlay buffer state =
    if state.map then
      Debug_map.draw buffer state.scene.Scene.world state.player state.found
  in
  let start =
    {
      scene = first;
      player =
        (match first.Scene.camera with
        | Some placed -> placed
        | None -> Player.spawn first.Scene.world);
      map = false;
      found = [];
    }
  in
  Result.map snd
    (Engine.run window
       (Engine.game ~update ~view ~overlay ~finished ~bindings ())
       start)
