(* Implementation of {!Camlcast_stage.Run}; the interface carries the prose. *)

open Camlcast

(* The scene is kept beside the player because the loop asks for the next state
   before it asks what to draw, and moving the player needs a world to be
   refused by. So a frame renders the description in [update] and [view] only
   hands over what it found — which also means the world collision is resolved
   against is the world the frame is drawn from, rather than the one before it. *)
type frame = { player : Player.t; scene : Scene.t }

let play ?title ?width ?height
    ?(bindings = Binding.make ~leave:[ Input.Key Key.escape ] ()) description =
  Engine.with_window ?title ?width ?height @@ fun window ->
  let mount = Mount.create () in
  let render () = Mount.render mount description in
  let first = render () in
  let update state ~dt:_ ~motion ~actions:_ =
    let scene = render () in
    { scene; player = Engine.step scene.Scene.world state.player motion }
  in
  let view state = (state.scene.Scene.world, state.player) in
  let start = { scene = first; player = Player.spawn first.Scene.world } in
  Result.map snd
    (Engine.run window (Engine.game ~update ~view ~bindings ()) start)
