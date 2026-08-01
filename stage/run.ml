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
  let render () = Mount.render mount description in
  let first = render () in
  let update state ~dt:_ ~motion ~actions =
    let scene = render () in
    let map = debug && state.map <> Input.pressed actions (Input.Key Key.f3) in
    {
      scene;
      map;
      (* Only while the map is up. Walking every wall of every room is nothing
         beside drawing one, but it is also nothing anybody asked for when the
         map is down. *)
      found = (if map then Check.world scene.Scene.world else []);
      player = Engine.step scene.Scene.world state.player motion;
    }
  in
  let view state = (state.scene.Scene.world, state.player) in
  let overlay buffer state =
    if state.map then
      Debug_map.draw buffer state.scene.Scene.world state.player state.found
  in
  let start =
    {
      scene = first;
      player = Player.spawn first.Scene.world;
      map = false;
      found = [];
    }
  in
  Result.map snd
    (Engine.run window (Engine.game ~update ~view ~overlay ~bindings ()) start)
