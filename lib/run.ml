(* Implementation of {!Camlcast.Run}; the interface carries the prose. *)

open Camlcast_core

(* The scene is kept beside the player because the loop asks for the next state
   before it asks what to draw, and moving the player needs a world to be
   refused by. So a frame renders the description in [update] and [view] only
   hands over what it found — which also means the world collision is resolved
   against is the world the frame is drawn from, rather than the one before it.

   The map and what it has to say live here for the same reason: the loop hands
   this state to the overlay, so a frame draws the map of the world it decided
   on rather than of whatever a ref happened to be holding. *)
(* Indices out, names in: a doorway is what the description called it, not what
   assembling the description happened to number it. *)
let crossings_of (scene : Scene.t) (movement : Player.movement) =
  let world = scene.Scene.world in
  let doorway room threshold =
    (Room.threshold_at (World.room world room) threshold).Room.name
  in
  List.map
    (fun (c : Player.crossing) ->
      {
        Events.from_room = World.name world c.from_room;
        from_doorway = doorway c.from_room c.from_threshold;
        to_room = World.name world c.to_room;
        to_doorway = doorway c.to_room c.to_threshold;
      })
    movement.Player.crossings

let carry (scene : Scene.t) ~was player =
  match World.named scene.Scene.world was with
  | Some room when room = player.Player.room -> player
  | Some room ->
      (* The rooms were written in another order, or one before this was added
         or taken away. An identity transform moves the index and nothing else. *)
      Player.through Transform.identity ~room player
  | None -> Player.spawn scene.Scene.world

(* Whether the crosshair is the player's this frame. See where it is used. *)
let aiming (scene : Scene.t) =
  (not scene.Scene.pointing) && Option.is_none scene.Scene.camera

type frame = {
  player : Player.t;
  (* The room by name rather than by index, because a description is rebuilt
     every frame and an index is what assembling one happened to produce. See
     run.mli for what that buys. *)
  room : string;
  (* Last frame's, because a description is rendered before the player is moved
     through the world it describes. See Events.crossings. *)
  crossings : Events.crossing list;
  aim : Aim.spot option;
  scene : Scene.t;
  map : bool;
  found : Check.t list;
  (* What the crosshair was on last frame, by path rather than by index: a room
     is rebuilt every frame and its indices move, and being looked at has to
     survive that. This is the whole of what makes an enter and a leave possible
     rather than a poll. *)
  gazed : Camlcast_loom.Path.t option;
}

type window = Engine.window
type ending = Engine.ending = Closed | Left

let with_window = Engine.with_window

let on window ?(controls = Controls.default) description =
  let mount = Mount.create () in
  (* Everything below is inside the mount's lifetime, the first render included:
     a run that ends, and a run that never got started because the first
     description was refused, owe the same cleanups. This sits inside
     {!with_window}, so what an effect took while there was a window is given
     back while there still is one. *)
  Fun.protect ~finally:(fun () -> Mount.destroy mount) @@ fun () ->
  (* The frame is bound around the description rather than pushed into it, so a
     component reads time and input by asking rather than by having them handed
     down through every parent between it and here. *)
  let render frame =
    Mount.render mount
      (Camlcast_loom.Element.provide Events.context frame [ description ])
  in
  (* The buffer's size is only known where a frame is drawn, and a description
     is rendered before there is one. So it is remembered from the last frame;
     see Events.viewport for what that costs. *)
  let viewport = ref Events.still.Events.viewport in
  let first = render Events.still in
  let update state ~dt ~motion ~actions =
    let scene =
      render
        {
          Events.dt;
          motion;
          actions;
          crossings = state.crossings;
          aim = state.aim;
          viewport = !viewport;
        }
    in
    (* No separate "is there a map at all": a game that has stopped wanting one
       binds it to nothing, and nothing is never taken. *)
    let map = state.map <> Binding.taken controls.Controls.map actions in
    (* Controlled or not, exactly as a text input is: a description that says
       where the eye is gets it there, and one that does not is walked. The
       controls are not applied to a camera the description is placing, since a
       walk it never asked for would fight it every frame. *)
    (* Engine.move rather than Engine.step, which is the same walk with the
       crossings thrown away. A description that wants to know where it has been
       needs them, and nothing else in the frame does. *)
    let player, crossings =
      match scene.Scene.camera with
      | Some placed -> (placed, [])
      | None ->
          let movement =
            Engine.move scene.Scene.world
              (carry scene ~was:state.room state.player)
              motion
          in
          (movement.Player.player, crossings_of scene movement)
    in
    (* One cast, here, where the frame holds the world it settled on and the
       player it settled them at. Two things want it — whatever it lands on has
       to be told, and a description wants it as a value — and casting it once
       is what makes those two the same answer rather than two answers taken a
       few lines apart. *)
    let sight = Sight.look scene.Scene.world player in
    (* Whether the player is aiming this, which is what gaze and use are about.
       They are not "something is under the middle of the screen"; they are the
       player looking at a thing and working it, and there are two states where
       the middle of the screen is not that.

       Under {!P.cursor} the mouse is loose and does not turn the camera, so the
       crosshair is wherever the view was left rather than anywhere the player
       is pointing — a pause menu over a corridor, and the use key working the
       door behind it. Under a placed camera the view is the description's: a
       cutscene panning across a room would otherwise drag gaze enter and leave
       over everything it swept past, and the use key would work whatever the
       camera happened to be facing.

       Suppressed by handing on no cast rather than by skipping the call. The
       difference is the leave: whatever held the crosshair when the menu went up
       is told it has lost it, and a highlight is not left burning behind a
       description that has taken the screen. *)
    let aiming = aiming scene in
    (* Everything an interacting frame does is Aim.crosshair, so the loop keeps
       no logic of its own that could only be tested through a window. *)
    let looking =
      Aim.crosshair scene.Scene.targets
        ~sight:(if aiming then sight else None)
        ~was:state.gazed
        ~used:(aiming && Binding.taken controls.Controls.use actions)
    in
    (* And the same cast as a value, for a description that shows something
       about whatever is being looked at without the thing itself having to say
       so. *)
    let aim = Option.map Aim.spot_of sight in
    {
      scene;
      map;
      (* Only while the map is up. Walking every wall of every room is nothing
         beside drawing one, but it is also nothing anybody asked for when the
         map is down. *)
      found = (if map then Check.world scene.Scene.world else []);
      player;
      room = World.name scene.Scene.world player.Player.room;
      crossings;
      aim;
      gazed = looking;
    }
  in
  let view state = (state.scene.Scene.world, state.player) in
  let finished state = state.scene.Scene.finished in
  let pointing state = state.scene.Scene.pointing in
  let overlay buffer state =
    viewport := (buffer.Framebuffer.width, buffer.Framebuffer.height);
    Overlay.draw
      ~aim:(state.scene.Scene.world, state.player)
      buffer state.scene.Scene.hud;
    (* Over the game's own layer, because it is a thing you turn on to look
       under what is there rather than a thing the game drew. *)
    if state.map then
      Debug_map.draw buffer state.scene.Scene.world state.player state.found
  in
  let start =
    let player =
      match first.Scene.camera with
      | Some placed -> placed
      | None -> Player.spawn first.Scene.world
    in
    {
      scene = first;
      player;
      room = World.name first.Scene.world player.Player.room;
      crossings = [];
      aim = None;
      map = false;
      found = [];
      gazed = None;
    }
  in
  Result.map snd
    (Engine.run window
       (Engine.game ~update ~view ~overlay ~finished ~pointing
          ~bindings:controls.Controls.bindings ())
       start)

let play ?title ?width ?height ?controls description =
  with_window ?title ?width ?height (fun window ->
      on window ?controls description)
