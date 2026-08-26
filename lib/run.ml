(* Implementation of {!Camlcast.Run}; the interface carries the prose. *)

open Camlcast_core

(* The scene is kept beside the player because the loop asks for the next state
   before it asks what to draw, and moving the player needs a world to collide
   with. So [update] renders the description and [view] only hands over the
   result. Collision is therefore resolved against the world the frame is
   drawn from, rather than the one before it.

   The map flag and its diagnostics live here for the same reason: the loop
   hands this state to the overlay, so a frame draws the map of the world it
   decided on rather than of whatever a ref happened to be holding. *)
(* Names go out, not indices: a doorway is reported by the name the description
   gave it, not by the number assembling the description assigned it. *)
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
  (* The frame's one cast, kept whole beside the {!Aim.spot} above, which is
     the same answer with the indices dropped. A description reads the spot; the
     ring {!P.highlight} draws needs the indices, and needs them to name what
     the gaze dispatch was given rather than what a second cast would find. *)
  sight : Sight.t option;
  scene : Scene.t;
  map : bool;
  found : Check.t list;
  (* What the crosshair was on last frame, by path rather than by index: a room
     is rebuilt every frame and its indices move, and gaze state has to survive
     that. This stored path is what makes an enter and a leave possible rather
     than a poll. *)
  gazed : Camlcast_loom.Path.t option;
}

type window = Engine.window
type ending = Engine.ending = Closed | Returned

let with_window = Engine.with_window

let on window ?(controls = Controls.default) ?(watch = Watch.none) description =
  let mount = Mount.create () in
  (* Everything below is inside the mount's lifetime, the first render included:
     a run that ends, and a run that never started because the first
     description was refused, need the same cleanups. This sits inside
     {!with_window}, so resources an effect took while there was a window are
     released while there still is one. *)
  Fun.protect ~finally:(fun () -> Mount.destroy mount) @@ fun () ->
  (* The frame is provided as context around the description rather than passed
     into it, so a component reads time and input from the context instead of
     having them threaded through every parent between it and here. *)
  let render frame =
    Mount.render ?trace:watch.Watch.trace ?inspect:watch.Watch.inspect
      ?patch:watch.Watch.patch mount
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
    (* No separate map-enabled flag: a game that does not want a map binds the
       control to an empty list, and an empty binding is never taken. *)
    let map = state.map <> Binding.taken controls.Controls.map actions in
    (* The camera is controlled or uncontrolled, exactly as a text input is: a
       description that places the eye has it placed, and one that does not
       gets the walked player. The controls are not applied to a camera the
       description is placing, because a walk it never asked for would conflict
       with the placement every frame. *)
    (* Engine.move rather than Engine.step; step is the same walk with the
       crossings discarded. A description that wants to know where it has been
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
    (* One cast, made here, where the frame holds the world it settled on and
       the player it settled on. Two consumers want it: whatever it lands on
       has to be told, and a description wants it as a value. Casting it once
       makes those two the same answer rather than two answers taken a few
       lines apart. *)
    let sight = Sight.look scene.Scene.world player in
    (* Whether the player is aiming this frame, which is what gaze and use are
       about. They are not "something is under the middle of the screen"; they
       are the player looking at a thing and operating it, and there are two
       states where the middle of the screen is not that.

       Under {!P.cursor} the mouse is released and does not turn the camera, so
       the crosshair is wherever the view was left rather than anywhere the
       player is pointing. Otherwise a pause menu over a corridor would let the
       use key operate the door behind it. Under a placed camera the view is
       the description's. Otherwise a cutscene panning across a room would fire
       gaze enter and leave over everything it swept past, and the use key
       would operate whatever the camera happened to be facing.

       Suppressed by handing on no cast rather than by skipping the call,
       because only the call delivers the leave: whatever held the crosshair
       when the menu went up is told it has lost it, so a highlight is not left
       on behind a description that has taken the screen. *)
    let aiming = aiming scene in
    (* All of a frame's interaction logic is in Aim.crosshair, so the loop
       keeps no logic of its own that could only be tested through a window. *)
    let looking =
      Aim.crosshair scene.Scene.targets
        ~sight:(if aiming then sight else None)
        ~was:state.gazed
        ~used:(aiming && Binding.taken controls.Controls.use actions)
    in
    (* The same cast exposed as a value, so a description can show something
       about whatever is being looked at without the target itself having a
       handler. *)
    let aim = Option.map Aim.spot_of sight in
    {
      scene;
      map;
      (* Only while the map is up. Walking every wall of every room is cheap
         next to drawing a room, but there is no reason to do it when the map
         is down. *)
      found = (if map then Check.assembled scene.Scene.world else []);
      player;
      room = World.name scene.Scene.world player.Player.room;
      crossings;
      aim;
      sight;
      gazed = looking;
    }
  in
  let view state = (state.scene.Scene.world, state.player) in
  let finished state = state.scene.Scene.finished in
  let pointing state = state.scene.Scene.pointing in
  let overlay buffer state =
    viewport := (buffer.Framebuffer.width, buffer.Framebuffer.height);
    (* The ring is the drawn counterpart of the gaze dispatch, so it reads the
       same answer the dispatch above reads — the same value, not a second cast
       of the same question: a frame under a cursor or a placed camera
       highlights nothing, exactly as it dispatches nothing, and the door
       behind a pause menu is neither operated nor ringed. Withheld here rather
       than inside {!Overlay.draw} because Overlay.draw has no scene to
       consult. *)
    Overlay.draw
      ?aim:
        (match state.sight with
        | Some sight when aiming state.scene ->
            Some (state.scene.Scene.world, state.player, sight)
        | Some _ | None -> None)
      buffer state.scene.Scene.hud;
    (* Drawn over the game's own layer, because the map is a debug view the
       player toggles to inspect the scene rather than something the game
       drew. *)
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
      (* No cast yet, and none needed: the loop asks for the next state before
         it draws, so this one is never the one on screen. See run.mli. *)
      sight = None;
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

let play ?title ?width ?height ?controls ?watch description =
  with_window ?title ?width ?height (fun window ->
      on window ?controls ?watch description)
