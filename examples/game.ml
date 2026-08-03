(* A game state of the game's own devising: a phase, a clock and a player,
   run by Engine.run over a type the engine has never heard of. Space lights
   the fuse; the light goes out of the room as it burns; when it has gone the
   run ends and the window closes by itself.

   This is step 12 of doc/making-a-game.mld, compiled; demo/phases.ml is the
   dressed-up version of the same shape. *)

open Camlcast_core

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)
let height = 4.
let fuse = 12.

(* The air at a given amount of light left: the fade closes in and both
   brightnesses fall as the fuse burns down. Every number stays in its range
   the whole way, which is what lets Atmosphere.make accept them all. *)
let air ~light =
  Atmosphere.make
    ~fog_distance:(2. +. (10. *. light))
    ~min_brightness:(0.05 +. (0.2 *. light))
    ~ambient:(0.1 +. (0.5 *. light))
    ~directional:(0.1 +. (0.3 *. light))
    ()

let world =
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:(Room.floor ~plane:floor ~material:ground)
      ~ceiling:(Room.roof ~plane:(Plane.above floor height) ~material:stone)
      (Room.rectangle ~height ~material:stone (Vec.make (-6.) (-6.))
         (Vec.make 6. 6.))
  in
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:(air ~light:1.)
    ~spawn:("room", Vec.make (-4.5) 0.)

type phase = Waiting | Burning | Done
type t = { phase : phase; left : float; player : Player.t }

let start = { phase = Waiting; left = fuse; player = Player.spawn world }

let update state ~dt ~motion ~actions =
  (* Engine.step turns, pitches and walks in the right order and resolves
     collision. Nearly every game's update begins with this line. *)
  let player = Engine.step world state.player motion in
  match state.phase with
  | Waiting ->
      if Input.pressed actions (Input.Key Key.space) then
        { phase = Burning; left = fuse; player }
      else { state with player }
  | Burning ->
      let left = state.left -. dt in
      if left <= 0. then { phase = Done; left = 0.; player }
      else { state with left; player }
  | Done -> { state with player }

(* The world handed to the renderer need not be the one collision uses, and
   need not outlive the frame. Here the light fails as the fuse burns down. *)
let view state =
  let light =
    match state.phase with
    | Waiting -> 1.
    | Burning -> state.left /. fuse
    | Done -> 0.
  in
  (World.with_atmosphere world (air ~light), state.player)

let () =
  match
    Engine.with_window @@ fun window ->
    Engine.run window
      (Engine.game ~update ~view
         ~bindings:(Binding.make ~leave:[ Input.Key Key.escape ] ())
         ~finished:(fun state -> state.phase = Done)
         ())
      start
  with
  | Ok (_final, _ending) -> ()
  | Error (`Msg m) ->
      prerr_endline m;
      exit 1
