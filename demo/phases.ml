(** {b A game state.} {!Camlcast.Engine.run} runs a state of whatever type you
    like, and asks four things of it: what it becomes after a frame, what world
    and player to draw it from, what to put over the top, and whether it is
    over.

    The state here is a phase, a clock and a player. Nothing happens until you
    press {b space}; then the light begins to go, and when it has gone the run
    ends and the window closes by itself. Neither the phase nor the clock exists
    anywhere in the engine — [update] keeps them, [finished] reads them, and the
    engine only ever hands the value back.

    [view] is where the light goes. The world's {!Camlcast.Atmosphere} is a
    plain immutable field, so a frame can be drawn from
    [{ world with atmosphere }] without the world it was made from changing at
    all. Every frame here is drawn from a world that has never been stored
    anywhere.

    Time only passes while the window has focus. Click on another window on the
    way down and the light stops where it was. *)

open Camlcast
open Result_ext

let height = 4.
let fuse = 20.

type phase = Waiting | Burning | Done
type t = { phase : phase; left : float; player : Player.t }

let world =
  let sw = Vec.make (-8.) (-8.)
  and se = Vec.make 8. (-8.)
  and ne = Vec.make 8. 8.
  and nw = Vec.make (-8.) 8. in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:
        [
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 1.5 0.);
          Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make (-1.) (-2.5));
        ]
      (List.concat
         [
           [ wall sw se; wall se ne; wall ne nw; wall nw sw ];
           Room.regular_polygon ~center:(Vec.make 4.5 3.5) ~radius:0.8 ~sides:4
             ~rotation:0.5 ~height:2.6 ~material:Surfaces.brick;
         ])
  in
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-5.) 0.)

let start = { phase = Waiting; left = fuse; player = Player.spawn world }

(** The air at a given amount of light left, from full daylight down to a dark
    that the walls are barely picked out of. Only the two brightnesses and the
    reach of the fade move; the haze colour and the light's direction stay put,
    so what changes reads as the light going rather than as a different room. *)
let air ~light =
  Atmosphere.make ~haze:(Color.rgb 24 24 32)
    ~fog_distance:(2. +. (10. *. light))
    ~min_brightness:(0.04 +. (0.21 *. light))
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:(0.08 +. (0.52 *. light))
    ~directional:(0.08 +. (0.32 *. light))

let update state ~dt ~motion ~actions =
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

let view state =
  let light =
    match state.phase with
    | Waiting -> 1.
    | Burning -> state.left /. fuse
    | Done -> 0.
  in
  ({ world with World.atmosphere = air ~light }, state.player)

let run () =
  let+ _, ending =
    Engine.run ~bindings:Bindings.escapable ~update ~view
      ~finished:(fun state -> state.phase = Done)
      start
  in
  ending
