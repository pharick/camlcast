(** {b A game's own state.} A phase and a clock, held by one component, with
    nothing anywhere in the engine that knows either exists.

    Nothing happens until you press {b space}; then the light begins to go, and
    when it has gone the run ends and the window closes by itself. The phase is
    a {!Camlcast.Hook.use_state}, the clock is another counted down by
    {!Camlcast.Events.use_frame}, and the ending is not a callback the loop asks
    about but a {!Camlcast.P.finish} the description writes when it is over —
    said in the same place and the same way as everything else it says.

    The light is where the light goes. The world's {!Camlcast_core.Atmosphere}
    is a plain immutable field and a description is rebuilt every frame, so the
    air is a function of how much fuse is left and there is no world stored
    anywhere for it to be changed in.

    Time only passes while the window has focus. Click on another window on the
    way down and the light stops where it was. *)

open Camlcast

let height = 4.
let fuse = 20.
let flat = Plane.horizontal 0.
let sw = Vec.make (-8.) (-8.)
let se = Vec.make 8. (-8.)
let ne = Vec.make 8. 8.
let nw = Vec.make (-8.) 8.

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
    ()

type phase = Waiting | Burning | Done

let at ~light ~over =
  P.(
    world ~atmosphere:(air ~light)
      ~spawn:("room", Vec.make (-5.) 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          [
            outline ~height ~material:Surfaces.stone [ sw; se; ne; nw ];
            polygon ~center:(Vec.make 4.5 3.5) ~radius:0.8 ~sides:4
              ~rotation:0.5 ~height:2.6 ~material:Surfaces.brick;
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 1.5 0.);
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make (-1.) (-2.5));
          ];
        (* The old version said this with ~finished, a callback the engine asked
           every frame. It is one more thing the description describes now, in
           the same place and the same way as everything else it says. *)
        (if over then finish else Element.empty);
      ])

let fusing =
  Element.declare ~name:"fusing" @@ fun () ->
  let phase, set_phase = Hook.use_state Waiting in
  let left, set_left = Hook.use_state fuse in
  Events.use_pressed (Input.Key Key.space) (fun () ->
      if phase = Waiting then set_phase Burning);
  Events.use_frame (fun ~dt ->
      if phase = Burning then
        let remaining = left -. dt in
        if remaining <= 0. then begin
          set_left 0.;
          set_phase Done
        end
        else set_left remaining);
  at
    ~light:
      (match phase with Waiting -> 1. | Burning -> left /. fuse | Done -> 0.)
    ~over:(phase = Done)

let world = (Mount.build (at ~light:1. ~over:false)).Scene.world
let run window = Run.on window ~controls:Bindings.escapable (fusing ())
