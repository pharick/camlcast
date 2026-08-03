(** {b Drawing over the world.} The overlay hook runs after the world has been
    rendered into the framebuffer and before the buffer reaches the screen, so
    what it draws is in front of everything and clipped by nothing.

    Three things are drawn here, all in {!Camlcast_core.Paint}: a crosshair in
    the middle, a meter along the bottom that fills over ten seconds and starts
    again, and a translucent panel behind the meter. The panel is drawn with
    {!Camlcast_core.Framebuffer.blend} rather than
    {!Camlcast_core.Framebuffer.set} — walk up to a wall and it tints the wall
    rather than replacing it.

    Note what the coordinates are. The overlay draws into the framebuffer, which
    is a whole-number fraction of the window (see
    {!Camlcast_core.Renderer.internal_size}), not into the window itself. Resize
    the window and the crosshair stays in the middle and the meter stays the
    width of the screen, because both are measured from the buffer it is handed.
*)

open Camlcast

let height = 4.
let period = 10.
let flat = Plane.horizontal 0.
let sw = Vec.make (-7.) (-7.)
let se = Vec.make 7. (-7.)
let ne = Vec.make 7. 7.
let nw = Vec.make (-7.) 7.

(** The layer, as a function of how far round the cycle it is and how big the
    buffer turned out to be.

    The old version drew this with a callback handed the framebuffer. It is part
    of the description now, and {!Camlcast.Events.use_viewport} is how it still
    knows the size — which is not the window's: the engine renders at whatever
    whole-number fraction of it stays under [max_render_height] and stretches
    the result. *)
let meter ~fraction ~viewport:(width, height) =
  let margin = width / 12 in
  let bar_h = Int.max 4 (height / 60) in
  let bar_y = height - (height / 8) in
  P.
    [
      (* A band behind the meter, blended so the world shows through it. *)
      rect ~x:0 ~y:(bar_y - bar_h) ~w:width ~h:(bar_h * 4)
        ~color:(Color.rgb 10 12 20) ~alpha:110 ();
      bar ~x:margin ~y:bar_y
        ~w:(width - (2 * margin))
        ~h:bar_h ~fraction ~color:(Color.rgb 230 190 90) ();
      crosshair ~color:(Color.rgb 245 245 245) ();
    ]

let at ~fraction ~viewport =
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("room", Vec.make (-4.5) 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          [
            boundary ~height ~material:Surfaces.brick
              (corners [ sw; se; ne; nw ]);
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 2.5 0.);
          ];
        hud (meter ~fraction ~viewport);
      ])

let filling =
  Element.declare ~name:"filling" @@ fun () ->
  let elapsed, set_elapsed = Hook.use_state 0. in
  Events.use_frame (fun ~dt -> set_elapsed (Float.rem (elapsed +. dt) period));
  at ~fraction:(elapsed /. period) ~viewport:(Events.use_viewport ())

let world =
  (Mount.build (at ~fraction:0. ~viewport:Events.still.Events.viewport))
    .Scene.world

let run window = Run.on window ~controls:Bindings.escapable (filling ())
