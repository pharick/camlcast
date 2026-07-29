(** {b Drawing over the world.} The overlay hook runs after the world has been
    rendered into the framebuffer and before the buffer reaches the screen, so
    what it draws is in front of everything and clipped by nothing.

    Three things are drawn here, all in {!Camlcast.Paint}: a crosshair in the
    middle, a meter along the bottom that fills over ten seconds and starts
    again, and a translucent panel behind the meter. The panel is drawn with
    {!Camlcast.Framebuffer.blend} rather than {!Camlcast.Framebuffer.set} — walk
    up to a wall and it tints the wall rather than replacing it.

    Note what the coordinates are. The overlay draws into the framebuffer, which
    is a whole-number fraction of the window (see
    {!Camlcast.Renderer.internal_size}), not into the window itself. Resize the
    window and the crosshair stays in the middle and the meter stays the width
    of the screen, because both are measured from the buffer it is handed. *)

open Camlcast
open Result_ext

let height = 4.
let period = 10.

type t = { elapsed : float; player : Player.t }

let world =
  let sw = Vec.make (-7.) (-7.)
  and se = Vec.make 7. (-7.)
  and ne = Vec.make 7. 7.
  and nw = Vec.make (-7.) 7. in
  let wall a b = Room.wall ~height ~material:Surfaces.brick a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:
        [ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2.5 0.) ]
      [ wall sw se; wall se ne; wall ne nw; wall nw sw ]
  in
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-4.5) 0.)

let start = { elapsed = 0.; player = Player.spawn world }

let update state ~dt ~motion ~actions:_ =
  {
    elapsed = Float.rem (state.elapsed +. dt) period;
    player = Engine.step world state.player motion;
  }

let overlay fb state =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let margin = width / 12 in
  let bar_h = Int.max 4 (height / 60) in
  let bar_y = height - (height / 8) in
  (* A band behind the meter, blended so the world shows through it. *)
  Paint.rect fb ~x:0 ~y:(bar_y - bar_h) ~w:width ~h:(bar_h * 4) ~r:10 ~g:12
    ~b:20 ~alpha:110;
  Paint.bar fb ~x:margin ~y:bar_y
    ~w:(width - (2 * margin))
    ~h:bar_h ~fraction:(state.elapsed /. period) ~r:230 ~g:190 ~b:90;
  Paint.crosshair fb ~r:245 ~g:245 ~b:245

let run window =
  let+ _, ending =
    Engine.run window ~bindings:Bindings.escapable ~update
      ~view:(fun state -> (world, state.player))
      ~overlay start
  in
  ending
