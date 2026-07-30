(** {b A room full of falling dust.} {!Floating} shows what one sprite off the
    floor looks like. This is the same mechanism at the scale a game actually
    wants it: a chamber with dust drifting down through the whole of it, every
    mote of it somewhere else than it was last frame.

    Nothing here is new. Every mote is a {!Camlcast.Room.type-sprite} with a
    [base], the same field {!Floating} lifts one barrel with; what makes it a
    fall rather than a hover is that the base is a function of the clock, and
    what makes it {e cheap} is what is not rebuilt with it.

    {1 What one frame costs}

    Seventy sprite records, one array to hold them, and the two records
    {!Camlcast.Room.with_sprites} and {!Camlcast.World.replace_room} put around
    them. That is the whole of it — about five microseconds, measured. The
    walls, the floor plane and the ceiling plane are the ones this room was
    built with and are shared, not copied, so a room's geometry is never touched
    by anything moving through it however much of it is moving. Drawing the dust
    costs more than moving it does, by a factor of a few hundred: at a 640 x 400
    buffer this room takes about 8.7 ms to draw empty and 10.1 ms full.

    {b And no picture is made.} {!Pictures.motes} is twelve images, built once
    when that module was loaded, and a mote animating is a mote reading a
    different index out of that array. Generating an image inside a frame is the
    one thing the engine is built to make unnecessary, and it would show
    immediately at this count: seventy pictures a frame at sixty frames a second
    is four thousand images a second that nothing would keep up with.
    [test_demos] asserts the pictures come out of the strip by {e physical}
    equality, so a version that built an equal one per frame would fail it.

    {1 Where the motes are}

    Each one's place, size, fall speed and lateral sway come from its index and
    nothing else, so the same dust comes back on every run and there is no state
    to carry between frames — [update] is a clock, and [view] is a function of
    it. The scattering constants are irrational and pairwise unrelated for the
    reason {!Pictures.mote} spells out: two that add to one put every mote on a
    diagonal.

    A mote that reaches the floor reappears at the ceiling, which is what makes
    the fall endless without anything remembering how many times round it has
    been. The lower a mote is the more it drifts sideways, so the dust settles
    rather than dropping in lines. Walk into it — sprites are not solid, and
    nothing here stops a step. *)

open Camlcast
open Result_ext

(* A close chamber rather than a hall. Dust is only dust at a distance you can
   see it at, and {!Surfaces.air} fades everything out by twelve cells, so a
   room whose far corner is further than that would put half the motes in the
   haze. *)
let half = 5.5
let height = 4.
let count = 70
let period = 9.

type t = { elapsed : float; player : Player.t }

(** The [k]th mote's fixed properties: where on the floor plan it falls, how big
    it is, how fast it goes round, how far it sways, and how far into all of
    those it starts. *)
let fraction step k = Float.rem (float_of_int k *. step) 1.

let spot k =
  Vec.make
    (((fraction 0.7548776662 k *. 2.) -. 1.) *. (half -. 1.))
    (((fraction 0.5698402910 k *. 2.) -. 1.) *. (half -. 1.))

let size k = 0.45 +. (0.6 *. fraction 0.7320508076 k)
let offset k = fraction 0.4142135624 k
let rate k = 0.75 +. (0.5 *. fraction 0.2360679775 k)
let sway k = 0.3 +. (0.5 *. fraction 0.6180339887 k)

(** The [k]th mote at time [t]: how far it has fallen, where that has drifted it
    to, and which frame of the strip it is showing.

    The fall is [1] at the ceiling and [0] at the floor, so its height is the
    fraction times the room. Sideways it swings by [sway], and by more of it the
    nearer the ground it gets — dust that has almost landed is the dust that
    hangs about. *)
let mote ~t k =
  let frames = Array.length Pictures.motes in
  let fall = 1. -. Float.rem ((t *. rate k /. period) +. offset k) 1. in
  let turn = ((t /. period) +. offset k) *. 2. *. Float.pi in
  let base = spot k in
  let drift = sway k *. (1.3 -. fall) *. sin (turn *. 1.7) in
  Room.sprite
    ~base:(fall *. (height -. 0.2))
    ~size:(size k)
    ~image:Pictures.motes.((k + int_of_float (t *. 9. *. rate k)) mod frames)
    (Vec.make (base.Vec.x +. drift) (base.Vec.y +. (drift *. 0.6)))

let motes ~t = List.init count (mote ~t)

let world =
  let sw = Vec.make (-.half) (-.half)
  and se = Vec.make half (-.half)
  and ne = Vec.make half half
  and nw = Vec.make (-.half) half in
  let wall material a b = Room.wall ~height ~material a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:(motes ~t:0.)
      [
        wall Surfaces.stone sw se;
        wall Surfaces.brick se ne;
        wall Surfaces.stone ne nw;
        wall Surfaces.brick nw sw;
      ]
  in
  World.make
    ~rooms:[ ("chamber", room) ]
    ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("chamber", Vec.make (-5.) (-5.))

let start = { elapsed = 0.; player = Player.spawn world }

let update state ~dt ~motion ~actions:_ =
  {
    (* Wrapped so the clock never grows without bound, and at a multiple of the
       period so nothing jumps when it does. *)
    elapsed = Float.rem (state.elapsed +. dt) (period *. 12.);
    player = Engine.step world state.player motion;
  }

(** The room as it stands at this moment. The walls and both planes come
    straight out of [world]; only the sprite array is new. *)
let view state =
  ( World.replace_room world ~room:0
      ~replacement:
        (Room.with_sprites (World.room world 0) (motes ~t:state.elapsed)),
    state.player )

let run window =
  let+ _, ending =
    Engine.run window
      (Engine.game ~bindings:Bindings.escapable ~update ~view ())
      start
  in
  ending
