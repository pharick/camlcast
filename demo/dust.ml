(** {b A room full of falling dust.} {!Floating} shows one sprite off the floor;
    this is the same mechanism at game scale: a chamber of dust drifting down
    through it, every mote somewhere else than it was last frame.

    Every mote is a {!Camlcast_core.Room.type-sprite} with a [base], the same
    field {!Floating} lifts one barrel with. It falls rather than hovers because
    the base is a function of the clock; it is cheap because of what is not
    rebuilt with it.

    {1 What one frame costs}

    Seventy sprite records, one array to hold them, and the two records
    {!Camlcast_core.Room.with_sprites} and {!Camlcast_core.World.replace_room}
    put around them — about five microseconds, measured. The walls, the floor
    plane and the ceiling plane are the ones the room was built with, shared and
    not copied, so a room's geometry is never touched by anything moving through
    it, at any count. Drawing the dust costs a few hundred times more than
    moving it: at a 640 x 400 buffer this room takes about 8.7 ms to draw empty
    and 10.1 ms full.

    No image is made per frame. {!Pictures.motes} is twelve images, built once
    when that module loaded; a mote animates by reading a different index out of
    that array. Per-frame image generation is the one cost the engine is built
    to make unnecessary, and it would show immediately at this count: seventy
    pictures a frame at sixty frames a second is four thousand images a second,
    more than anything would keep up with. [test_demos] asserts the pictures
    come out of the strip by {e physical} equality, so a version that built an
    equal one per frame would fail it.

    {1 Where the motes are}

    Each mote's place, size, fall speed and lateral sway come from its index and
    nothing else, so the same dust comes back on every run; the only thing
    carried between frames is the clock — one {!Camlcast.Hook.use_state}, and
    every mote a function of it. The scattering constants are irrational and
    pairwise unrelated for the reason {!Pictures.mote} spells out: two that add
    to one put every mote on a diagonal.

    A mote that reaches the floor reappears at the ceiling, which makes the fall
    endless without counting cycles. The lower a mote is the more it drifts
    sideways, so the dust settles rather than dropping in lines. Sprites are not
    solid: nothing here stops a step. *)

open Camlcast

(* A close chamber rather than a hall: {!Surfaces.air} fades everything out by
   twelve cells, so a room whose far corner is further than that would put half
   the motes in the haze. *)
let half = 5.5
let height = 4.
let count = 70
let period = 9.

(** The [k]th mote's fixed properties: where on the floor plan it falls, its
    size, its cycle rate, how far it sways, and how far into all of those it
    starts. *)
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
    to, and which frame of the strip it shows.

    The fall is [1] at the ceiling and [0] at the floor, so its height is the
    fraction times the room. Sideways it swings by [sway], and by more the
    nearer the ground it gets.

    Keyed by its number, so the reconciler knows which mote is which however the
    seventy are ordered. *)
let mote ~t k =
  let frames = Array.length Pictures.motes in
  let fall = 1. -. Float.rem ((t *. rate k /. period) +. offset k) 1. in
  let turn = ((t /. period) +. offset k) *. 2. *. Float.pi in
  let base = spot k in
  let drift = sway k *. (1.3 -. fall) *. sin (turn *. 1.7) in
  P.sprite ~key:(string_of_int k)
    ~base:(fall *. (height -. 0.2))
    ~size:(size k)
    ~image:Pictures.motes.((k + int_of_float (t *. 9. *. rate k)) mod frames)
    (Vec.make (base.Vec.x +. drift) (base.Vec.y +. (drift *. 0.6)))

let flat = Plane.horizontal 0.
let sw = Vec.make (-.half) (-.half)
let se = Vec.make half (-.half)
let ne = Vec.make half half
let nw = Vec.make (-.half) half

let at ~t =
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.stone
          ~floor:(floor ~plane:flat Surfaces.ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) Surfaces.soffit)
          ~outline:
            [
              corner sw;
              corner se ~material:Surfaces.brick;
              corner ne;
              corner nw ~material:Surfaces.brick;
            ]
          (spawn (Vec.make (-5.) (-5.)) :: List.init count (mote ~t));
      ])

let falling =
  Element.declare ~name:"falling" @@ fun () ->
  let elapsed, set_elapsed = Hook.use_state 0. in
  (* Wrapped so the clock never grows without bound, and at a multiple of the
     period so nothing jumps when it does. *)
  Events.use_frame (fun ~dt ->
      set_elapsed (Float.rem (elapsed +. dt) (period *. 12.)));
  at ~t:elapsed

let world = (Mount.build (at ~t:0.)).Scene.world
let run window = Run.on window ~controls:Bindings.escapable (falling ())
