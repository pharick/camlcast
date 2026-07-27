(** {b Sprites off the floor, and frames chosen rather than made.} A sprite
    stands at a point on the floor plan and is a picture tall; two numbers
    decide where that picture actually goes.

    [base] is how far its foot floats above the floor under it. Zero is
    something resting on the ground, which is every sprite in every other demo.
    Anything else lifts it, and — like a decal's [z] — the lift is measured from
    the floor and not from an absolute height, so over the sloping floor here a
    cloud of dust rides up with the ground instead of the ground climbing
    through it.

    Its width is not [size]. A billboard is as wide as its picture says it is,
    so the clouds are drawn as clouds: their image is three times as wide as it
    is tall and they come out that shape. The barrel and the figure are square
    pictures and are unchanged by that rule, which is the point of taking the
    aspect from the art rather than from a field somebody has to fill in.

    Five things to look at:

    - the {b stack of dust} on the left: three clouds over the same spot on the
      floor, so the only thing that separates them is the [base] each was given;
    - the {b two barrels} side by side, the same picture and the same [size],
      one on the ground and one lifted a cell and a half;
    - the {b partition} on the right, with a cloud behind and above it: its top
      falls between the cloud's foot and its head, so it cuts the bottom off and
      leaves the rest — a sprite is depth-tested per pixel, not accepted or
      rejected whole;
    - the {b doorway} ahead, with a cloud floating in the room beyond, trimmed
      to the opening's own outline;
    - the {b one that moves}, drifting up the hall and back down.

    That last one is the other half of this. It rises and changes picture every
    frame, and no picture is made while it does: {!Pictures.motes} is twelve
    images built once when that module loaded, and each frame picks one by
    index. What is rebuilt is the sprite array alone —
    {!Raycaster.Room.with_sprites} hands back this room with the walls, the
    thresholds and both planes it already had — and
    {!Raycaster.World.replace_room} puts it in place. Compare {!Changing}, which
    rebuilds a room from its parts every frame because everything in it is
    moving, and {!Dust}, which is this one sprite turned into seventy and is
    where the cost of doing it is worth reading. *)

open Raycaster
open Result_ext

let height = 4.5
let period = 7.

type t = { elapsed : float; player : Player.t }

(* The transform the link will be given, exactly as {!Raycaster.World.make} is
   about to derive it, so the annex's floor can be carried across the doorway
   rather than restated — the seam argument is {!Slopes}'. *)
let across (a : Room.threshold) (b : Room.threshold) =
  Transform.between ~a1:a.Room.a ~a2:a.Room.b ~b1:b.Room.a ~b2:b.Room.b

let hall_sw = Vec.make (-2.) (-6.)
let hall_se = Vec.make 13. (-6.)
let hall_ne = Vec.make 13. 6.
let hall_nw = Vec.make (-2.) 6.
let annex_sw = Vec.make 0. (-4.)
let annex_se = Vec.make 8. (-4.)
let annex_ne = Vec.make 8. 4.
let annex_nw = Vec.make 0. 4.

let cloud ?base pos = Room.sprite ?base ~size:0.8 ~image:Pictures.motes.(0) pos

(** Everything in the hall that does not move. The drifting one is added to
    these, in {!view}, and this list is what makes it cheap: the sprites are the
    only part of the room that is built again. *)
let still =
  [
    (* Stacked one above another over the same spot, so what separates them is
       the base and nothing else. The floor under them is climbing, and all
       three ride it. *)
    cloud ~base:0.4 (Vec.make 6.2 (-2.2));
    cloud ~base:1.5 (Vec.make 6.2 (-2.2));
    cloud ~base:2.6 (Vec.make 6.2 (-2.2));
    (* The same picture and the same size, one lifted. *)
    Room.sprite ~size:1. ~image:Pictures.barrel (Vec.make 5. 1.2);
    Room.sprite ~base:1.5 ~size:1. ~image:Pictures.barrel (Vec.make 6.5 1.2);
    (* A square picture, on the floor, where the clouds beside it are neither. *)
    Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 8.5 (-4.8));
    (* High enough that the partition in front of it hides only its foot. *)
    cloud ~base:2.2 (Vec.make 11. 4.2);
  ]

let world =
  let hall_jambs, hall_onward =
    Room.doorway ~name:"onward" ~width:2.8 ~opening:3.4 ~height
      ~material:Surfaces.brick hall_se hall_ne
  and annex_jambs, annex_back =
    Room.doorway ~name:"back" ~width:2.8 ~opening:3.4 ~height
      ~material:Surfaces.brick annex_nw annex_sw
  in
  let hall_floor = Plane.make ~a:0.07 ~b:0. ~c:0. in
  let onward = across hall_onward annex_back in
  let annex_floor = Plane.through onward hall_floor in
  let stone a b = Room.wall ~height ~material:Surfaces.stone a b in
  let hall =
    Room.make ~thresholds:[ hall_onward ]
      ~floor:{ Room.plane = hall_floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           {
             Room.plane = Plane.above hall_floor height;
             material = Surfaces.soffit;
           })
      ~sprites:still
      (hall_jambs
      @ [
          stone hall_sw hall_se;
          stone hall_ne hall_nw;
          stone hall_nw hall_sw;
          (* A partition that stops well short of the roof, standing across the
             line between you and the cloud beyond it. *)
          Room.wall ~height:2.2 ~material:Surfaces.panel (Vec.make 9. 1.8)
            (Vec.make 9. 5.5);
        ])
  and annex =
    Room.make ~thresholds:[ annex_back ]
      ~floor:{ Room.plane = annex_floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           {
             Room.plane = Plane.above annex_floor height;
             material = Surfaces.soffit;
           })
      ~sprites:[ cloud ~base:1.9 (Vec.make 4. 0.) ]
      (annex_jambs
      @ [ stone annex_sw annex_se; stone annex_se annex_ne; stone annex_ne annex_nw ])
  in
  World.make
    ~rooms:[ ("hall", hall); ("annex", annex) ]
    ~links:[ (("hall", "onward"), ("annex", "back")) ]
    ~atmosphere:Surfaces.air
    ~spawn:("hall", Vec.make 0. 0.)

let start = { elapsed = 0.; player = Player.spawn world }

let update state ~dt ~motion ~actions:_ =
  {
    elapsed = Float.rem (state.elapsed +. dt) period;
    player = Engine.step world state.player motion;
  }

(* The player walks in [world] itself, whose sprites do not stop a step anyway,
   so only what is drawn changes. *)
let view state =
  let phase = state.elapsed /. period in
  let frames = Array.length Pictures.motes in
  (* Three times round the strip per rise, so the cloud flickers faster than it
     climbs. Both are a function of the clock and nothing else. *)
  let frame = int_of_float (phase *. float_of_int (3 * frames)) mod frames in
  let drifting =
    Room.sprite ~size:0.8
      ~base:(0.6 +. (1.2 *. (1. -. cos (phase *. 2. *. Float.pi))))
      ~image:Pictures.motes.(frame)
      (Vec.make 7.5 (-0.5))
  in
  let hall = World.room world 0 in
  ( World.replace_room world ~room:0
      ~replacement:(Room.with_sprites hall (drifting :: still)),
    state.player )

let run () =
  let+ _ = Engine.run_state ~update ~view start in
  ()
