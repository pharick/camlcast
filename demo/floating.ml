(** {b Sprites off the floor, and frames chosen rather than made.} A sprite
    stands at a point on the floor plan and is a picture tall; two numbers place
    that picture.

    [base] is how far its foot floats above the floor under it. Zero rests it on
    the ground, which is every sprite in every other demo. Like a decal's [z],
    the lift is measured from the floor and not from an absolute height, so over
    the sloping floor here a cloud of dust rides up with the ground instead of
    the ground climbing through it.

    Width is not [size]. A billboard is as wide as its picture says, so the
    clouds draw at their image's shape — three times as wide as tall. The barrel
    and the figure are square pictures and unchanged by the rule; the aspect
    comes from the art rather than from a field somebody has to fill in.

    Five things shown:

    - the {b stack of dust} on the left: three clouds over the same floor spot,
      separated only by the [base] each was given;
    - the {b two barrels}: the same picture and the same [size], one on the
      ground and one lifted a cell and a half;
    - the {b partition} on the right, with a cloud behind and above it: its top
      falls between the cloud's foot and head, so it cuts off the bottom and
      leaves the rest — a sprite is depth-tested per pixel, not accepted or
      rejected whole;
    - the {b doorway} ahead, with a cloud in the room beyond, trimmed to the
      opening's own outline;
    - the {b one that moves}, drifting up the hall and back down.

    The moving one rises and changes picture every frame, and no picture is made
    while it does: {!Pictures.motes} is twelve images built once when that
    module loaded, and each frame picks one by index. Only the sprite array is
    rebuilt — {!Camlcast_core.Room.with_sprites} hands back this room with the
    walls, thresholds and both planes it already had — and
    {!Camlcast_core.World.replace_room} puts it in place. Compare {!Changing},
    which rebuilds a room from its parts every frame because everything in it is
    moving, and {!Dust}, which turns this one sprite into seventy and is where
    the cost of doing it is worth reading. *)

open Camlcast

let height = 4.5
let period = 7.
let hall_sw = Vec.make (-2.) (-6.)
let hall_se = Vec.make 13. (-6.)
let hall_ne = Vec.make 13. 6.
let hall_nw = Vec.make (-2.) 6.
let annex_sw = Vec.make 0. (-4.)
let annex_se = Vec.make 8. (-4.)
let annex_ne = Vec.make 8. 4.
let annex_nw = Vec.make 0. 4.
let width = 2.8
let hall_floor = Plane.make ~a:0.07 ~b:0. ~c:0.

let annex_floor =
  P.through
    ~from:(P.opening ~width hall_se hall_ne)
    ~into:(P.opening ~width annex_nw annex_sw)
    hall_floor

let cloud ?base ~key pos =
  P.sprite ~key ?base ~size:0.8 ~image:Pictures.motes.(0) pos

(** Everything in the hall that does not move. The drifting sprite is written
    beside these; where the old version rebuilt the sprite list to keep that
    cheap, a description says both and the reconciler works out that only one
    changed. *)
let still =
  P.
    [
      (* Stacked over the same spot, so only the base separates them. The floor
         under them climbs, and all three ride it. *)
      cloud ~key:"low" ~base:0.4 (Vec.make 6.2 (-2.2));
      cloud ~key:"middle" ~base:1.5 (Vec.make 6.2 (-2.2));
      cloud ~key:"high" ~base:2.6 (Vec.make 6.2 (-2.2));
      (* The same picture and the same size, one lifted. *)
      sprite ~key:"grounded" ~size:1. ~image:Pictures.barrel (Vec.make 5. 1.2);
      sprite ~key:"lifted" ~base:1.5 ~size:1. ~image:Pictures.barrel
        (Vec.make 6.5 1.2);
      (* A square picture, on the floor, where the clouds beside it are
         neither. *)
      sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
        (Vec.make 8.5 (-4.8));
      (* High enough that the partition in front of it hides only its foot. *)
      cloud ~key:"behind" ~base:2.2 (Vec.make 11. 4.2);
    ]

let at ~phase =
  let frames = Array.length Pictures.motes in
  (* Three times round the strip per rise, so the cloud flickers faster than it
     climbs. Both are a function of the clock and nothing else. *)
  let frame = int_of_float (phase *. float_of_int (3 * frames)) mod frames in
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("hall", Vec.make 0. 0.)
      [
        room ~name:"hall"
          ~floor:(floor ~plane:hall_floor ~material:Surfaces.ground)
          ~ceiling:
            (roof
               ~plane:(Plane.above hall_floor height)
               ~material:Surfaces.soffit)
          ([
             boundary ~closed:false ~height ~material:Surfaces.stone
               (corners [ hall_ne; hall_nw; hall_sw; hall_se ]);
             doorway ~name:"onward" ~width ~opening:3.4 ~height
               ~material:Surfaces.brick hall_se hall_ne;
             (* A partition well short of the roof, standing across the
                sightline to the cloud beyond it. *)
             wall ~height:2.2 ~material:Surfaces.panel (Vec.make 9. 1.8)
               (Vec.make 9. 5.5);
             sprite ~key:"drifting" ~size:0.8
               ~base:(0.6 +. (1.2 *. (1. -. cos (phase *. 2. *. Float.pi))))
               ~image:Pictures.motes.(frame) (Vec.make 7.5 (-0.5));
           ]
          @ still);
        room ~name:"annex"
          ~floor:(floor ~plane:annex_floor ~material:Surfaces.ground)
          ~ceiling:
            (roof
               ~plane:(Plane.above annex_floor height)
               ~material:Surfaces.soffit)
          [
            boundary ~closed:false ~height ~material:Surfaces.stone
              (corners [ annex_sw; annex_se; annex_ne; annex_nw ]);
            doorway ~name:"back" ~width ~opening:3.4 ~height
              ~material:Surfaces.brick annex_nw annex_sw;
            cloud ~key:"annex" ~base:1.9 (Vec.make 4. 0.);
          ];
        link ("hall", "onward") ("annex", "back");
      ])

let drift =
  Element.declare ~name:"drift" @@ fun () ->
  let elapsed, set_elapsed = Hook.use_state 0. in
  Events.use_frame (fun ~dt -> set_elapsed (Float.rem (elapsed +. dt) period));
  at ~phase:(elapsed /. period)

let world = (Mount.build (at ~phase:0.)).Scene.world
let run window = Run.on window ~controls:Bindings.escapable (drift ())
