(** {b A room that changes.} A room is immutable, so a room that changes is a
    room described again — which every frame does anyway. This demo is a clock
    and some arithmetic on it:

    - the sign on the far wall slides along it and swaps between two pictures
      (an animated sign is a decal moved and a frame changed);
    - the panel it hangs on cycles through the materials;
    - the barrel drifts, so the sprites change too;
    - the floor rises and falls a little, because a floor is a plane like any
      other and nothing outside the room depends on it.

    One component holds one number — the cycle fraction — and everything above
    is a function of it. Nothing is stored, nothing is replaced, and nothing is
    told what changed.

    The cost is rebuilding a whole room every frame, as the layer does
    everywhere: see [bench/frame.exe] for what a frame comes to. It is cheap
    here — one room, six walls — because a room is reassembled from a
    description rather than diffed against a picture. *)

open Camlcast

let height = 4.
let period = 6.
let sw = Vec.make (-7.) (-6.)
let se = Vec.make 7. (-6.)
let ne = Vec.make 7. 6.
let nw = Vec.make (-7.) 6.
let coats = [| Surfaces.brick; Surfaces.panel; Surfaces.stone; Surfaces.tile |]

(** The world as it stands at [phase], a fraction of the way round the cycle.

    One world, described afresh each frame and both walked in and drawn. It can
    be one because the walls never move: what changes is the coat on them, and
    collision is a flat question about wall segments that the material takes no
    part in. *)
let at ~phase =
  let turn = phase *. 2. *. Float.pi in
  let coat = coats.(int_of_float (phase *. 4.) mod 4) in
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.stone
          ~floor:
            (floor ~plane:(Plane.horizontal (0.3 *. sin turn)) Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.horizontal (height +. 0.5)) Surfaces.soffit)
          ~outline:
            [
              corner sw;
              (* The wall the sign hangs on, faced at spawn. Its ends never
                 move: only what is painted on it does. *)
              corner se ~material:coat
                ~decals:
                  [
                    (* Two pictures alternating is a two-frame animation; the
                     slide along the wall is the same decal placed somewhere
                     else. *)
                    decal
                      ~along:(6. +. (3.5 *. sin turn))
                      ~z:(1.8 +. (0.25 *. sin (turn *. 2.)))
                      ~half_width:0.9 ~half_height:0.9
                      (if Float.rem (phase *. 6.) 1. < 0.5 then
                         Pictures.painting
                       else Pictures.poster);
                  ];
              corner ne;
              corner nw;
            ]
          [
            spawn (Vec.make (-4.5) 0.);
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make 2. (2. *. sin turn));
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 4. (-3.));
          ];
      ])

(** The clock, and nothing else. Everything that changes is a function of it. *)
let cycle =
  Element.declare ~name:"cycle" @@ fun () ->
  let elapsed, set_elapsed = Hook.use_state 0. in
  Events.use_frame (fun ~dt -> set_elapsed (Float.rem (elapsed +. dt) period));
  at ~phase:(elapsed /. period)

let world = (Mount.build (at ~phase:0.)).Scene.world
let run window = Run.on window ~controls:Bindings.escapable (cycle ())
