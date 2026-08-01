(** {b A room that changes.} A room is immutable, so a room that changes is a
    room that is described again — which is what every frame does anyway, so
    there is nothing here but a clock and some arithmetic on it:

    - the sign on the far wall slides along it and swaps between two pictures,
      which is what an animated sign is — a decal moved and a frame changed;
    - the panel it hangs on cycles through the materials;
    - the barrel drifts, so the sprites change too;
    - the floor rises and falls a little, because a floor is a plane like any
      other and nothing outside the room depends on it.

    One component holds one number — how far round the cycle it is — and
    everything above is a function of that. Nothing is stored, nothing is
    replaced, and nothing has to be told what changed.

    Rebuilding a whole room every frame is what this costs, and it is what the
    layer costs everywhere: see [bench/frame.exe] for what a frame of it comes
    to. It is cheap here — one room, six walls — and cheap because a room is
    reassembled from a description rather than diffed against a picture. *)

open Camlcast

let height = 4.
let period = 6.

(* The wall the sign hangs on, which is the one you are facing when you arrive.
   Its endpoints never move: only what is painted on it does. *)
let sign_wall_a = Vec.make 7. (-6.)
let sign_wall_b = Vec.make 7. 6.
let sw = Vec.make (-7.) (-6.)
let se = Vec.make 7. (-6.)
let ne = Vec.make 7. 6.
let nw = Vec.make (-7.) 6.
let coats = [| Surfaces.brick; Surfaces.panel; Surfaces.stone; Surfaces.tile |]

(** The world as it stands at [phase], a fraction of the way round the cycle.

    The old version of this demo kept one authored world for the player to walk
    in and replaced its room every frame with another for the renderer to draw.
    It does not have to any more: the walls never move, and collision is a flat
    question about wall segments that the floor plane takes no part in — so the
    world described here is the world walked in, and there is only one of it. *)
let at ~phase =
  let turn = phase *. 2. *. Float.pi in
  let coat = coats.(int_of_float (phase *. 4.) mod 4) in
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("room", Vec.make (-4.5) 0.)
      [
        room ~name:"room"
          ~floor:
            (floor
               ~plane:(Plane.horizontal (0.3 *. sin turn))
               ~material:Surfaces.ground)
          ~ceiling:
            (roof
               ~plane:(Plane.horizontal (height +. 0.5))
               ~material:Surfaces.soffit)
          [
            wall ~height ~material:Surfaces.stone sw se;
            wall ~height ~material:coat sign_wall_a sign_wall_b
              ~decals:
                [
                  (* Two pictures alternating is a two-frame animation; the
                     slide along the wall is the same decal placed somewhere
                     else. *)
                  decal
                    ~along:(6. +. (3.5 *. sin turn))
                    ~z:(1.8 +. (0.25 *. sin (turn *. 2.)))
                    ~half_width:0.9 ~half_height:0.9
                    (if Float.rem (phase *. 6.) 1. < 0.5 then Pictures.painting
                     else Pictures.poster);
                ];
            wall ~height ~material:Surfaces.stone ne nw;
            wall ~height ~material:Surfaces.stone nw sw;
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
