(** {b Atmosphere.} The air a world is seen through, and the light that falls in
    it: two numbers for the fade and three for the shading.

    A long colonnade, deliberately longer than you can see the end of. The
    {!Camlcast.Atmosphere} here has a short [fog_distance], so the far pillars
    are lost in the haze colour while the near ones are not, and a low
    [min_brightness], so the fade goes nearly the whole way down. The pillars
    are hexagonal, which means each presents six faces at six angles to the
    light: [directional] is how much of the shading depends on which way a wall
    turns, [ambient] is how much does not, and [light] is where it comes from.

    Walk the length of it. The pillar that was a silhouette resolves into six
    lit faces as you reach it, and the one behind takes its place. *)

open Camlcast

let height = 6.
let length = 90.

(** Thick, cold air with the light low from one side: a short fade to a
    blue-grey, and most of the shading directional, so the six faces of a pillar
    read as six different greys. *)
let fog =
  Atmosphere.make ~haze:(Color.rgb 38 44 58) ~fog_distance:13.
    ~min_brightness:0.15 ~light:(Vec.make (-0.5) (-0.85)) ~ambient:0.35
    ~directional:0.65 ()

let flat = Plane.horizontal 0.

let level =
  P.(
    world ~atmosphere:fog
      ~spawn:("colonnade", Vec.make 2. 0.)
      [
        room ~name:"colonnade"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          (* The colonnade runs east, which is the way you are facing when you
             arrive. A plain box boundary, so four corners say all of it. *)
          (boundary ~height ~material:Surfaces.stone
             (corners
                [
                  Vec.make 0. (-6.);
                  Vec.make length (-6.);
                  Vec.make length 6.;
                  Vec.make 0. 6.;
                ])
          :: List.concat
               (List.init 14 (fun k ->
                    let x = 6. +. (float_of_int k *. 6.) in
                    List.map
                      (fun y ->
                        boundary ~height ~material:Surfaces.brick
                          (polygon ~center:(Vec.make x y) ~radius:0.7 ~sides:6
                             ~rotation:0.2))
                      [ -3.5; 3.5 ])));
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
