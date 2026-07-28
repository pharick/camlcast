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
    ~directional:0.65

let world =
  (* The colonnade runs east, which is the way you are facing when you arrive. *)
  let sw = Vec.make 0. (-6.)
  and se = Vec.make length (-6.)
  and ne = Vec.make length 6.
  and nw = Vec.make 0. 6. in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  let colonnade =
    List.concat
      (List.init 14 (fun k ->
           let x = 6. +. (float_of_int k *. 6.) in
           List.concat_map
             (fun y ->
               Room.regular_polygon ~center:(Vec.make x y) ~radius:0.7 ~sides:6
                 ~rotation:0.2 ~height ~material:Surfaces.brick)
             [ -3.5; 3.5 ]))
  in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ([ wall sw se; wall se ne; wall ne nw; wall nw sw ] @ colonnade)
  in
  World.make
    ~rooms:[ ("colonnade", room) ]
    ~links:[] ~atmosphere:fog
    ~spawn:("colonnade", Vec.make 2. 0.)

let run () = Engine.enter world
