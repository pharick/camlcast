(** {b The open sky.} A room with no roof shows a {!Camlcast.Sky} instead: a
    gradient from a horizon colour to a zenith colour, with a sun somewhere in
    it.

    The sky belongs to the room and not to the world, so two rooms can be under
    different ones. Walk through the doorway ahead and the light overhead
    changes while everything under it stays as it was — the sun swings round
    behind you and drops to the horizon, and the gradient tightens into it.

    Nothing about the sky lights the walls, though. What lights those is the
    world's {!Camlcast.Atmosphere}, which is one per world and is the subject of
    the {!Haze} demo instead. That is why the ground in the second yard is as
    bright as in the first, however low its sun has got. *)

open Camlcast

(* Low walls and a wide yard, so that most of what you can see is sky. *)
let height = 2.4

(** Noon: a pale horizon deepening to a blue zenith, the sun high in the west.
    This is {!Surfaces.day}, the sky the showcase level stands under. *)
let noon = Surfaces.day

(** Dusk: the same sky wound on a few hours. A warmer, tighter gradient, and a
    bigger, redder sun sitting on the horizon behind you. *)
let dusk =
  Sky.make ~horizon:(Color.rgb 236 152 96) ~zenith:(Color.rgb 28 30 78)
    ~sun:(Color.rgb 255 214 150) ~sun_azimuth:2.7 ~sun_height:0.06
    ~sun_radius:1.2 ~gradient:4.5 ()

(* A walled yard, open overhead, with a doorway in the wall you face on the way
   in. Something tall stands in it to catch the light against the sky. *)
let yard ~sky ~column =
  let sw = Vec.make 0. (-8.)
  and se = Vec.make 17. (-8.)
  and ne = Vec.make 17. 8.
  and nw = Vec.make 0. 8. in
  let jambs, threshold =
    Room.doorway ~name:"door" ~width:2.8 ~opening:2.4 ~height
      ~material:Surfaces.stone se ne
  in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ threshold ]
    ~floor:(Room.floor ~plane:floor ~material:Surfaces.ground)
    ~ceiling:(Room.open_sky sky)
    (jambs @ [ wall sw se; wall ne nw; wall nw sw ] @ column)

let column ~at =
  Room.regular_polygon ~center:at ~radius:0.9 ~sides:6 ~rotation:0. ~height:7.
    ~material:Surfaces.brick

let world =
  World.make
    ~rooms:
      [
        ("noon", yard ~sky:noon ~column:(column ~at:(Vec.make 9. 4.)));
        ("dusk", yard ~sky:dusk ~column:(column ~at:(Vec.make 9. (-4.))));
      ]
    ~links:[ (("noon", "door"), ("dusk", "door")) ]
    ~atmosphere:Surfaces.air
    ~spawn:("noon", Vec.make 3. 0.)

let run window = Engine.run_world window world
