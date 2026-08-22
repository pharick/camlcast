(** {b The open sky.} A room with no roof shows a {!Camlcast_core.Sky} instead:
    a gradient from a horizon colour to a zenith colour, with a sun in it.

    The sky belongs to the room, not the world, so two rooms can be under
    different ones: through the doorway the sky changes while everything under
    it stays the same.

    The sky does not light the walls. That is the world's
    {!Camlcast_core.Atmosphere}, one per world and the subject of the {!Haze}
    demo — which is why the ground in the second yard is as bright as in the
    first, however low its sun is. *)

open Camlcast

(* Low walls and a wide yard, so most of the view is sky. *)
let height = 2.4

(** Noon: a pale horizon deepening to a blue zenith, the sun high in the west.
    This is {!Surfaces.day}, the showcase level's sky. *)
let noon = Surfaces.day

(** Dusk: the same sky a few hours later. A warmer, tighter gradient, and a
    bigger, redder sun on the horizon behind the spawn. *)
let dusk =
  Sky.make ~horizon:(Color.rgb 236 152 96) ~zenith:(Color.rgb 28 30 78)
    ~sun:(Color.rgb 255 214 150) ~sun_azimuth:2.7 ~sun_height:0.06
    ~sun_radius:1.2 ~gradient:4.5 ()

let flat = Plane.horizontal 0.
let sw = Vec.make 0. (-8.)
let se = Vec.make 17. (-8.)
let ne = Vec.make 17. 8.
let nw = Vec.make 0. 8.

(* A walled yard, open overhead, with a doorway in the wall faced on entry and
   a tall column to catch the light against the sky.

   The outline closes over all four corners and the gate is cut out of one of
   them. Which one is named by its two ends, in either order: the outline is
   what says which side of that leg is in, so the opening takes its winding
   from the wall rather than from the order it was written in. *)
let yard ~gate ~sky ~column ~holds =
  P.(
    room ~height ~material:Surfaces.stone
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(open_sky sky)
      ~outline:(corners [ sw; se; ne; nw ])
      (cut gate ~along:(se, ne)
      :: block ~height:7. ~material:Surfaces.brick
           (polygon ~center:column ~radius:0.9 ~sides:6 ~rotation:0.)
      :: holds))

(* One gate per yard, and the connection between them is what makes the two the
   same opening. Neither yard is named, and neither names the other. *)
let noon_gate = P.door ~width:2.8 ~clearance:2.4 ()
let dusk_gate = P.door ~width:2.8 ~clearance:2.4 ()

let level =
  P.(
    world ~atmosphere:Surfaces.air
      [
        yard ~gate:noon_gate ~sky:noon ~column:(Vec.make 9. 4.)
          ~holds:[ spawn (Vec.make 3. 0.) ];
        yard ~gate:dusk_gate ~sky:dusk ~column:(Vec.make 9. (-4.)) ~holds:[];
        connect noon_gate dusk_gate;
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
