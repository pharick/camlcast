(** {b Doorways.} How two rooms are joined, and what a link really is.

    A hexagonal hub with doorways cut into two of its slanted sides, and a room
    beyond each. The two rooms beyond are {e the same room} — the identical
    rectangle, built once and used twice, standing at the origin facing north in
    its own coordinates. What puts one of them off to the right and the other
    off to the left, each turned to meet the wall it opens onto, is the doorway
    and nothing else.

    That is the whole idea. A room is authored in its own frame and knows
    nothing of where it sits; a link between two thresholds is a rigid transform
    derived from the four endpoints, and there is no global frame for any of it
    to disagree with. Which is also why a world can be folded into a shape that
    could not exist — join a room to one four doorways behind it and the
    corridor returns you somewhere it could not possibly go, and nothing in the
    engine objects.

    Look through one doorway from the middle of the hub: the renderer follows
    the ray into the next room, transformed, up to
    {!Camlcast_core.Config.max_portal_depth} doorways deep. *)

open Camlcast

let height = 5.
let flat = Plane.horizontal 0.

(* The hub's corners. Sides 0 and 5 run diagonally, so the transforms through
   them are genuine rotations and not merely translations. *)
let hub_corner k =
  let angle = float_of_int k *. Float.pi /. 3. in
  Vec.make (8. *. cos angle) (8. *. sin angle)

let sw = Vec.make (-3.5) 0.
let se = Vec.make 3.5 0.
let ne = Vec.make 3.5 9.
let nw = Vec.make (-3.5) 9.

(* A component, because there are two of them and they are the same room. Given
   different names, they are two rooms of one shape — which is exactly what the
   old version said by calling a function twice, in a form the runtime can also
   tell apart. *)
let chamber =
  Element.declare ~name:"chamber" @@ fun name ->
  P.(
    room ~name
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.horizontal height) ~material:Surfaces.soffit)
      [
        boundary ~closed:false ~height ~material:Surfaces.brick
          (corners [ se; ne; nw; sw ]);
        doorway ~name:"back" ~width:2.6 ~opening:3. ~height
          ~material:Surfaces.brick sw se;
        sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 0. 6.5);
      ])

let level =
  P.(
    (* Standing back from the middle, so that both doorways are ahead of you and
       the same room can be seen through each. *)
    world ~atmosphere:Surfaces.air
      ~spawn:("hub", Vec.make (-6.) 0.)
      [
        room ~name:"hub"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.horizontal height) ~material:Surfaces.soffit)
          ((* Sides 0 and 5 are the two that meet at due east: both slanted, so
              both links turn as well as move, and near enough each other to be
              seen at the same time. *)
           doorway ~name:"right" ~width:2.6 ~opening:3. ~height
             ~material:Surfaces.stone (hub_corner 0) (hub_corner 1)
          :: doorway ~name:"left" ~width:2.6 ~opening:3. ~height
               ~material:Surfaces.stone (hub_corner 5) (hub_corner 0)
          :: List.map
               (fun k ->
                 wall ~height ~material:Surfaces.stone (hub_corner k)
                   (hub_corner ((k + 1) mod 6)))
               [ 1; 2; 3; 4 ]);
        (* The same room, twice. *)
        chamber ~key:"east" "east";
        chamber ~key:"west" "west";
        link ("hub", "right") ("east", "back");
        link ("hub", "left") ("west", "back");
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
