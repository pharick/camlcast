(** {b Doorways.} How two rooms are joined, and what a link is.

    A hexagonal hub with doorways cut into two of its slanted sides, and a room
    beyond each. The two rooms beyond are {e the same room} — one rectangle,
    built once and used twice, standing at the origin facing north in its own
    coordinates. The doorway alone puts one to the right and one to the left,
    each turned to meet the wall it opens onto.

    A room is authored in its own frame and knows nothing of where it sits; a
    link between two thresholds is a rigid transform derived from the four
    endpoints, and there is no global frame to disagree with. This also allows
    impossible topology — join a room to one four doorways behind it and the
    corridor returns somewhere it could not physically go; nothing in the engine
    objects.

    Looking through a doorway, the renderer follows the ray into the next room,
    transformed, up to {!Camlcast_core.Config.max_portal_depth} doorways deep.
*)

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

(* A component, because the two chambers are the same room. Given a door each,
   they are two rooms of one shape — the same thing a function called twice
   would say, in a form the runtime can also tell apart. The door is what tells
   them apart now: it is the one thing about a chamber that is its own. *)
let chamber =
  Element.declare ~name:"chamber" @@ fun back ->
  P.(
    room ~height ~material:Surfaces.brick
      ~floor:(floor ~plane:flat Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.horizontal height) Surfaces.soffit)
      ~outline:(corners [ sw; se; ne; nw ])
      [
        cut back ~along:(sw, se);
        sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 0. 6.5);
      ])

let right = P.door ~width:2.6 ~clearance:3. ()
let left = P.door ~width:2.6 ~clearance:3. ()
let east_back = P.door ~width:2.6 ~clearance:3. ()
let west_back = P.door ~width:2.6 ~clearance:3. ()

let level =
  P.(
    (* Spawn is set back from the middle, so both doorways are ahead and the
       same room is visible through each. *)
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.stone
          ~floor:(floor ~plane:flat Surfaces.ground)
          ~ceiling:(roof ~plane:(Plane.horizontal height) Surfaces.soffit)
          ~outline:(corners (List.init 6 hub_corner))
          [
            spawn (Vec.make (-6.) 0.);
            (* Sides 0 and 5 are the two that meet at due east: both slanted,
               so both connections turn as well as move, and near enough each
               other to be seen at the same time. *)
            cut right ~along:(hub_corner 0, hub_corner 1);
            cut left ~along:(hub_corner 5, hub_corner 0);
          ];
        (* The same room, twice. *)
        chamber ~key:"east" east_back;
        chamber ~key:"west" west_back;
        connect right east_back;
        connect left west_back;
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
