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
    {!Raycaster.Config.max_portal_depth} doorways deep. *)

open Raycaster

let height = 5.

(* The hub's corners, counterclockwise. Sides 0 and 2 run diagonally, so the
   transforms through them are genuine rotations and not merely translations. *)
let corner k =
  let angle = float_of_int k *. Float.pi /. 3. in
  Vec.make (8. *. cos angle) (8. *. sin angle)

let chamber () =
  let sw = Vec.make (-3.5) 0.
  and se = Vec.make 3.5 0.
  and ne = Vec.make 3.5 9.
  and nw = Vec.make (-3.5) 9. in
  let jambs, threshold =
    Room.doorway ~name:"back" ~width:2.6 ~opening:3. ~height
      ~material:Surfaces.brick sw se
  in
  let wall a b = Room.wall ~height ~material:Surfaces.brick a b in
  Room.make ~thresholds:[ threshold ]
    ~floor:{ Room.plane = Plane.horizontal 0.; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.horizontal height; material = Surfaces.soffit })
    ~sprites:[ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 0. 6.5) ]
    (jambs @ [ wall se ne; wall ne nw; wall nw sw ])

let world =
  let gate k name =
    Room.doorway ~name ~width:2.6 ~opening:3. ~height ~material:Surfaces.stone
      (corner k)
      (corner ((k + 1) mod 6))
  in
  (* Sides 0 and 5 are the two that meet at due east: both slanted, so both
     links turn as well as move, and near enough each other to be seen at the
     same time. *)
  let right_jambs, right = gate 0 "right"
  and left_jambs, left = gate 5 "left" in
  let side k =
    Room.wall ~height ~material:Surfaces.stone (corner k)
      (corner ((k + 1) mod 6))
  in
  let hub =
    Room.make ~thresholds:[ right; left ]
      ~floor:{ Room.plane = Plane.horizontal 0.; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.horizontal height; material = Surfaces.soffit })
      (List.concat [ right_jambs; left_jambs; List.map side [ 1; 2; 3; 4 ] ])
  in
  World.make
    ~rooms:
      [
        ("hub", hub);
        (* The same room, twice. *)
        ("east", chamber ());
        ("west", chamber ());
      ]
    ~links:
      [
        (("hub", "right"), ("east", "back")); (("hub", "left"), ("west", "back"));
      ]
    ~atmosphere:Surfaces.air
      (* Standing back from the middle, so that both doorways are ahead of you
       and the same room can be seen through each. *)
    ~spawn:("hub", Vec.make (-6.) 0.)

let run () = Engine.run world
