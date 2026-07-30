(* Two rooms and a doorway — three rooms and two of them, in fact: a hexagonal
   hub whose sides 0 and 5 are cut into doorways, and one chamber description
   called twice. What puts one instance off to the right and one off to the
   left is the links and nothing else, because a room never knows where it is.

   This is step 5 of doc/making-a-game.mld, compiled; the guide quotes the
   world below and the two must stay in step. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)
let red = dressed (Color.rgb 170 84 84)
let soffit = dressed (Color.rgb 96 90 84)

(* Taller than step 1's rooms: this one is its own program. *)
let height = 5.

(* One room description, called twice below to build its two instances. *)
let chamber () =
  (* Wound as the rule at the top of the guide says; doorway endpoints run the
     same way as the boundary they are cut from. *)
  let sw = Vec.make (-3.5) 0.
  and se = Vec.make 3.5 0.
  and ne = Vec.make 3.5 9.
  and nw = Vec.make (-3.5) 9. in
  let jambs, back =
    Room.doorway ~name:"back" ~width:2.6 ~opening:3. ~height ~material:red sw se
  in
  (* Partial application is the wall-list idiom: fix what the walls share,
     write only their endpoints. *)
  let wall = Room.wall ~height ~material:red in
  Room.make ~thresholds:[ back ]
    ~floor:(Room.floor ~plane:(Plane.horizontal 0.) ~material:ground)
    ~ceiling:(Room.roof ~plane:(Plane.horizontal height) ~material:soffit)
    (jambs @ [ wall se ne; wall ne nw; wall nw sw ])

let world =
  (* The hub is a hexagon of radius 8: corner k sits k sixths of a turn
     around it, and side k runs from corner k to the next. Sides 0 and 5
     become doorways; sides 1 to 4 stay walls. *)
  let corner k =
    let angle = float_of_int k *. Float.pi /. 3. in
    Vec.make (8. *. cos angle) (8. *. sin angle)
  in
  let gate k name =
    Room.doorway ~name ~width:2.6 ~opening:3. ~height ~material:stone (corner k)
      (corner ((k + 1) mod 6))
  in
  let right_jambs, right = gate 0 "right"
  and left_jambs, left = gate 5 "left" in
  let side k =
    Room.wall ~height ~material:stone (corner k) (corner ((k + 1) mod 6))
  in
  let hub =
    Room.make ~thresholds:[ right; left ]
      ~floor:(Room.floor ~plane:(Plane.horizontal 0.) ~material:ground)
      ~ceiling:(Room.roof ~plane:(Plane.horizontal height) ~material:soffit)
      (List.concat [ right_jambs; left_jambs; List.map side [ 1; 2; 3; 4 ] ])
  in
  World.make
    ~rooms:[ ("hub", hub); ("east", chamber ()); ("west", chamber ()) ]
    ~links:
      [ (("hub", "right"), ("east", "back"));
        (("hub", "left"), ("west", "back")) ]
    ~atmosphere:Atmosphere.default
    ~spawn:("hub", Vec.make (-6.) 0.)

let () =
  match Engine.with_window (fun window -> Engine.run_world window world) with
  | Ok _ending -> ()
  | Error (`Msg m) ->
      prerr_endline m;
      exit 1
