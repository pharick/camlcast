(* The smallest complete game: one room, a window and a call.

   This is step 1 of doc/making-a-game.mld and the example README.md quotes —
   the three must stay in step. It compiles as part of the default build, so if
   the engine moves under it the drift fails here rather than on a reader. *)

open Camlcast

(* A pattern is a pure function from a texel coordinate to a colour. This one is
   a check; Color.level scales all three channels together, so it moves the
   brightness without touching the hue. Both coordinates and the levels are
   0 .. 255 — the range every pattern computes in. *)
let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let world =
  let height = 4. in
  (* Distances are in cells: one cell is one texture repeat, and the eye
     stands half a cell up, so a 12-cell room under a 4-cell ceiling reads as
     a hall. *)
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:(Room.floor ~plane:floor ~material:ground)
      ~ceiling:(Room.roof ~plane:(Plane.above floor height) ~material:stone)
      (* The axis-aligned box, from two opposite corners. *)
      (Room.rectangle ~height ~material:stone (Vec.make (-6.) (-6.))
         (Vec.make 6. 6.))
  in
  (* The air of an unremarkable day. Step 3 of the guide is about your own. *)
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:Atmosphere.default
    ~spawn:("room", Vec.make (-4.5) 0.)

let () =
  (* [with_window] opens the window, hands it over, and closes it again when
     this is done with it — on the way out of an error just the same. *)
  match Engine.with_window (fun window -> Engine.run_world window world) with
  (* How the run ended matters only to a program that plays a second one.
     This one has the single room above and nothing to go back to. *)
  | Ok _ending -> ()
  | Error (`Msg m) ->
      prerr_endline m;
      exit 1
