(* Step 2 of doc/making-a-game.mld — "Every wall its own". The guide quotes
   only what each step adds; this file is the whole game as of this step.

   New here: a second and third material, a corner that dresses the wall
   leaving it, and a free-standing wall — a plinth low enough to see over. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)
let brick = dressed (Color.rgb 146 88 70)
let slab = dressed (Color.rgb 92 96 108)
let height = 4.
let flat = Plane.horizontal 0.
let sw = Vec.make (-6.) (-6.)
let se = Vec.make 6. (-6.)
let ne = Vec.make 6. 6.
let nw = Vec.make (-6.) 6.

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      ~spawn:("vault", Vec.make (-4.5) 0.)
      [
        room ~name:"vault"
          ~floor:(floor ~plane:flat ~material:ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
            (* A corner describes the wall leaving it, so the south wall —
               from sw to se — is brick, and the rest fall back to what the
               boundary was given. *)
            boundary ~height ~material:stone
              [ corner sw ~material:brick; corner se; corner ne; corner nw ];
            (* A free-standing wall, drawn from both sides, low enough to see
               over. For the room's edge reach for boundary; this is for what
               stands on its own. *)
            wall ~height:1.1 ~material:slab (Vec.make (-2.) (-1.6))
              (Vec.make (-2.) 1.6);
          ];
      ])

let () =
  match Run.play ~title:"The Undercroft" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
