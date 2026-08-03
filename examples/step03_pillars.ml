(* Step 3 of doc/making-a-game.mld — "Pillars". The guide quotes only what
   each step adds; this file is the whole game as of this step.

   New here: P.polygon. It hands back corners, not walls, so a pillar is a
   boundary like any other and the winding is still not your problem. *)

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

let pillar center =
  P.boundary ~height ~material:slab
    (P.polygon ~center ~radius:0.7 ~sides:6 ~rotation:0.)

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      ~spawn:("vault", Vec.make (-4.5) 0.)
      [
        room ~name:"vault"
          ~floor:(floor ~plane:flat ~material:ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
            boundary ~height ~material:stone
              [ corner sw ~material:brick; corner se; corner ne; corner nw ];
            wall ~height:1.1 ~material:slab (Vec.make (-2.) (-1.6))
              (Vec.make (-2.) 1.6);
            pillar (Vec.make 3. 3.);
            pillar (Vec.make 3. (-3.));
            pillar (Vec.make (-3.) 3.);
            pillar (Vec.make (-3.) (-3.));
          ];
      ])

let () =
  match Run.play ~title:"The Undercroft" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
