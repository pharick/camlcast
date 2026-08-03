(* Step 1 of doc/making-a-game.mld — "One room". The guide quotes only what
   each step adds; this file is the whole game as of this step.

   One square vault you can walk around in. Everything the engine needs is
   here: a material to draw the walls with, a floor and a ceiling, a boundary
   through four corners, and a spawn. README.md quotes this program. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let height = 4.
let flat = Plane.horizontal 0.

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
              (corners
                 [
                   Vec.make (-6.) (-6.);
                   Vec.make 6. (-6.);
                   Vec.make 6. 6.;
                   Vec.make (-6.) 6.;
                 ]);
          ];
      ])

let () =
  match Run.play ~title:"The Undercroft" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
