(* Step 4 of doc/making-a-game.mld — "The air". The guide quotes only what
   each step adds; this file is the whole game as of this step.

   New here: an Atmosphere of the vault's own — how far you can see, what
   colour the distance is, and where the light comes from. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)
let brick = dressed (Color.rgb 146 88 70)
let slab = dressed (Color.rgb 92 96 108)

let air =
  Atmosphere.make ~haze:(Color.rgb 22 22 30) ~fog_distance:10.
    ~min_brightness:0.12 ~light:(Vec.make (-0.5) (-0.85)) ~ambient:0.3
    ~directional:0.7 ()

let height = 4.
let flat = Plane.horizontal 0.
let sw = Vec.make (-6.) (-6.)
let se = Vec.make 6. (-6.)
let ne = Vec.make 6. 6.
let nw = Vec.make (-6.) 6.

let pillar center =
  P.block ~height ~material:slab
    (P.polygon ~center ~radius:0.7 ~sides:6 ~rotation:0.)

let studio = Camlcast_edit.Session.create ()

let level =
  P.(
    world ~atmosphere:air
      [
        room ~height ~material:stone ~floor:(floor ~plane:flat ground)
          ~ceiling:(roof stone)
          ~outline:
            [ corner sw ~material:brick; corner se; corner ne; corner nw ]
          [
            spawn (Vec.make (-4.5) 0.);
            wall ~height:1.1 ~material:slab (Vec.make (-2.) (-1.6))
              (Vec.make (-2.) 1.6);
            pillar (Vec.make 3. 3.);
            pillar (Vec.make 3. (-3.));
            pillar (Vec.make (-3.) 3.);
            pillar (Vec.make (-3.) (-3.));
          ];
        Camlcast_edit.Session.pointer studio;
        hud [ Camlcast_edit.Session.overlay studio () ];
      ])

let () =
  match Camlcast_edit.Session.play ~title:"The Undercroft" studio level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
