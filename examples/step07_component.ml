(* Step 7 of doc/making-a-game.mld — "Your first component". The guide
   quotes only what each step adds; this file is the whole game as of this
   step.

   New here: a torch component, declared once at the top level and placed
   four times. Each placement is an instance of its own — which matters the
   moment they hold state, and that is the next step. *)

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

let mural =
  Image.make ~width:48 (fun ~u ~v ->
      let ring r w =
        Image.disc ~cx:24. ~cy:24. ~r ~u ~v
        && not (Image.disc ~cx:24. ~cy:24. ~r:(r -. w) ~u ~v)
      in
      if ring 20. 3. || ring 11. 3. || Image.disc ~cx:24. ~cy:24. ~r:3. ~u ~v
      then (Color.rgb 208 178 122, 255)
      else Image.clear)

let brazier_cold =
  Image.make ~width:24 (fun ~u ~v ->
      if v >= 10 && Image.disc ~cx:12. ~cy:14. ~r:9. ~u ~v then
        (Color.rgb 52 48 46, 255)
      else Image.clear)

let torch_stub =
  Image.make ~width:12 ~height:24 (fun ~u ~v ->
      if u >= 5 && u <= 7 && v >= 8 then (Color.rgb 74 56 40, 255)
      else Image.clear)

let mote =
  Image.make ~width:8 (fun ~u ~v ->
      if Image.disc ~cx:4. ~cy:4. ~r:3. ~u ~v then (Color.rgb 240 210 140, 255)
      else Image.clear)

let height = 4.
let flat = Plane.horizontal 0.

(* The vault's corners. *)
let sw = Vec.make (-6.) (-6.)
let se = Vec.make 6. (-6.)
let ne = Vec.make 6. 6.
let nw = Vec.make (-6.) 6.

(* The corridor's, in a frame of its own — rooms share no compass, and only
   the link will say where it stands. *)
let c_sw = Vec.make 0. (-2.)
let c_se = Vec.make 8. (-2.)
let c_ne = Vec.make 8. 2.
let c_nw = Vec.make 0. 2.

(* The doorway the two rooms share, as a door on each side. The corridor's
   floor is not here because it is not written: a room that gives its floor no
   plane takes its neighbour's through the connection between them. *)
let east = P.door ~name:"east" ~width:2. ~clearance:2.6 ()
let west = P.door ~name:"west" ~width:2. ~clearance:2.6 ()

(* A component: a function from props to a description, declared once at the
   top level. Every torch placed with it is its own instance. *)
let torch =
  Element.declare ~name:"torch" @@ fun (pos : Vec.t) ->
  P.sprite ~size:0.7 ~base:0.9 ~image:torch_stub pos

let pillar center =
  P.block ~height ~material:slab
    (P.polygon ~center ~radius:0.7 ~sides:6 ~rotation:0.)

let level =
  P.(
    world ~atmosphere:air
      [
        room ~height ~material:stone ~floor:(floor ~plane:flat ground)
          ~ceiling:(roof stone)
          ~outline:
            [
              corner sw ~material:brick
                ~decals:
                  [
                    decal ~along:6. ~z:2. ~half_width:1.2 ~half_height:1.2 mural;
                  ];
              corner se;
              corner ne;
              corner nw;
            ]
          [
            spawn (Vec.make (-4.5) 0.);
            cut east ~along:(se, ne);
            wall ~height:1.1 ~material:slab (Vec.make (-2.) (-1.6))
              (Vec.make (-2.) 1.6);
            pillar (Vec.make 3. 3.);
            pillar (Vec.make 3. (-3.));
            pillar (Vec.make (-3.) 3.);
            pillar (Vec.make (-3.) (-3.));
            sprite ~size:0.9 ~image:brazier_cold (Vec.make 0. 0.);
            torch ~key:"ne" (Vec.make 2.1 2.1);
            torch ~key:"se" (Vec.make 2.1 (-2.1));
            torch ~key:"nw" (Vec.make (-2.1) 2.1);
            torch ~key:"sw" (Vec.make (-2.1) (-2.1));
            sprite ~base:1.4 ~glow:0.9 ~size:0.25 ~image:mote (Vec.make 1.5 2.);
          ];
        room ~height ~material:stone ~floor:(floor ground) ~ceiling:(roof stone)
          ~outline:(corners [ c_sw; c_se; c_ne; c_nw ])
          [ cut west ~along:(c_nw, c_sw) ];
        connect east west;
      ])

let () =
  match Run.play ~title:"The Undercroft" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
