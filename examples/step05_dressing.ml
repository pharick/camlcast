(* Step 5 of doc/making-a-game.mld — "Pictures in the room". The guide quotes
   only what each step adds; this file is the whole game as of this step.

   New here: Image, a decal flat on a wall, and two sprites standing in the
   room — one on the floor, one floating clear of it with a glow of its own. *)

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

(* A mural: two rings and a point, drawn by a pure function. Everything not
   painted is Image.clear, and the clear parts of a decal show the wall. *)
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

let mote =
  Image.make ~width:8 (fun ~u ~v ->
      if Image.disc ~cx:4. ~cy:4. ~r:3. ~u ~v then (Color.rgb 240 210 140, 255)
      else Image.clear)

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
    world ~atmosphere:air
      ~spawn:("vault", Vec.make (-4.5) 0.)
      [
        room ~name:"vault"
          ~floor:(floor ~plane:flat ~material:ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
            boundary ~height ~material:stone
              [
                (* The mural hangs on the wall this corner describes, placed
                   in that wall's own terms: along from its first end, z up
                   from the floor. *)
                corner sw ~material:brick
                  ~decals:
                    [
                      decal ~along:6. ~z:2. ~half_width:1.2 ~half_height:1.2
                        mural;
                    ];
                corner se;
                corner ne;
                corner nw;
              ];
            wall ~height:1.1 ~material:slab (Vec.make (-2.) (-1.6))
              (Vec.make (-2.) 1.6);
            pillar (Vec.make 3. 3.);
            pillar (Vec.make 3. (-3.));
            pillar (Vec.make (-3.) 3.);
            pillar (Vec.make (-3.) (-3.));
            (* A sprite stands at a point and turns to face you; size is its
               height and the width follows the picture. *)
            sprite ~size:0.9 ~image:brazier_cold (Vec.make 0. 0.);
            (* base floats it clear of the floor, and glow is the light it
               makes of its own — which is what keeps it visible when the
               room's light goes. *)
            sprite ~base:1.4 ~glow:0.9 ~size:0.25 ~image:mote (Vec.make 1.5 2.);
          ];
      ])

let () =
  match Run.play ~title:"The Undercroft" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
