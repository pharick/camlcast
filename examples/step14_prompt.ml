(* Step 14 of doc/making-a-game.mld — "Asking what the eye is on". The guide
   quotes only what each step adds; this file is the whole game as of this
   step.

   New here: a prompt that appears when the crosshair rests on something
   workable within reach, read from Events.use_aim — the same question
   on_gaze answers, asked from the other side. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)
let brick = dressed (Color.rgb 146 88 70)
let slab = dressed (Color.rgb 92 96 108)

(* The air is now a function of how much fire is left: full is the step-4
   gloom, none is nearly night. *)
let air ~fire =
  Atmosphere.make ~haze:(Color.rgb 22 22 30)
    ~fog_distance:(4. +. (6. *. fire))
    ~min_brightness:(0.06 +. (0.06 *. fire))
    ~light:(Vec.make (-0.5) (-0.85))
    ~ambient:(0.12 +. (0.18 *. fire))
    ~directional:(0.3 +. (0.4 *. fire))
    ()

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

let brazier_hot =
  Image.make ~width:24 (fun ~u ~v ->
      if v >= 10 && Image.disc ~cx:12. ~cy:14. ~r:9. ~u ~v then
        (Color.rgb 52 48 46, 255)
      else if Image.disc ~cx:12. ~cy:8. ~r:5. ~u ~v then
        (Color.rgb 244 176 84, 255)
      else Image.clear)

let torch_stub =
  Image.make ~width:12 ~height:24 (fun ~u ~v ->
      if u >= 5 && u <= 7 && v >= 8 then (Color.rgb 74 56 40, 255)
      else Image.clear)

let torch_flame =
  Image.make ~width:12 ~height:24 (fun ~u ~v ->
      if u >= 5 && u <= 7 && v >= 8 then (Color.rgb 74 56 40, 255)
      else if Image.disc ~cx:6. ~cy:5. ~r:4. ~u ~v then
        (Color.rgb 248 188 92, 255)
      else Image.clear)

(* How close the crosshair's target has to be before E works it. The eye can
   reach as far as it can see; whether a thing can be worked from here is the
   game's own rule, and Aim.spot is what the rule is written with. *)
let reach = 2.5

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

(* The corridor's floor is the vault's, carried through the doorway the two
   rooms share. Derived, it cannot drift — and Check would report a step in
   the floor if it did. *)
let corridor_floor =
  P.through
    ~from:(P.opening ~width:2. se ne)
    ~into:(P.opening ~width:2. c_sw c_nw)
    flat

(* A component: a function from props to a description, declared once at the
   top level. Every torch placed with it is its own instance.

   on_gaze is an enter and a leave — true when the crosshair arrives, false
   when it goes — and on_use fires when the player works the use control (E,
   unless rebound) while looking at this torch. *)
let torch =
  Element.declare ~name:"torch" @@ fun (pos : Vec.t) ->
  let lit, set_lit = Hook.use_state false in
  let eyed, set_eyed = Hook.use_state false in
  P.sprite ~size:0.7 ~base:0.9
    ~glow:(if lit then 0.9 else if eyed then 0.25 else 0.)
    ~image:(if lit then torch_flame else torch_stub)
    ~on_gaze:set_eyed
    ~on_use:(fun (spot : Aim.spot) ->
      if spot.distance <= reach then set_lit (not lit))
    pos

(* A wisp drifting a slow circle. The phase lives in a ref, not state: a ref
   is a box handed back unchanged every render, and writing it asks for no
   frame — which is exactly right for a number that changes every frame
   anyway, since the loop is already rendering every frame. State is for what
   arrives in steps; a ref is for what merely accumulates. *)
let wisp =
  Element.declare ~name:"wisp" @@ fun (center : Vec.t) ->
  let phase = Hook.use_ref 0. in
  Events.use_frame (fun ~dt -> phase := !phase +. dt);
  let pos = Vec.add center (Vec.scale (Vec.of_angle (0.45 *. !phase)) 2.6) in
  P.sprite ~key:"wisp"
    ~base:(1.3 +. (0.25 *. sin !phase))
    ~glow:0.9 ~size:0.25 ~image:mote pos

type brazier_props = { fuel : float; pos : Vec.t }

(* The brazier no longer holds its own state: it is told what it holds,
   because the world's air depends on the same number. State lives at the
   lowest place everything that reads it can reach. *)
let brazier =
  Element.declare ~name:"brazier" @@ fun { fuel; pos } ->
  P.sprite ~size:0.9
    ~glow:(if fuel > 0. then 0.85 else 0.)
    ~image:(if fuel > 0. then brazier_hot else brazier_cold)
    pos

let pillar center =
  P.boundary ~height ~material:slab
    (P.polygon ~center ~radius:0.7 ~sides:6 ~rotation:0.)

(* How long a struck brazier burns, in seconds. *)
let fuse = 40.

let message =
  "The undercroft is dark. Strike the brazier with Space, and carry what light \
   you can."

let game =
  Element.declare ~name:"game" @@ fun (font : Font.t) ->
  let fuel, set_fuel = Hook.use_state 0. in
  Events.use_pressed (Input.Key Key.space) (fun () -> set_fuel fuse);
  (* Scale the work by dt: a frame's length is not a constant, and the first
     call comes with dt = 0. before anything is drawn. *)
  Events.use_frame (fun ~dt ->
      if fuel > 0. then set_fuel (Float.max 0. (fuel -. dt)));
  (* The buffer's own size — not the window's. The engine renders at a
     whole-number fraction of the window and stretches the result, so a HUD
     that wants an edge has to ask where the edges are. *)
  let w, h = Events.use_viewport () in
  (* An eased meter: the drawn fraction chases the true one at a rate scaled
     by how long the last frame took. A ref again — the chase is redrawn
     every frame regardless. *)
  let dt = Events.use_dt () in
  let shown = Hook.use_ref 0. in
  shown := !shown +. (((fuel /. fuse) -. !shown) *. Float.min 1. (6. *. dt));
  (* Wrapping is arithmetic over every character, so it is worked out once
     and remembered until what it depends on — the buffer's width — changes.
     use_memo, not an if: hooks run unconditionally, and the lines are simply
     not drawn once the brazier is lit. *)
  let lines =
    Hook.use_memo ~deps:w (fun () -> Font.wrap font message ~width:(w - 24))
  in
  (* Asked, not told: use_aim is what the crosshair was on when the last
     frame was drawn, for a description that wants to say something about it
     without the thing itself having to say so. It knows the kind and the
     geometry, never the identity — the torch's own on_use still decides
     what E does. *)
  let prompt =
    match Events.use_aim () with
    | Some { Aim.where = Aim.On_sprite; distance; _ } when distance <= reach ->
        Some "press E"
    | _ -> None
  in
  P.(
    world
      ~atmosphere:(air ~fire:(fuel /. fuse))
      ~spawn:("vault", Vec.make (-4.5) 0.)
      [
        room ~name:"vault"
          ~floor:(floor ~plane:flat ~material:ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
            (* Three sides run as an open boundary; the fourth is cut. The
               last corner of an open run describes no wall, so it carries
               nothing. *)
            boundary ~closed:false ~height ~material:stone
              [
                corner ne;
                corner nw;
                corner sw ~material:brick
                  ~decals:
                    [
                      decal ~along:6. ~z:2. ~half_width:1.2 ~half_height:1.2
                        mural;
                    ];
                corner se;
              ];
            doorway ~name:"east" ~width:2. ~opening:2.6 ~height ~material:stone
              se ne;
            wall ~height:1.1 ~material:slab (Vec.make (-2.) (-1.6))
              (Vec.make (-2.) 1.6);
            pillar (Vec.make 3. 3.);
            pillar (Vec.make 3. (-3.));
            pillar (Vec.make (-3.) 3.);
            pillar (Vec.make (-3.) (-3.));
            brazier { fuel; pos = Vec.make 0. 0. };
            torch ~key:"ne" (Vec.make 2.1 2.1);
            torch ~key:"se" (Vec.make 2.1 (-2.1));
            torch ~key:"nw" (Vec.make (-2.1) 2.1);
            torch ~key:"sw" (Vec.make (-2.1) (-2.1));
            wisp (Vec.make 0. 0.);
          ];
        room ~name:"corridor"
          ~floor:(floor ~plane:corridor_floor ~material:ground)
          ~ceiling:
            (roof ~plane:(Plane.above corridor_floor height) ~material:stone)
          [
            boundary ~closed:false ~height ~material:stone
              (corners [ c_nw; c_ne; c_se; c_sw ]);
            doorway ~name:"west" ~width:2. ~opening:2.6 ~height ~material:stone
              c_sw c_nw;
          ];
        link ("vault", "east") ("corridor", "west");
        hud
          ((if fuel <= 0. then
              List.mapi
                (fun i line -> text ~font ~x:12 ~y:(12 + (i * 12)) line)
                lines
            else [])
          @ [
              rect ~alpha:140
                ~x:(((w - 120) / 2) - 4)
                ~y:(h - 24) ~w:128 ~h:16 ~color:(Color.rgb 10 10 14) ();
              bar
                ~x:((w - 120) / 2)
                ~y:(h - 20) ~w:120 ~h:8 ~fraction:!shown
                ~color:(Color.rgb 230 170 80) ();
              highlight ();
              crosshair ();
              (match prompt with
              | None -> Element.empty
              | Some line ->
                  let tw, _ = Font.measure font line in
                  text ~font ~x:((w - tw) / 2) ~y:(h - 36) line);
            ]);
      ])

(* The engine holds no font, so the game brings one: a 6x10 atlas starting
   at code point 32, with a hollow box standing in for anything outside it.
   Run `dune build` once first — that is what copies assets/ beside the
   executables in _build. *)
let typeface =
  Result.map
    (fun atlas ->
      Font.make ~fallback:'\127' ~atlas ~width:6 ~height:10 ~first:32 ())
    (Image.of_asset "assets/font.png")

let () =
  match typeface with
  | Error (`Msg reason) ->
      prerr_endline reason;
      exit 1
  | Ok font -> (
      match Run.play ~title:"The Undercroft" (game font) with
      | Ok _ending -> ()
      | Error (`Msg reason) ->
          prerr_endline reason;
          exit 1)
