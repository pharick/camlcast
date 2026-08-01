(** {b Looking through a doorway.} What the crosshair is on, named — including
    when it is in the room next door.

    {!Camlcast_core.Sight.look} traces the middle of the view through one open
    doorway, carries it into the next room's frame, and reports what it meets
    first: which room, and which wall, sprite or threshold of it. Everything
    that stops the eye stops it — a nearer sprite, an opaque wall, a shut door,
    the lintel over an opening — so what can be picked is what can be seen.

    Stand in this room and look through the opening at the three barrels beyond
    it. The crosshair tells you what it has found:

    - {b white} — nothing;
    - {b amber} — something in the room you are standing in;
    - {b blue} — a doorway, or the wall over one;
    - {b green} — a barrel in the room beyond, which you may collect.

    Press {b E} on a green one and it is recorded: a tick appears along the
    bottom, and that barrel cannot be recorded twice. Walk into the far room and
    the barrels turn amber — they are in {e your} room now, and this demo will
    not take them. That rule is the demo's, not the engine's:
    {!Camlcast_core.Sight} reports how many doorways it looked through and
    {!collectable} is where the "at least one" is written down. The engine has
    no notion of a thing worth collecting, only of the sprite that happens to be
    one.

    Whatever is targeted is {b ringed}, from the same numbers the renderer drew
    it with — so the ring lands on it exactly, even through the doorway and in
    the far room's own coordinates. {!Camlcast_core.Sight.t} carries the pose to
    work that out from.

    There is a {b picture hung on the far wall} too. Aim at it and the crosshair
    says so: a wall hit reports which of its decals is under the crosshair, by
    the same rule that drew it, alpha and all. Aim at the wall an inch beside
    the frame and it is a bare wall again.

    The two rings are not the same shape, and that is the point. A sprite faces
    you, so it rings as a rectangle. A picture is flat on a wall, and a wall
    recedes — so its far edge is shorter than its near one and the ring is a
    trapezoid. Stand square on to the picture and it squares up; step to one
    side and watch it lean.

    Two more things worth trying. Walk so one barrel is behind another — the
    near one wins, and the far one cannot be taken. And aim at the gap between a
    barrel's outline and the corner of its box: the crosshair goes white,
    because a sprite is a cut-out and the pick is asked of the texel rather than
    the box. *)

open Camlcast

let height = 4.

(** Clearer air than the other demos use. What is being looked at here is in the
    next room and a good way off, and the point is to be able to see it. *)
let air =
  Atmosphere.make ~haze:(Color.rgb 26 26 34) ~fog_distance:22.
    ~min_brightness:0.4 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.65
    ~directional:0.35 ()

let flat = Plane.horizontal 0.
let ground = P.floor ~plane:flat ~material:Surfaces.ground
let roofed = P.roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit

(** Three of them, spread across the doorway's view: one square on, one to each
    side, so turning the head picks a different one.

    Each is keyed by a name, and that name is what it is recorded under — where
    the old version recorded a room and a sprite index, which are numbers
    assembling a description happens to produce and not anything the demo meant.
*)
let barrels =
  [
    ("straight", Vec.make 3. 0.);
    ("aside", Vec.make 4. 2.5);
    ("across", Vec.make 4. (-2.5));
  ]

(** This demo's rule about what may be recorded, and the whole of it: a barrel,
    in a room the eye reached through at least one doorway, that has not been
    recorded already.

    The [crossed] test is the "from safety" part — you may study the next room
    without standing in it, and what you are already standing among does not
    count. Nothing in the engine says so. *)
let may_take ~collected name (spot : Aim.spot) =
  spot.Aim.crossed >= 1 && not (List.mem name collected)

(** What colour to ring and aim in, which is the demo saying what it thinks of
    what you are looking at.

    Two questions are asked of two different things, because they are two
    different questions. What {e kind} of thing it is comes from
    {!Camlcast.Events.use_aim}, which answers about the crosshair; {e which}
    barrel it is comes from the barrel, which was told by [on_gaze]. *)
let tint ~collected ~aimed (aim : Aim.spot option) =
  match (aim, aimed) with
  | Some { Aim.where = Aim.On_sprite; crossed; _ }, Some name
    when crossed >= 1 && not (List.mem name collected) ->
      Color.rgb 120 230 130
  | Some { Aim.where = Aim.On_wall { decal = Some _; _ }; _ }, _ ->
      Color.rgb 215 130 235
  | Some { Aim.where = Aim.On_doorway; _ }, _ -> Color.rgb 120 170 240
  | Some _, _ -> Color.rgb 235 195 100
  | None, _ -> Color.rgb 245 245 245

(* Not (width, height): a local open of P puts a wall's height in scope. *)
let at ~collected ~aimed ~aim ~take ~look ~viewport:(_, down) =
  let unit = Int.max 3 (down / 60) in
  let color = tint ~collected ~aimed aim in
  P.(
    world ~atmosphere:air
      ~spawn:("near", Vec.make 1. 0.)
      [
        room ~name:"near" ~floor:ground ~ceiling:roofed
          [
            path ~height ~material:Surfaces.brick
              [
                Vec.make 6. 6.;
                Vec.make (-6.) 6.;
                Vec.make (-6.) (-6.);
                Vec.make 6. (-6.);
              ];
            doorway ~name:"east" ~width:2.6 ~opening:2.8 ~height
              ~material:Surfaces.brick (Vec.make 6. (-6.)) (Vec.make 6. 6.);
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 2. 3.5);
          ];
        room ~name:"far" ~floor:ground ~ceiling:roofed
          ([
             (* One down the far room's side wall, which the doorway only ever
                shows you at an angle — the ring round this one is a
                trapezoid. *)
             wall ~height ~material:Surfaces.stone (Vec.make 0. (-6.))
               (Vec.make 9. (-6.))
               ~decals:
                 [
                   decal ~along:6.5 ~z:1.7 ~half_width:1. ~half_height:1.
                     Pictures.poster;
                 ];
             (* And one on the end wall, square in the doorway's view. *)
             wall ~height ~material:Surfaces.stone (Vec.make 9. (-6.))
               (Vec.make 9. 6.)
               ~decals:
                 [
                   decal ~along:6. ~z:1.6 ~half_width:1. ~half_height:1.
                     Pictures.painting;
                 ];
             wall ~height ~material:Surfaces.stone (Vec.make 9. 6.)
               (Vec.make 0. 6.);
             doorway ~name:"west" ~width:2.6 ~opening:2.8 ~height
               ~material:Surfaces.stone (Vec.make 0. 6.) (Vec.make 0. (-6.));
           ]
          @ List.map
              (fun (name, pos) ->
                sprite ~key:name ~size:1.2 ~image:Pictures.barrel
                  ~on_gaze:(fun here -> look (if here then Some name else None))
                  ~on_use:(fun spot ->
                    if may_take ~collected name spot then take name)
                  pos)
              barrels);
        link ("near", "east") ("far", "west");
        hud
          ((* Round the target, wherever the renderer put it. *)
           highlight ~color ()
          :: crosshair ~color ()
          :: (* One tick per barrel recorded. *)
             List.mapi
               (fun i _ ->
                 rect
                   ~x:((2 * unit) + (i * 3 * unit))
                   ~y:(down - (5 * unit))
                   ~w:(2 * unit) ~h:(3 * unit) ~color:(Color.rgb 120 230 130) ())
               collected);
      ])

let studying =
  Element.declare ~name:"studying" @@ fun () ->
  let collected, set_collected = Hook.use_state [] in
  let aimed, set_aimed = Hook.use_state None in
  at ~collected ~aimed ~aim:(Events.use_aim ())
    ~take:(fun name -> set_collected (name :: collected))
    ~look:set_aimed ~viewport:(Events.use_viewport ())

let world =
  (Mount.build
     (at ~collected:[] ~aimed:None ~aim:None
        ~take:(fun _ -> ())
        ~look:(fun _ -> ())
        ~viewport:Events.still.Events.viewport))
    .Scene.world

let run window = Run.on window ~bindings:Bindings.escapable (studying ())
