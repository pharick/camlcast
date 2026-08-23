(** {b Looking through a doorway.} Names what the crosshair is on, including
    when it is in the next room.

    {!Camlcast_core.Sight.look} traces the middle of the view through every open
    doorway the frame was drawn through, carrying it into each next room's
    frame, and reports what it meets first: which room, and which wall, sprite
    or threshold of it. Everything that stops the eye stops the trace — a nearer
    sprite, an opaque wall, a shut door, the lintel over an opening — so what
    can be picked is what can be seen. There is one doorway here; the rule holds
    for any number in a line.

    The crosshair colour reports the hit:

    - {b white} — nothing;
    - {b amber} — something in the current room;
    - {b blue} — a doorway, or the wall over one;
    - {b green} — a barrel in the room beyond, collectible.

    Pressing {b E} on a green one records it: a tick appears along the bottom,
    and that barrel cannot be recorded twice. Inside the far room the barrels
    turn amber — they are in the current room now, and this demo will not take
    them. That rule is the demo's, not the engine's: {!Camlcast_core.Sight}
    reports how many doorways it looked through and {!may_take} is where the "at
    least one" is written down. The engine has no notion of a thing worth
    collecting, only of the sprite that happens to be one.

    The target is {b ringed} from the same numbers the renderer drew it with, so
    the ring lands on it exactly, even through the doorway and in the far room's
    own coordinates. {!Camlcast_core.Sight.t} carries the pose to work that out
    from.

    A {b picture hangs on the far wall}. A wall hit reports which of its decals
    is under the crosshair, by the same rule that drew it, alpha and all; an
    inch beside the frame the wall reports bare.

    The two rings are deliberately different shapes. A sprite faces the camera,
    so it rings as a rectangle. A picture is flat on a receding wall, so its far
    edge is shorter than its near one and the ring is a trapezoid; it squares up
    only when viewed square on.

    Two more cases: with one barrel behind another the near one wins and the far
    one cannot be taken; and the gap between a barrel's outline and the corner
    of its box reads white, because a sprite is a cut-out and the pick is asked
    of the texel rather than the box. *)

open Camlcast

let height = 4.

(** Clearer air than the other demos use: the targets are in the next room and a
    good way off, and must stay visible. *)
let air =
  Atmosphere.make ~haze:(Color.rgb 26 26 34) ~fog_distance:22.
    ~min_brightness:0.4 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.65
    ~directional:0.35 ()

let flat = Plane.horizontal 0.
let ground = P.floor ~plane:flat Surfaces.ground
let roofed = P.roof ~plane:(Plane.above flat height) Surfaces.soffit

(** Three barrels spread across the doorway's view: one square on, one to each
    side, so turning the head picks a different one.

    Each is keyed by a name, and that name is what it is recorded under, rather
    than a room and a sprite index — numbers that assembling a description
    happens to produce rather than anything the demo meant. *)
let barrels =
  [
    ("straight", Vec.make 3. 0.);
    ("aside", Vec.make 4. 2.5);
    ("across", Vec.make 4. (-2.5));
  ]

(** This demo's rule about what may be recorded, and the whole of it: a barrel,
    in a room the eye reached through at least one doorway, not recorded
    already.

    The [crossed] test is the "from safety" part — the next room may be studied
    without standing in it, and the current room's contents do not count.
    Nothing in the engine says so. *)
let may_take ~collected name (spot : Aim.spot) =
  spot.Aim.crossed >= 1 && not (List.mem name collected)

(** The ring and crosshair colour: the demo's verdict on the target.

    Two questions go to two different sources. What {e kind} of thing it is
    comes from {!Camlcast.Events.use_aim}, which answers about the crosshair;
    {e which} barrel it is comes from the barrel, told by [on_gaze]. *)
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
            boundary ~closed:false ~height ~material:Surfaces.brick
              (corners
                 [
                   Vec.make 6. 6.;
                   Vec.make (-6.) 6.;
                   Vec.make (-6.) (-6.);
                   Vec.make 6. (-6.);
                 ]);
            doorway ~name:"east" ~width:2.6 ~opening:2.8 ~height
              ~material:Surfaces.brick (Vec.make 6. (-6.)) (Vec.make 6. 6.);
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 2. 3.5);
          ];
        room ~name:"far" ~floor:ground ~ceiling:roofed
          ([
             (* One on the far room's side wall, which the doorway only ever
                shows at an angle — the ring round this one is a trapezoid. *)
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

let run window = Run.on window ~controls:Bindings.escapable (studying ())
