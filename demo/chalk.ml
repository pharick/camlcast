(** {b Marking a wall.} Aim at one, press {b C}, and a chalk symbol is on it —
    where the crosshair was, on the face you were looking at, and there for the
    rest of the run.

    {!Camlcast.Sight.look} already said what the crosshair is on. A wall hit
    reports four numbers, and they are exactly the four a
    {!Camlcast.Room.type-decal} is made of: [index] is which wall, [along] and
    [z] are that wall's own coordinates, and [facing] is the face being looked
    at. Nothing is converted between the two ends — the mark is put where the
    crosshair was because those are the same numbers.

    {b Marks are on one side.} A chalk stroke is paint, and paint is on one face
    of a wall. The free-standing partition across the middle of the hall is the
    only place you can check that: mark it, walk round an end, and there is
    nothing on the back. The rest of the hall is its own boundary, and the far
    face of that is not somewhere you can get to.

    {b Marks survive the room being rebuilt.} The hall is built again from its
    parts every frame — every wall, both planes, all new values. The chalk is
    not part of that room. It is a list this demo keeps, re-applied with
    {!Camlcast.Room.add_decal} after the rebuild, which is the only thing that
    makes it persistent: the engine has no memory of a mark, and a game that
    dropped its list would lose them all.

    {b What is the game's, not the engine's.}

    - {b Eight strokes.} How many marks there may be is a rule with no engine
      opinion behind it. The counter is in this file.
    - {b Two symbols,} an arrow and a cross, on {b 1} and {b 2}. What a mark
      means is the player's business and nothing else here reads it. They are
      also chalked with different [glow]s — see below.
    - {b Your own room only.} You can see a wall through the doorway and
      {!Camlcast.Sight} will name it, but chalking it would be reaching through
      a wall. {!markable} is where "not through a doorway" is written down, in
      one line, off [crossed].
    - {b Within arm's reach.} §6 has chalk placed "directly on" a wall, so a
      wall further than {!reach} away is named but not markable — walk up to the
      one you mean. That is one comparison against the [distance]
      {!Camlcast.Sight} already reports.

    The crosshair says which of those you are up against: {b white} for nothing,
    {b amber} for a wall you cannot mark, and {b chalk} for one you can. Where
    there is something to be done about it — walk closer, turn round, find more
    chalk — {!refusal} puts the word under the crosshair.

    {b The lamp is the atmosphere, and one of the two chalks glows against it.}
    A failing lamp here closes {!Camlcast.World.atmosphere} in and touches
    nothing else — a single record update per frame, no room rebuilt for it, and
    what a game should want a lamp to be.

    That would take plain chalk with it. A decal is lit by exactly the same
    orientation-and-fog factor as the wall under it, so paint fades into the
    dark along with what it is painted on — correct for a poster, and useless
    for the marks a player left to find their way back by. Which is what
    {!Camlcast.Room.type-decal}'s [glow] is for, and why the two symbols here
    carry different ones.

    Chalk a wall with an arrow and a cross side by side and watch the lamp go
    down. Both dim, because a glow lifts a decal towards its own colours rather
    than pinning it there. But the arrow dims {e with} the wall and the cross
    hardly does: across the swing the wall falls to 0.39 of its brightness and
    the arrow to 0.39 with it, while the cross keeps 0.86. §6's "faintly
    phosphorescent" is that one number, it is per mark rather than per room, and
    it does not change as the light does — a material that glows glows the same
    whatever the lamp is doing. *)

open Camlcast
open Result_ext

let height = 3.6
let strokes = 8
let lamp_period = 9.

(** How far away a wall may be and still be chalkable, in cells.

    §6 has the player place chalk "directly on" a wall, so this is close enough
    to be an arm's length and no more: walk up to the wall you mean. Collision
    stops the player {!Camlcast.Config.collision_padding} short of one, so every
    wall in this hall can be reached. *)
let reach = 2.

(** {1 The symbols}

    Two marks, drawn cut out against {!Camlcast.Image.clear} so a stroke is a
    stroke and not a white tile on the wall. They are values in this file, made
    once when it loads, for the same reason every animation frame in this
    library is: a mark is placed while the game is running and nothing is going
    to generate a picture then. *)

let chalk = Color.rgb 236 233 222

(** A thick line from one point to another, in texels, as a predicate. *)
let stroke ~x0 ~y0 ~x1 ~y1 ~width u v =
  let px = float_of_int u +. 0.5 and py = float_of_int v +. 0.5 in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let len2 = (dx *. dx) +. (dy *. dy) in
  let t =
    if len2 = 0. then 0.
    else
      Float.max 0.
        (Float.min 1. ((((px -. x0) *. dx) +. ((py -. y0) *. dy)) /. len2))
  in
  Float.hypot (px -. (x0 +. (t *. dx))) (py -. (y0 +. (t *. dy))) <= width

let size = 24

let drawn f =
  Image.make size (fun ~u ~v -> if f u v then (chalk, 255) else Image.clear)

(** An arrow: a shaft up the middle and two barbs off its head. *)
let arrow =
  drawn (fun u v ->
      let line x0 y0 x1 y1 = stroke ~x0 ~y0 ~x1 ~y1 ~width:1.6 u v in
      line 12. 20. 12. 5. || line 12. 4. 5. 12. || line 12. 4. 19. 12.)

(** A cross: two strokes corner to corner. *)
let cross =
  drawn (fun u v ->
      let line x0 y0 x1 y1 = stroke ~x0 ~y0 ~x1 ~y1 ~width:1.6 u v in
      line 5. 5. 19. 19. || line 19. 5. 5. 19.)

(** The two symbols, and how much of its own light each one makes.

    They differ deliberately. The {b arrow} is plain chalk: a
    {!Camlcast.Room.type-decal} with no [glow], lit by the room like the wall it
    is on, so it goes into the dark with everything else. The {b cross} is the
    phosphorescent kind. Mark a wall with one of each, let the lamp go down, and
    one of them is still there.

    A constant, and not something that rises as the light fails: a material that
    glows glows the same whatever else is happening, and a glow tuned to cancel
    the lamp exactly would leave the mark pinned at one brightness, which reads
    as a sticker rather than a surface. *)
let symbols = [| ("arrow", arrow, 0.); ("cross", cross, 0.7) |]

(** {1 The world} *)

type mark = {
  room : int;
  wall : int;
  along : float;
  z : float;
  facing : Room.side;
  symbol : int;
}
(** One chalk stroke, in the only terms that survive a room being rebuilt: a
    room and a wall by {e index}, and the wall's own coordinates. Not a
    {!Camlcast.Room.type-decal} — a decal belongs to the room it is on, and this
    list has to outlive several of those. *)

type t = {
  player : Player.t;
  marks : mark list;  (** newest first *)
  selected : int;  (** which of {!symbols} the next stroke will be *)
  left : int;  (** strokes remaining *)
  elapsed : float;
}

let hall_sw = Vec.make (-6.) (-5.)
let hall_se = Vec.make 6. (-5.)
let hall_ne = Vec.make 6. 5.
let hall_nw = Vec.make (-6.) 5.
let back_sw = Vec.make 0. (-4.)
let back_se = Vec.make 7. (-4.)
let back_ne = Vec.make 7. 4.
let back_nw = Vec.make 0. 4.

let back_jambs, back_here =
  Room.doorway ~name:"here" ~width:2.2 ~opening:2.8 ~height
    ~material:Surfaces.brick back_nw back_sw

(** The hall, built again.

    Nothing in here depends on the clock: the lamp is the {!Camlcast.Atmosphere}
    and the surfaces never change. Every wall — the jambs of the doorway
    included — both planes and the partition are nevertheless {e new values}
    every time this is called, and that is the point. A game rebuilds a room
    whenever anything in it changes, and this demo has to show that the marks
    come through one. They are not being left undisturbed; they are being put
    back.

    The doorway is cut here rather than once outside so that the jambs are
    rebuilt with everything else. Its {!Camlcast.Room.type-threshold} comes out
    the same every time — same name, same endpoints, same height — which is
    exactly what {!Camlcast.World.replace_room} insists on, since a portal was
    derived from those. *)
let hall () =
  let jambs, onward =
    Room.doorway ~name:"onward" ~width:2.2 ~opening:2.8 ~height
      ~material:Surfaces.brick hall_se hall_ne
  in
  let wall material a b = Room.wall ~height ~material a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ onward ]
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.above floor height; material = Surfaces.soffit })
    (jambs
    @ [
        wall Surfaces.stone hall_sw hall_se;
        wall Surfaces.stone hall_ne hall_nw;
        wall Surfaces.stone hall_nw hall_sw;
        (* The one wall with two faces you can get to. Chalk it and walk round
           an end: the far side is bare. It is opaque, and has to be — a bare
           wall you can see through is one {!Camlcast.Sight} looks {e through},
           so the crosshair never lands on it and the first mark could never be
           placed. (A mark already on such a wall is a different matter: that
           one is drawn, so that one can be picked.) *)
        Room.wall ~height:2.4 ~material:Surfaces.panel (Vec.make (-1.5) 1.)
          (Vec.make 2.5 1.);
      ])

let back =
  Room.make ~thresholds:[ back_here ]
    ~floor:{ Room.plane = Plane.horizontal 0.; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.horizontal height; material = Surfaces.soffit })
    ~sprites:[ Room.sprite ~size:1.7 ~image:Pictures.figure (Vec.make 4. 0.) ]
    (back_jambs
    @ [
        Room.wall ~height ~material:Surfaces.stone back_sw back_se;
        Room.wall ~height ~material:Surfaces.brick back_se back_ne;
        Room.wall ~height ~material:Surfaces.stone back_ne back_nw;
      ])

let world =
  World.make
    ~rooms:[ ("hall", hall ()); ("back", back) ]
    ~links:[ (("hall", "onward"), ("back", "here")) ]
    ~atmosphere:Surfaces.air
    ~spawn:("hall", Vec.make (-4.) (-2.))

let start =
  {
    player = Player.spawn world;
    marks = [];
    selected = 0;
    left = strokes;
    elapsed = 0.;
  }

(** {1 The rules this demo has and the engine does not} *)

(** May the crosshair's target be marked, and where? A wall, in the room you are
    standing in, within {!reach}, with a stroke left. Everything before the
    [when] is {!Camlcast.Sight}'s answer; everything in it is this demo's rule.

    {!Camlcast.Sight.t} carries the [distance] already — it is how far the ray
    went to find what it found — so "too far away" costs a comparison and no
    engine support at all. *)
let markable state = function
  | Some { Sight.kind = Sight.Wall w; room; crossed; distance; _ }
    when crossed = 0 && distance <= reach && state.left > 0 ->
      Some
        {
          room;
          wall = w.index;
          along = w.along;
          z = w.z;
          facing = w.facing;
          symbol = state.selected;
        }
  | _ -> None

(** How bright the lamp is: a slow swing, never all the way out. *)
let lamp elapsed =
  0.35
  +. (0.65 *. ((1. +. cos (elapsed /. lamp_period *. 2. *. Float.pi)) /. 2.))

(** The air at that brightness, and the whole of the lamp: fog closing in and
    less of everything reaching you. Nothing else in the room changes with it.

    {!Camlcast.World.atmosphere} is a plain field the renderer reads every
    frame, so a lamp is one record update and no rebuilding of anything. *)
let air ~lamp =
  Atmosphere.make ~haze:(Color.rgb 18 18 24)
    ~fog_distance:(2.5 +. (11. *. lamp))
    ~min_brightness:(0.05 +. (0.2 *. lamp))
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:(0.3 +. (0.3 *. lamp))
    ~directional:0.4

(** The world as it stands: the hall built again, in air of this moment's
    brightness, then every mark put back onto it.

    Each mark takes the [glow] its symbol was given, so the two kinds behave
    differently in the same room and under the same lamp. Nothing here depends
    on the brightness: the glow is a property of the chalk, and the lamp is a
    property of the air.

    Oldest mark first, so the newest ends up on top of the pile — which is the
    order {!Camlcast.Room.add_decal} appends in and the order {!Camlcast.Sight}
    reads back. *)
let dressed state =
  let brightness = lamp state.elapsed in
  let lit = World.replace_room world ~room:0 ~replacement:(hall ()) in
  let lit = { lit with World.atmosphere = air ~lamp:brightness } in
  List.fold_left
    (fun w m ->
      let _, image, glow = symbols.(m.symbol) in
      World.replace_room w ~room:m.room
        ~replacement:
          (Room.add_decal (World.room w m.room) ~wall:m.wall
             (Room.decal ~facing:m.facing ~glow ~along:m.along ~z:m.z
                ~half_width:0.22 ~half_height:0.22 image)))
    lit (List.rev state.marks)

(** Chalk whatever the crosshair is on, or do nothing if it is not something
    this demo will mark.

    The sighting is taken against the world as it is {e drawn}, marks and all,
    rather than against the authored one — so what can be marked is what can be
    seen, including a wall that a mark already on it does not hide. Split out of
    {!update} because the test suite drives it: pressing a key is SDL's, but
    what pressing it does is the rule worth asserting. *)
let place state =
  match markable state (Sight.look (dressed state) state.player) with
  | Some mark ->
      { state with marks = mark :: state.marks; left = state.left - 1 }
  | None -> state

let update state ~dt ~motion ~actions =
  let player = Engine.step world state.player motion in
  let selected =
    if Input.pressed actions (Input.Key Key.k1) then 0
    else if Input.pressed actions (Input.Key Key.k2) then 1
    else state.selected
  in
  let state = { state with player; selected; elapsed = state.elapsed +. dt } in
  if Input.pressed actions (Input.Key Key.c) then place state else state

let view state = (dressed state, state.player)

(** {1 The overlay} *)

(* Shared with the menu, and with nothing to teach here: this demo is about
   marking a wall, and the words over it are only there to say why it will not.
   {!Text} still builds its own, because there the font is the lesson. *)
let font = Typeface.font

(** Why the crosshair's target cannot be chalked, in a word, or [None] where it
    can be or where saying so would not help. Only the reasons the player can do
    something about are worth a word: walk closer, or turn round and use one of
    your own walls. *)
let refusal state seen =
  if markable state seen <> None then None
  else
    match seen with
    | _ when state.left = 0 -> Some "no chalk left"
    | Some { Sight.kind = Sight.Wall _; crossed; distance; _ } ->
        if crossed > 0 then Some "another room"
        else if distance > reach then Some "too far"
        else None
    | _ -> None

let overlay fb state =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let seen = Sight.look (dressed state) state.player in
  let can = markable state seen <> None in
  let r, g, b =
    if can then (236, 233, 222)
    else match seen with Some _ -> (215, 165, 90) | None -> (150, 150, 155)
  in
  Paint.crosshair fb ~r ~g ~b;
  let font = Lazy.force font in
  let pad = 6 in
  (* A word under the crosshair when there is something to be done about it. *)
  (match refusal state seen with
  | None -> ()
  | Some why ->
      let w, _ = Font.measure font why in
      Font.draw fb font why
        ~x:((width - w) / 2)
        ~y:((height / 2) + (2 * pad))
        ~color:(Color.rgb 215 165 90));
  let name, _, glow = symbols.(state.selected) in
  let line =
    Printf.sprintf "%s  glow %.2f   %d of %d strokes left" name glow state.left
      strokes
  in
  let tw, th = Font.measure font line in
  Paint.rect fb ~x:pad
    ~y:(height - th - (3 * pad))
    ~w:(tw + (2 * pad))
    ~h:(th + (2 * pad))
    ~r:12 ~g:14 ~b:22 ~alpha:190;
  Font.draw fb font line ~x:(2 * pad)
    ~y:(height - th - (2 * pad))
    ~color:(Color.rgb 236 233 222);
  (* Printed from the keys themselves rather than spelled out, so that moving a
     binding above moves the words here with it. *)
  let help =
    Printf.sprintf "%s to mark   %s arrow   %s cross" (Key.name Key.c)
      (Key.name Key.k1) (Key.name Key.k2)
  in
  let hw, _ = Font.measure font help in
  Font.draw fb font help
    ~x:(width - hw - pad)
    ~y:pad ~color:(Color.rgb 140 146 160)

let run () =
  let+ _, ending =
    Engine.run ~bindings:Bindings.escapable ~update ~view ~overlay start
  in
  ending
