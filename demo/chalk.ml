(** {b Marking a wall.} Aim at one, press {b C}, and a chalk symbol is on it —
    where the crosshair was, on the face you were looking at, and there for the
    rest of the run.

    {!Camlcast_core.Sight.look} already said what the crosshair is on. A wall
    hit reports four numbers, and they are exactly the four a
    {!Camlcast_core.Room.type-decal} is made of: [index] is which wall, [along]
    and [z] are that wall's own coordinates, and [facing] is the face being
    looked at. Nothing is converted between the two ends — the mark is put where
    the crosshair was because those are the same numbers.

    {b Marks are on one side.} A chalk stroke is paint, and paint is on one face
    of a wall. The free-standing partition across the middle of the hall is the
    only place you can check that: mark it, walk round an end, and there is
    nothing on the back. The rest of the hall is its own boundary, and the far
    face of that is not somewhere you can get to.

    {b Marks survive the room being rebuilt.} The hall is built again from its
    parts every frame — every wall, both planes, all new values. The chalk is
    not part of that room. It is a list this demo keeps, re-applied with
    {!Camlcast_core.Room.add_decal} after the rebuild, which is the only thing
    that makes it persistent: the engine has no memory of a mark, and a game
    that dropped its list would lose them all.

    {b What is the game's, not the engine's.}

    - {b Eight strokes.} How many marks there may be is a rule with no engine
      opinion behind it. The counter is in this file.
    - {b Two symbols,} an arrow and a cross, on {b 1} and {b 2}. What a mark
      means is the player's business and nothing else here reads it. They are
      also chalked with different [glow]s — see below.
    - {b Your own room only.} You can see a wall through the doorway and
      {!Camlcast_core.Sight} will name it, but chalking it would be reaching
      through a wall. {!markable} is where "not through a doorway" is written
      down, in one line, off [crossed].
    - {b Within arm's reach.} §6 has chalk placed "directly on" a wall, so a
      wall further than {!reach} away is named but not markable — walk up to the
      one you mean. That is one comparison against the [distance]
      {!Camlcast_core.Sight} already reports.

    The crosshair says which of those you are up against: {b white} for nothing,
    {b amber} for a wall you cannot mark, and {b chalk} for one you can. Where
    there is something to be done about it — walk closer, turn round, find more
    chalk — {!refusal} puts the word under the crosshair.

    {b The lamp is the atmosphere, and one of the two chalks glows against it.}
    A failing lamp here closes {!Camlcast_core.World.atmosphere} in and touches
    nothing else — a single record update per frame, no room rebuilt for it, and
    what a game should want a lamp to be.

    That would take plain chalk with it. A decal is lit by exactly the same
    orientation-and-fog factor as the wall under it, so paint fades into the
    dark along with what it is painted on — correct for a poster, and useless
    for the marks a player left to find their way back by. Which is what
    {!Camlcast_core.Room.type-decal}'s [glow] is for, and why the two symbols
    here carry different ones.

    Chalk a wall with an arrow and a cross side by side and watch the lamp go
    down. Both dim, because a glow lifts a decal towards its own colours rather
    than pinning it there. But the arrow dims {e with} the wall and the cross
    hardly does: across the swing the wall falls to 0.39 of its brightness and
    the arrow to 0.39 with it, while the cross keeps 0.86. §6's "faintly
    phosphorescent" is that one number, it is per mark rather than per room, and
    it does not change as the light does — a material that glows glows the same
    whatever the lamp is doing. *)

open Camlcast

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
  Image.make ~width:size (fun ~u ~v ->
      if f u v then (chalk, 255) else Image.clear)

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
    {!Camlcast_core.Room.type-decal} with no [glow], lit by the room like the
    wall it is on, so it goes into the dark with everything else. The {b cross}
    is the phosphorescent kind. Mark a wall with one of each, let the lamp go
    down, and one of them is still there.

    A constant, and not something that rises as the light fails: a material that
    glows glows the same whatever else is happening, and a glow tuned to cancel
    the lamp exactly would leave the mark pinned at one brightness, which reads
    as a sticker rather than a surface. *)
let symbols = [| ("arrow", arrow, 0.); ("cross", cross, 0.7) |]

(** {1 The world} *)

type mark = {
  wall : string;  (** which wall, by the name the description gave it *)
  along : float;
  z : float;
  facing : side;
  symbol : int;
}
(** One chalk stroke.

    The old version of this kept a room and a wall by {e index}, and said so:
    "the only terms that survive a room being rebuilt". A description has better
    terms. A wall has a name here because the description gave it one, and a
    name survives a great deal more than an index does — including the rooms
    being written down in another order, which an index does not. *)

let flat = Plane.horizontal 0.
let hall_sw = Vec.make (-6.) (-5.)
let hall_se = Vec.make 6. (-5.)
let hall_ne = Vec.make 6. 5.
let hall_nw = Vec.make (-6.) 5.
let back_sw = Vec.make 0. (-4.)
let back_se = Vec.make 7. (-4.)
let back_ne = Vec.make 7. 4.
let back_nw = Vec.make 0. 4.
let width = 2.2

(* Not `opening`: a local open of P puts P.opening in scope, and how tall a
   doorway is and where its ends land are two different things. *)
let clearance = 2.8

(** The two ends of each doorway, worked out once so the jambs either side can
    be written as walls of their own — which is what lets them be chalked, since
    a handler goes on a wall and {!Camlcast.P.doorway} keeps its jambs to
    itself. *)
let hall_gate = P.opening ~width hall_se hall_ne

let back_gate = P.opening ~width back_nw back_sw
let this_demos_lintel : lintel = { top = height; material = Surfaces.brick }

(** The marks on one wall, oldest first, so the newest ends up on top of the
    pile — which is the order {!Camlcast.Aim} reads them back in.

    Each takes the [glow] its symbol was given, so the two kinds behave
    differently in the same room and under the same lamp. Nothing here depends
    on the brightness: the glow is a property of the chalk, and the lamp is a
    property of the air. *)
let chalked ~marks name =
  List.filter_map
    (fun m ->
      if m.wall <> name then None
      else
        let _, image, glow = symbols.(m.symbol) in
        Some
          (P.decal ~facing:m.facing ~glow ~along:m.along ~z:m.z ~half_width:0.22
             ~half_height:0.22 image))
    (List.rev marks)

(** How bright the lamp is: a slow swing, never all the way out. *)
let lamp elapsed =
  0.35
  +. (0.65 *. ((1. +. cos (elapsed /. lamp_period *. 2. *. Float.pi)) /. 2.))

(** The air at that brightness, and the whole of the lamp: fog closing in and
    less of everything reaching you. Nothing else in the room changes with it.
*)
let air ~lamp =
  Atmosphere.make ~haze:(Color.rgb 18 18 24)
    ~fog_distance:(2.5 +. (11. *. lamp))
    ~min_brightness:(0.05 +. (0.2 *. lamp))
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:(0.3 +. (0.3 *. lamp))
    ~directional:0.4 ()

(** This demo's rule about what may be chalked, and the whole of it: a wall, in
    the room you are standing in, within {!reach}, with a stroke left.

    An {!Camlcast.Aim.spot} carries the [distance] already — it is how far the
    ray went to find what it found — so "too far away" costs a comparison and no
    engine support at all. *)
let markable ~left (spot : Aim.spot) =
  match spot.Aim.where with
  | Aim.On_wall _ ->
      spot.Aim.crossed = 0 && spot.Aim.distance <= reach && left > 0
  | _ -> false

(** Why the crosshair's target cannot be chalked, in a word, or [None] where it
    can be or where saying so would not help. Only the reasons the player can do
    something about are worth a word: walk closer, or turn round and use one of
    your own walls. *)
let refusal ~left (aim : Aim.spot option) =
  match aim with
  | _ when left = 0 -> Some "no chalk left"
  | Some ({ Aim.where = Aim.On_wall _; _ } as spot) ->
      if spot.Aim.crossed > 0 then Some "another room"
      else if spot.Aim.distance > reach then Some "too far"
      else None
  | _ -> None

(* Shared with the menu, and with nothing to teach here: this demo is about
   marking a wall, and the words over it are only there to say why it will not.
   Text still builds its own, because there the font is the lesson. *)
let panel ~selected ~left ~aim ~font ~across ~down =
  let can = match aim with Some spot -> markable ~left spot | None -> false in
  let color =
    if can then Color.rgb 236 233 222
    else
      match aim with
      | Some _ -> Color.rgb 215 165 90
      | None -> Color.rgb 150 150 155
  in
  match font with
  | None -> [ P.crosshair ~color () ]
  | Some font ->
      let pad = 6 in
      let name, _, glow = symbols.(selected) in
      let line =
        Printf.sprintf "%s  glow %.2f   %d of %d strokes left" name glow left
          strokes
      in
      let tw, th = Font.measure font line in
      P.(
        [ crosshair ~color () ]
        (* A word under the crosshair when there is something to be done about
           it. *)
        @ (match refusal ~left aim with
          | None -> []
          | Some why ->
              let w, _ = Font.measure font why in
              [
                text ~font
                  ~x:((across - w) / 2)
                  ~y:((down / 2) + (2 * pad))
                  ~color:(Color.rgb 215 165 90) why;
              ])
        @ [
            rect ~x:pad
              ~y:(down - th - (3 * pad))
              ~w:(tw + (2 * pad))
              ~h:(th + (2 * pad))
              ~color:(Color.rgb 14 16 24) ~alpha:190 ();
            text ~font ~x:(2 * pad)
              ~y:(down - th - (2 * pad))
              ~color:(Color.rgb 236 233 222) line;
          ])

let at ~marks ~selected ~left ~elapsed ~aim ~mark ~font ~viewport:(across, down)
    =
  (* A wall that can be chalked: named, so a mark can say which one it is on,
     and told, so it can take one. *)
  let chalkable ?(material = Surfaces.stone) ?(tall = height) name a b =
    P.wall ~key:name ~height:tall ~material ~decals:(chalked ~marks name)
      ~on_use:(fun spot ->
        match spot.Aim.where with
        | Aim.On_wall { along; z; facing; _ } when markable ~left spot ->
            mark { wall = name; along; z; facing; symbol = selected }
        | _ -> ())
      a b
  in
  let hall_p, hall_q = hall_gate and back_p, back_q = back_gate in
  P.(
    world
      ~atmosphere:(air ~lamp:(lamp elapsed))
      ~spawn:("hall", Vec.make (-4.) (-2.))
      [
        room ~name:"hall"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          [
            chalkable "south" hall_sw hall_se;
            chalkable "north" hall_ne hall_nw;
            chalkable "west" hall_nw hall_sw;
            chalkable ~material:Surfaces.brick "jamb-south" hall_se hall_p;
            chalkable ~material:Surfaces.brick "jamb-north" hall_q hall_ne;
            threshold ~name:"onward" ~height:clearance ~lintel:this_demos_lintel
              hall_p hall_q;
            (* The one wall with two faces you can get to. Chalk it and walk
               round an end: the far side is bare. It is opaque, and has to be —
               a bare wall you can see through is one Sight looks through, so
               the crosshair never lands on it and the first mark could never be
               placed. *)
            chalkable ~material:Surfaces.panel ~tall:2.4 "partition"
              (Vec.make (-1.5) 1.) (Vec.make 2.5 1.);
          ];
        room ~name:"back"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.horizontal height) ~material:Surfaces.soffit)
          [
            chalkable "back-south" back_sw back_se;
            chalkable ~material:Surfaces.brick "back-east" back_se back_ne;
            chalkable "back-north" back_ne back_nw;
            chalkable ~material:Surfaces.brick "back-jamb-north" back_nw back_p;
            chalkable ~material:Surfaces.brick "back-jamb-south" back_q back_sw;
            threshold ~name:"here" ~height:clearance ~lintel:this_demos_lintel
              back_p back_q;
            sprite ~key:"figure" ~size:1.7 ~image:Pictures.figure
              (Vec.make 4. 0.);
          ];
        link ("hall", "onward") ("back", "here");
        hud (panel ~selected ~left ~aim ~font ~across ~down);
      ])

let world_of ~marks ~selected ~left ~elapsed =
  at ~marks ~selected ~left ~elapsed ~aim:None
    ~mark:(fun _ -> ())
    ~font:None ~viewport:Events.still.Events.viewport

let world =
  (Mount.build (world_of ~marks:[] ~selected:0 ~left:strokes ~elapsed:0.))
    .Scene.world

let marking =
  Element.declare ~name:"marking" @@ fun () ->
  let marks, set_marks = Hook.use_state [] in
  let selected, set_selected = Hook.use_state 0 in
  let elapsed, set_elapsed = Hook.use_state 0. in
  Events.use_frame (fun ~dt -> set_elapsed (elapsed +. dt));
  Events.use_key_down Key.k1 (fun () -> set_selected 0);
  Events.use_key_down Key.k2 (fun () -> set_selected 1);
  at ~marks ~selected
    ~left:(strokes - List.length marks)
    ~elapsed ~aim:(Events.use_aim ())
    ~mark:(fun m -> set_marks (m :: marks))
    ~font:(Some (Lazy.force Typeface.font))
    ~viewport:(Events.use_viewport ())

let run window = Run.on window ~bindings:Bindings.escapable (marking ())
