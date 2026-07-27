(** {b Marking a wall.} Aim at one, press {b C}, and a chalk symbol is on it —
    where the crosshair was, on the face you were looking at, and there for the
    rest of the run.

    {!Raycaster.Sight.cast} already said what the crosshair is on. A wall hit
    reports four numbers, and they are exactly the four a
    {!Raycaster.Room.type-decal} is made of: [index] is which wall,
    [along] and [z] are that wall's own coordinates, and [facing] is the face
    being looked at. Nothing is converted between the two ends — the mark is put
    where the crosshair was because those are the same numbers.

    {b Marks are on one side.} A chalk stroke is paint, and paint is on one face
    of a wall. The free-standing partition across the middle of the hall is the
    only place you can check that: mark it, walk round an end, and there is
    nothing on the back. The rest of the hall is its own boundary, and the far
    face of that is not somewhere you can get to.

    {b Marks survive the room being rebuilt.} The lamp in this room dims and
    brightens, and it does it by building the room again from its parts with
    different materials — walls, floor, ceiling, all new values, every few
    seconds. The chalk is not part of that room. It is a list this demo keeps,
    re-applied with {!Raycaster.Room.add_decal} after the rebuild, which is the
    only thing that makes it persistent: the engine has no memory of a mark, and
    a game that dropped its list would lose them all.

    {b What is the game's, not the engine's.}

    - {b Eight strokes.} How many marks there may be is a rule with no engine
      opinion behind it. The counter is in this file.
    - {b Two symbols,} an arrow and a cross, on {b 1} and {b 2}. What a mark
      means is the player's business and nothing else here reads it.
    - {b Your own room only.} You can see a wall through the doorway and
      {!Raycaster.Sight} will name it, but chalking it would be reaching through
      a wall. {!markable} is where "not through a doorway" is written down, in
      one line, off [crossed].

    The crosshair says which of those you are up against: {b white} for nothing,
    {b amber} for a wall you cannot mark, and {b chalk} for one you can.

    {b Watch what the lamp does to the chalk.} It dims — a decal is lit by
    exactly the same orientation-and-fog factor as the wall under it, so it
    fades with distance and with the air like everything else drawn. But it dims
    {e less} than the wall does, and the gap widens as the light fails. Measured
    off this room, from full lamp to lowest: the mark falls to about four tenths
    of its brightness and the wall to about one tenth, so what was twice as
    bright as its background ends up five times as bright.

    That is not a special case for chalk. The lamp here does two things — it
    closes the {!Raycaster.Atmosphere} in, and it re-tints every
    {!Raycaster.Material} in the room — and only the first of them reaches a
    decal. A {!Raycaster.Image} carries its own colours where a
    {!Raycaster.Texture} takes them from the material wearing it, which is why a
    poster on a red wall is not red. So the wall goes down twice over and the
    mark goes down once, and a mark left in a dying room is the last thing still
    legible in it. *)

open Raycaster
open Result_ext
open Tsdl

let height = 3.6
let strokes = 8
let lamp_period = 9.

(** {1 The symbols}

    Two marks, drawn cut out against {!Raycaster.Image.clear} so a stroke is a
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

let symbols = [| ("arrow", arrow); ("cross", cross) |]

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
    {!Raycaster.Room.type-decal} — a decal belongs to the room it is on, and
    this list has to outlive several of those. *)

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

(** The hall, dressed for a given lamp brightness.

    Every wall — the jambs of the doorway included — both planes and the
    partition are built again here. That is the point: nothing in this room is
    the value it was a moment ago, so a mark that is still on the wall
    afterwards is one that was put back.

    The doorway is cut here rather than once outside, so that its jambs dim with
    the rest of the room instead of staying lit while everything around them
    goes out. Its {!Raycaster.Room.type-threshold} comes out the same every time
    — same name, same endpoints, same height — which is exactly what
    {!Raycaster.World.replace_room} insists on, since a portal was derived from
    those. *)
let hall ~lamp =
  let shade (m : Material.t) =
    Material.make ~color:(Color.shade m.Material.color lamp)
      ~pattern:m.Material.pattern
  in
  let jambs, onward =
    Room.doorway ~name:"onward" ~width:2.2 ~opening:2.8 ~height
      ~material:(shade Surfaces.brick) hall_se hall_ne
  in
  let wall material a b = Room.wall ~height ~material:(shade material) a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ onward ]
    ~floor:{ Room.plane = floor; material = shade Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.above floor height; material = shade Surfaces.soffit })
    (jambs
    @ [
        wall Surfaces.stone hall_sw hall_se;
        wall Surfaces.stone hall_ne hall_nw;
        wall Surfaces.stone hall_nw hall_sw;
        (* The one wall with two faces you can get to. Chalk it and walk round
           an end: the far side is bare. It is opaque, and has to be — a wall
           you can see through is one {!Raycaster.Sight} looks {e through},
           so it is never what the crosshair is on and never something to
           mark. *)
        Room.wall ~height:2.4 ~material:(shade Surfaces.panel)
          (Vec.make (-1.5) 1.) (Vec.make 2.5 1.);
      ])

let back =
  Room.make ~thresholds:[ back_here ]
    ~floor:{ Room.plane = Plane.horizontal 0.; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         {
           Room.plane = Plane.horizontal height;
           material = Surfaces.soffit;
         })
    ~sprites:[ Room.sprite ~size:1.7 ~image:Pictures.figure (Vec.make 4. 0.) ]
    (back_jambs
    @ [
        Room.wall ~height ~material:Surfaces.stone back_sw back_se;
        Room.wall ~height ~material:Surfaces.brick back_se back_ne;
        Room.wall ~height ~material:Surfaces.stone back_ne back_nw;
      ])

let world =
  World.make
    ~rooms:[ ("hall", hall ~lamp:1.); ("back", back) ]
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
    standing in, with a stroke left. Everything before the [when] is
    {!Raycaster.Sight}'s answer; everything in it is this demo's rule. *)
let markable state = function
  | Some { Sight.kind = Sight.Wall w; room; crossed; _ }
    when crossed = 0 && state.left > 0 ->
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
  0.35 +. (0.65 *. ((1. +. cos (elapsed /. lamp_period *. 2. *. Float.pi)) /. 2.))

(** The air at that brightness. A failing lamp is fog closing in and less of
    everything reaching you, so it is {!Raycaster.Atmosphere} that changes and
    not only the surfaces.

    This is the half of the lamp that reaches the chalk. The renderer multiplies
    a {!Raycaster.Room.type-decal} by the same orientation-and-fog factor it
    multiplies the wall by, so a mark goes down with the light exactly as the
    wall it is on does. Walk away from one and watch it fade — that much is true
    whatever the lamp is doing, since it is only distance. *)
let air ~lamp =
  Atmosphere.make
    ~haze:(Color.rgb 18 18 24)
    ~fog_distance:(2.5 +. (11. *. lamp))
    ~min_brightness:(0.05 +. (0.2 *. lamp))
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:(0.3 +. (0.3 *. lamp))
    ~directional:0.4

(** The world as it stands: the hall built again at this moment's brightness, in
    air of that brightness, then every mark put back onto it.

    The lamp does two separate things here and they are worth telling apart. It
    re-tints every {!Raycaster.Material} in the room, which is what forces the
    {b rebuild} — and so is what makes the marks surviving worth showing. And it
    closes the {b atmosphere} in, which is what dims the marks along with
    everything else. Neither alone would do: an atmosphere that changed on its
    own would never rebuild the room, and materials that changed on their own
    would leave the chalk conspicuously bright in a dark room.

    Oldest mark first, so the newest ends up on top of the pile — which is the
    order {!Raycaster.Room.add_decal} appends in and the order
    {!Raycaster.Sight} reads back. *)
let dressed state =
  let brightness = lamp state.elapsed in
  let lit =
    World.replace_room world ~room:0 ~replacement:(hall ~lamp:brightness)
  in
  let lit = { lit with World.atmosphere = air ~lamp:brightness } in
  List.fold_left
    (fun w m ->
      let _, image = symbols.(m.symbol) in
      World.replace_room w ~room:m.room
        ~replacement:
          (Room.add_decal (World.room w m.room) ~wall:m.wall
             (Room.decal ~facing:m.facing ~along:m.along ~z:m.z ~half_width:0.22
                ~half_height:0.22 image)))
    lit (List.rev state.marks)

(** Chalk whatever the crosshair is on, or do nothing if it is not something
    this demo will mark.

    The sighting is taken against the world as it is {e drawn}, marks and all,
    rather than against the authored one — so what can be marked is what can be
    seen, including a wall that a mark already on it does not hide. Split out of
    {!update} because the test suite drives it: pressing a key is SDL's, but
    what pressing it does is the rule worth asserting. *)
let place state =
  match markable state (Sight.cast (dressed state) state.player) with
  | Some mark -> { state with marks = mark :: state.marks; left = state.left - 1 }
  | None -> state

let update state ~dt ~motion ~actions =
  let player = Engine.step world state.player motion in
  let selected =
    if Input.pressed actions (Input.Key Sdl.Scancode.k1) then 0
    else if Input.pressed actions (Input.Key Sdl.Scancode.k2) then 1
    else state.selected
  in
  let state = { state with player; selected; elapsed = state.elapsed +. dt } in
  if Input.pressed actions (Input.Key Sdl.Scancode.c) then place state else state

let view state = (dressed state, state.player)

(** {1 The overlay} *)

let font =
  lazy
    (match
       let* path = Asset.path "assets/font.png" in
       let+ atlas = Image.load path in
       Font.make ~fallback:'\127' ~atlas ~width:6 ~height:10 ~first:32 ()
     with
    | Ok font -> font
    | Error (`Msg m) -> failwith ("the chalk demo could not read its font: " ^ m))

let overlay fb state =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let seen = Sight.cast (dressed state) state.player in
  let can = markable state seen <> None in
  let r, g, b =
    if can then (236, 233, 222)
    else match seen with Some _ -> (215, 165, 90) | None -> (150, 150, 155)
  in
  Paint.crosshair fb ~r ~g ~b;
  let font = Lazy.force font in
  let pad = 6 in
  let name, _ = symbols.(state.selected) in
  let line =
    Printf.sprintf "%s   %d of %d strokes left" name state.left strokes
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
  let help = "C to mark   1 arrow   2 cross" in
  let hw, _ = Font.measure font help in
  Font.draw fb font help
    ~x:(width - hw - pad)
    ~y:pad
    ~color:(Color.rgb 140 146 160)

let run () =
  let+ _ = Engine.run_state ~update ~view ~overlay start in
  ()
