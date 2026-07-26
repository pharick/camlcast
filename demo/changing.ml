(** {b Replacing a room.} A room is immutable, so a room that changes is a room
    that is built again.

    {!Raycaster.World.replace_room} puts a new version of a room where the old
    one was. It insists the openings are untouched — same thresholds, same
    order, same names, endpoints and heights, because those are what every
    portal in the world was derived from or indexes into — and lets everything
    else become anything: walls, decals, floor and ceiling planes, sprites, and
    whether a leaf hangs in a doorway.

    Here the room is rebuilt every frame, in [view], and nothing is stored:

    - the sign on the far wall slides along it and swaps between two pictures,
      which is what an animated sign is — a decal moved and a frame changed;
    - the panel it hangs on cycles through the materials;
    - the barrel drifts, so the sprites change too;
    - the floor rises and falls a little, because a floor is a plane like any
      other and nothing outside the room depends on it.

    The world it all happens in is never modified. [view] hands the renderer a
    world that was made for that one frame and is dropped after it, which is why
    [update] can stay a pure function of a clock and a pose.

    Rebuilding a whole room every frame is what this costs. It is cheap here —
    one room, six walls — and a game with a hundred rooms would replace only the
    ones with something moving in them. *)

open Raycaster
open Result_ext

let height = 4.
let period = 6.

type t = { elapsed : float; player : Player.t }

(* The wall the sign hangs on, which is the one you are facing when you arrive.
   Its endpoints never move: only what is painted on it does. *)
let sign_wall_a = Vec.make 7. (-6.)
let sign_wall_b = Vec.make 7. 6.

let sw = Vec.make (-7.) (-6.)
let se = Vec.make 7. (-6.)
let ne = Vec.make 7. 6.
let nw = Vec.make (-7.) 6.

(** The room as it stands at [phase], a fraction of the way round the cycle. *)
let room ~phase =
  let turn = phase *. 2. *. Float.pi in
  let coats = [| Surfaces.brick; Surfaces.panel; Surfaces.stone; Surfaces.tile |] in
  let coat = coats.(int_of_float (phase *. 4.) mod 4) in
  (* Two pictures alternating is a two-frame animation; the slide along the
     wall is the same decal placed somewhere else. *)
  let sign =
    {
      Room.along = 6. +. (3.5 *. sin turn);
      z = 1.8 +. (0.25 *. sin (turn *. 2.));
      half_width = 0.9;
      half_height = 0.9;
      image =
        (if Float.rem (phase *. 6.) 1. < 0.5 then Pictures.painting
         else Pictures.poster);
    }
  in
  let floor = Plane.horizontal (0.3 *. sin turn) in
  let wall material a b = Room.wall ~height ~material a b in
  Room.make
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.horizontal (height +. 0.5); material = Surfaces.soffit })
    ~sprites:
      [
        { Room.pos = Vec.make 2. (2. *. sin turn); size = 0.9; image = Pictures.barrel };
        { Room.pos = Vec.make 4. (-3.); size = 1.8; image = Pictures.figure };
      ]
    [
      wall Surfaces.stone sw se;
      Room.wall ~height ~material:coat sign_wall_a sign_wall_b ~decals:[ sign ];
      wall Surfaces.stone ne nw;
      wall Surfaces.stone nw sw;
    ]

(** The world the demo starts from, and the one every frame is a replacement
    into. Its one room is the cycle at rest. *)
let world =
  World.make
    ~rooms:[ ("room", room ~phase:0.) ]
    ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-4.5) 0.)

let start = { elapsed = 0.; player = Player.spawn world }

let update state ~dt ~motion ~actions:_ =
  {
    elapsed = Float.rem (state.elapsed +. dt) period;
    player = Engine.step world state.player motion;
  }

(* The player walks in [world], whose floor is flat and whose walls never move,
   so collision is against the room as authored. Only what is drawn changes. *)
let view state =
  let phase = state.elapsed /. period in
  ( World.replace_room world ~room:0 ~replacement:(room ~phase),
    state.player )

let run () =
  let+ _ = Engine.run_state ~update ~view start in
  ()
