(** {b Looking through a doorway.} What the crosshair is on, named — including
    when it is in the room next door.

    {!Raycaster.Sight.cast} traces the middle of the view through one open
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
    not take them. That rule is the demo's, not the engine's: {!Raycaster.Sight}
    reports how many doorways it looked through and {!collectable} is where the
    "at least one" is written down. The engine has no notion of a thing worth
    collecting, only of the sprite that happens to be one.

    Whatever is targeted is {b ringed}, from the same numbers the renderer drew
    it with — so the ring lands on it exactly, even through the doorway and in
    the far room's own coordinates. {!Raycaster.Sight.t} carries the pose to work
    that out from.

    There is a {b picture hung on the far wall} too. Aim at it and the crosshair
    says so: a wall hit reports which of its decals is under the crosshair, by
    the same rule that drew it, alpha and all. Aim at the wall an inch beside the
    frame and it is a bare wall again.

    The two rings are not the same shape, and that is the point. A sprite faces
    you, so it rings as a rectangle. A picture is flat on a wall, and a wall
    recedes — so its far edge is shorter than its near one and the ring is a
    trapezoid. Stand square on to the picture and it squares up; step to one side
    and watch it lean.

    Two more things worth trying. Walk so one barrel is behind another — the near
    one wins, and the far one cannot be taken. And aim at the gap between a
    barrel's outline and the corner of its box: the crosshair goes white, because
    a sprite is a cut-out and the pick is asked of the texel rather than the
    box. *)

open Raycaster
open Result_ext
open Tsdl

let height = 4.

(** Clearer air than the other demos use. What is being looked at here is in the
    next room and a good way off, and the point is to be able to see it. *)
let air =
  Atmosphere.make
    ~haze:(Color.rgb 26 26 34)
    ~fog_distance:22. ~min_brightness:0.4
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:0.65 ~directional:0.35

type t = {
  player : Player.t;
  collected : (int * int) list;  (** room and sprite, of each one recorded *)
}

let barrel pos = Room.sprite ~size:1.2 ~image:Pictures.barrel pos

let world =
  let near_jambs, east =
    Room.doorway ~name:"east" ~width:2.6 ~opening:2.8 ~height
      ~material:Surfaces.brick (Vec.make 6. (-6.)) (Vec.make 6. 6.)
  and far_jambs, west =
    Room.doorway ~name:"west" ~width:2.6 ~opening:2.8 ~height
      ~material:Surfaces.stone (Vec.make 0. 6.) (Vec.make 0. (-6.))
  in
  let wall material a b = Room.wall ~height ~material a b in
  let floor = Plane.horizontal 0. in
  let surfaces plane =
    ( { Room.plane; material = Surfaces.ground },
      Room.Roof
        { Room.plane = Plane.above plane height; material = Surfaces.soffit } )
  in
  let ground, roof = surfaces floor in
  let near =
    Room.make ~thresholds:[ east ] ~floor:ground ~ceiling:roof
      ~sprites:
        [ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2. 3.5) ]
      (near_jambs
      @ [
          wall Surfaces.brick (Vec.make (-6.) (-6.)) (Vec.make 6. (-6.));
          wall Surfaces.brick (Vec.make 6. 6.) (Vec.make (-6.) 6.);
          wall Surfaces.brick (Vec.make (-6.) 6.) (Vec.make (-6.) (-6.));
        ])
  and hung =
    (* A picture on the far room's end wall, square in the doorway's view. *)
    Room.wall ~height ~material:Surfaces.stone (Vec.make 9. (-6.))
      (Vec.make 9. 6.)
      ~decals:
        [
          Room.decal ~along:6. ~z:1.6 ~half_width:1. ~half_height:1.
            Pictures.painting;
        ]
  and sidelong =
    (* And one down the far room's side wall, which the doorway only ever shows
       you at an angle — the ring round this one is a trapezoid. *)
    Room.wall ~height ~material:Surfaces.stone (Vec.make 0. (-6.))
      (Vec.make 9. (-6.))
      ~decals:
        [
          Room.decal ~along:6.5 ~z:1.7 ~half_width:1. ~half_height:1.
            Pictures.poster;
        ]
  in
  let far =
    Room.make ~thresholds:[ west ] ~floor:ground ~ceiling:roof
      (* Three of them, spread across the doorway's view: one square on, one to
         each side, so turning the head picks a different one. *)
      ~sprites:
        [
          barrel (Vec.make 3. 0.); barrel (Vec.make 4. 2.5);
          barrel (Vec.make 4. (-2.5));
        ]
      (far_jambs
      @ [ sidelong; hung; wall Surfaces.stone (Vec.make 9. 6.) (Vec.make 0. 6.) ])
  in
  World.make
    ~rooms:[ ("near", near); ("far", far) ]
    ~links:[ (("near", "east"), ("far", "west")) ]
    ~atmosphere:air
    ~spawn:("near", Vec.make 1. 0.)

let start = { player = Player.spawn world; collected = [] }

(** This demo's rule about what may be recorded, and the whole of it: a sprite,
    in a room the eye reached through at least one doorway, that has not been
    recorded already.

    The [crossed] test is the "from safety" part — you may study the next room
    without standing in it, and what you are already standing among does not
    count. Nothing in the engine says so. *)
let collectable state (seen : Sight.t option) =
  match seen with
  | Some { Sight.kind = Sight.Sprite s; room; crossed; _ }
    when crossed >= 1 && not (List.mem (room, s.index) state.collected) ->
      Some (room, s.index)
  | _ -> None

let update state ~dt:_ ~motion ~actions =
  let player = Engine.step world state.player motion in
  let state = { state with player } in
  match
    (Input.pressed actions (Input.Key Sdl.Scancode.e), Sight.cast world player)
  with
  | true, seen -> (
      match collectable state seen with
      | Some what -> { state with collected = what :: state.collected }
      | None -> state)
  | false, _ -> state

(** The corners of the target on the screen, joined up, if there is one worth
    ringing.

    Both cases rebuild the viewport the frame was drawn with — the same window
    size, the same pitch, the same eye — and both take the pose from the
    sighting, so a thing in the room next door is placed in {e that} room's
    coordinates and still lands where it was drawn.

    A {b sprite} is square to the view, so
    {!Raycaster.Viewport.sprite_box} gives it outright and the ring is a
    rectangle. A {b decal} is flat on a wall, so it is not: a wall recedes, and
    the far edge of a picture on it is smaller than the near one. What holds is
    that its four corners project to four points and the straight edges between
    them stay straight, so the ring is the trapezoid through those. *)
let ringed fb (state : t) (seen : Sight.t option) =
  let here = World.room world state.player.Player.room in
  let viewport =
    Viewport.create ~pitch:state.player.Player.pitch
      ~eye_z:
        (Plane.elevation here.Room.floor.Room.plane state.player.Player.pos
        +. Config.eye_height)
      ~width:fb.Framebuffer.width ~height:fb.Framebuffer.height
  in
  let whole = List.filter_map Fun.id in
  match seen with
  | Some { Sight.kind = Sight.Sprite s; room; pose; distance; _ } ->
      let there = World.room world room in
      let sprite = there.Room.sprites.(s.index) in
      let left, top, right, bottom =
        Viewport.sprite_box viewport pose
          ~floor_z:(Plane.elevation there.Room.floor.Room.plane sprite.Room.pos)
          ~distance sprite
      in
      Some [ (left, top); (right, top); (right, bottom); (left, bottom) ]
  | Some { Sight.kind = Sight.Wall { index; decal = Some d; _ }; room; pose; _ }
    ->
      let there = World.room world room in
      let wall = there.Room.walls.(index) in
      let decal = List.nth wall.Room.decals d in
      (* Where along the wall the picture starts and stops, as points on it. *)
      let at along =
        Vec.add wall.Room.a
          (Vec.scale wall.Room.edge (along /. wall.Room.length))
      in
      let near = at (decal.Room.along -. decal.Room.half_width)
      and far = at (decal.Room.along +. decal.Room.half_width) in
      (* A decal hangs above the floor under the wall, so on a sloped one its
         two ends are at different elevations — measured at each end, not once. *)
      let corner point up =
        let foot = Plane.elevation there.Room.floor.Room.plane point in
        Viewport.project_point viewport pose ~point
          ~z:
            (foot +. decal.Room.z
            +. (up *. decal.Room.half_height))
      in
      let corners =
        whole [ corner near 1.; corner far 1.; corner far (-1.); corner near (-1.) ]
      in
      if List.length corners = 4 then Some corners else None
  | _ -> None

let overlay fb state =
  let height = fb.Framebuffer.height in
  let unit = Int.max 3 (height / 60) in
  let seen = Sight.cast world state.player in
  let r, g, b =
    match (collectable state seen, seen) with
    | Some _, _ -> (120, 230, 130)
    | None, Some { Sight.kind = Sight.Wall { decal = Some _; _ }; _ } ->
        (215, 130, 235)
    | None, Some { Sight.kind = Sight.Doorway _; _ } -> (120, 170, 240)
    | None, Some _ -> (235, 195, 100)
    | None, None -> (245, 245, 245)
  in
  (* Round the target, wherever the renderer put it. *)
  Option.iter
    (fun corners ->
      Paint.ring fb
        (List.map
           (fun (x, y) ->
             (int_of_float (Float.round x), int_of_float (Float.round y)))
           corners)
        ~r ~g ~b)
    (ringed fb state seen);
  Paint.crosshair fb ~r ~g ~b;
  (* One tick per barrel recorded. *)
  List.iteri
    (fun i _ ->
      Paint.rect fb
        ~x:((2 * unit) + (i * 3 * unit))
        ~y:(height - (5 * unit))
        ~w:(2 * unit) ~h:(3 * unit) ~r:120 ~g:230 ~b:130 ~alpha:255)
    state.collected

let run () =
  let+ _ =
    Engine.run_state ~update
      ~view:(fun state -> (world, state.player))
      ~overlay start
  in
  ()
