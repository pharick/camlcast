(** {b Growing a world.} A corridor that does not exist until you walk down it.

    {!Camlcast.Engine.run_world} takes an [extend] callback and calls it on a
    frame the player went through a doorway on, with the world and where they
    now are; whatever it returns is the world drawn from then on. It runs on a
    frame that crossed and not on every frame, so a generator may take its time
    — and once per such frame however many doorways it crossed, which is why
    what arrives is a room to build ahead of and not a doorway to build at.

    What it does here is build ahead of the player, using the three primitives a
    world grows by and nothing else:

    - {!Camlcast.World.open_doorway} replaces a room with one that has one more
      threshold than it had, the ones it already had unmoved. That check is the
      whole safety of this: a room cannot move a doorway that something else is
      already linked through.
    - {!Camlcast.World.add_room} appends a room whose doorways lead nowhere yet.
    - {!Camlcast.World.link} joins two doorways that both exist and neither of
      which leads anywhere.

    Each appends and nothing else, so every index anything is holding stays
    valid, and each leaves a world that renders and walks — a generator that
    stopped halfway would leave you facing a wall rather than an exception in
    the middle of a frame.

    It builds {!Camlcast.Config.max_portal_depth} segments ahead, which is
    exactly as deep as the renderer looks through doorways, so the end of the
    corridor is never in shot. Segments alternate brick and stone so you can
    count how far you have gone. *)

open Camlcast

let height = 4.
let width = 2.5
let depth = 9.

(** One segment: a rectangle with a doorway back the way you came and, once
    something has been built beyond it, another one on.

    The coat is taken from the room's own index rather than from a counter, so
    that rebuilding a room to give it a way on rebuilds it exactly as it was —
    [open_doorway] permits the walls to change, but a room that changed colour
    as you stepped into it would be a strange thing to watch. *)
let segment ~index ~back ~onward =
  (* Each segment runs east, which is the way you are facing when you arrive in
     it, so the corridor is straight ahead from the moment it starts. *)
  let sw = Vec.make 0. (-.width)
  and se = Vec.make depth (-.width)
  and ne = Vec.make depth width
  and nw = Vec.make 0. width in
  let coat = if index mod 2 = 0 then Surfaces.brick else Surfaces.stone in
  let cut name a b =
    Room.doorway ~name ~width:2.2 ~opening:3. ~height ~material:coat a b
  in
  let wall a b = Room.wall ~height ~material:coat a b in
  let back_jambs, back_door = cut "back" nw sw
  and on_jambs, on_door = cut "on" se ne in
  let floor = Plane.horizontal 0. in
  Room.make
    ~thresholds:
      (List.concat
         [
           (if back then [ back_door ] else []);
           (if onward then [ on_door ] else []);
         ])
    ~floor:(Room.floor ~plane:floor ~material:Surfaces.ground)
    ~ceiling:
      (Room.roof ~plane:(Plane.above floor height) ~material:Surfaces.soffit)
    (List.concat
       [
         [ wall sw se ];
         (if onward then on_jambs else [ wall se ne ]);
         [ wall ne nw ];
         (if back then back_jambs else [ wall nw sw ]);
       ])

let named index = Printf.sprintf "segment-%d" index

(* Does this room already have a way on, and if so which threshold is it? *)
let way_on (room : Room.t) =
  let rec search index =
    if index >= Room.threshold_count room then None
    else if String.equal (Room.threshold_at room index).Room.name "on" then
      Some index
    else search (index + 1)
  in
  search 0

(** Build [depth] segments beyond [room], following the ones that are there
    already and adding the ones that are not. *)
let rec build world ~room ~ahead =
  if ahead <= 0 then world
  else
    match way_on (World.room world room) with
    | Some index ->
        let portal = Option.get (World.portal world ~room ~threshold:index) in
        build world ~room:portal.World.to_room ~ahead:(ahead - 1)
    | None ->
        (* This room was built as a dead end. Give it a way on — the doorway it
           already has stays exactly where it was — and put a new dead end
           beyond it. *)
        let world =
          World.open_doorway world ~room
            ~opened:(segment ~index:room ~back:true ~onward:true)
        in
        let index = World.room_count world in
        let world, next =
          World.add_room world ~name:(named index)
            (segment ~index ~back:true ~onward:false)
        in
        let world = World.link world (room, "on") (next, "back") in
        build world ~room:next ~ahead:(ahead - 1)

let world =
  (* The first segment has no way back, and the second is where growth begins. *)
  let start =
    World.make
      ~rooms:
        [
          (named 0, segment ~index:0 ~back:false ~onward:true);
          (named 1, segment ~index:1 ~back:true ~onward:false);
        ]
      ~links:[ ((named 0, "on"), (named 1, "back")) ]
      ~atmosphere:Surfaces.air
      ~spawn:(named 0, Vec.make 2. 0.)
  in
  build start ~room:0 ~ahead:Config.max_portal_depth

let extend world (player : Player.t) =
  build world ~room:player.Player.room ~ahead:Config.max_portal_depth

let run window = Engine.run_world window ~extend world
