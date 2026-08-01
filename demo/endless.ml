(** {b Growing a world.} A corridor that does not exist until you walk down it.

    {!Camlcast_core.Engine.run_world} takes an [extend] callback and calls it on
    a frame the player went through a doorway on, with the world and where they
    now are; whatever it returns is the world drawn from then on. It runs on a
    frame that crossed and not on every frame, so a generator may take its time
    — and once per such frame however many doorways it crossed, which is why
    what arrives is a room to build ahead of and not a doorway to build at.

    What it does here is build ahead of the player, using the three primitives a
    world grows by and nothing else:

    - {!Camlcast_core.World.open_doorway} replaces a room with one that has one
      more threshold than it had, the ones it already had unmoved. That check is
      the whole safety of this: a room cannot move a doorway that something else
      is already linked through.
    - {!Camlcast_core.World.add_room} appends a room whose doorways lead nowhere
      yet.
    - {!Camlcast_core.World.link} joins two doorways that both exist and neither
      of which leads anywhere.

    Each appends and nothing else, so every index anything is holding stays
    valid, and each leaves a world that renders and walks — a generator that
    stopped halfway would leave you facing a wall rather than an exception in
    the middle of a frame.

    It builds {!Camlcast_core.Config.max_portal_depth} segments ahead, which is
    exactly as deep as the renderer looks through doorways, so the end of the
    corridor is never in shot. Segments alternate brick and stone so you can
    count how far you have gone. *)

open Camlcast

let height = 4.
let width = 2.5
let depth = 9.

(** How far ahead of the player the corridor is kept. The renderer looks through
    {!Camlcast.Config.max_portal_depth} doorways and no further, so building
    that many beyond wherever they have got to is exactly enough for the end
    never to be in shot. *)
let ahead = Config.max_portal_depth

let named index = Printf.sprintf "segment-%d" index

let index_of name =
  match String.index_opt name '-' with
  | Some dash ->
      int_of_string_opt
        (String.sub name (dash + 1) (String.length name - dash - 1))
  | None -> None

(** One segment: a rectangle with a doorway back the way you came and, unless it
    is the last one built, another one on.

    The coat is taken from the segment's own number rather than from a counter,
    so that a segment which grows a way on is the same colour it was a moment
    ago. A room that changed colour as you stepped into it would be a strange
    thing to watch. *)
let segment ~index ~back ~onward =
  (* Each segment runs east, which is the way you are facing when you arrive in
     it, so the corridor is straight ahead from the moment it starts. *)
  let sw = Vec.make 0. (-.width)
  and se = Vec.make depth (-.width)
  and ne = Vec.make depth width
  and nw = Vec.make 0. width in
  let coat = if index mod 2 = 0 then Surfaces.brick else Surfaces.stone in
  let flat = Plane.horizontal 0. in
  P.(
    room ~name:(named index)
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
      [
        wall ~height ~material:coat sw se;
        (if onward then
           doorway ~name:"on" ~width:2.2 ~opening:3. ~height ~material:coat se
             ne
         else wall ~height ~material:coat se ne);
        wall ~height ~material:coat ne nw;
        (if back then
           doorway ~name:"back" ~width:2.2 ~opening:3. ~height ~material:coat nw
             sw
         else wall ~height ~material:coat nw sw);
      ])

(** The corridor as far as it has been built.

    This is the whole of what used to be a graph walked and surgically extended:
    open_doorway to give a dead end a way on, add_room for what lies beyond it,
    link to join the two, and a search for whether a room already had a way on
    so that none of it was done twice. A description does not need any of that,
    because it does not modify a world — it says what the world is, and saying
    it with one more segment in it {e is} growing the corridor. *)
let corridor ~built =
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:(named 0, Vec.make 2. 0.)
      (List.init (built + 1) (fun index ->
           segment ~index ~back:(index > 0) ~onward:(index < built))
      @ List.init built (fun index ->
          link (named index, "on") (named (index + 1), "back"))))

let walking =
  Element.declare ~name:"walking" @@ fun () ->
  let built, set_built = Hook.use_state ahead in
  let crossings = Events.use_crossings () in
  Events.use_frame (fun ~dt:_ ->
      (* Every doorway the frame went through, because a single step can cross
         several. The deepest of them is where the player has got to. *)
      let deepest =
        List.fold_left
          (fun deepest (c : Events.crossing) ->
            match index_of c.Events.to_room with
            | Some index -> Int.max deepest index
            | None -> deepest)
          0 crossings
      in
      if deepest + ahead > built then set_built (deepest + ahead));
  corridor ~built

let world = (Mount.build (corridor ~built:ahead)).Scene.world
let run window = Run.on window ~bindings:Bindings.escapable (walking ())
