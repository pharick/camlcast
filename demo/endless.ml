(** {b Growing a world.} A corridor that does not exist until you walk down it.

    A world that grows is a description with more in it than it had last frame,
    and nothing besides. One number of state says how many segments have been
    built; {!Camlcast.Events.use_crossings} says which doorways the frame before
    this one went through, so the deepest of them is how far the player has got;
    and when that comes close enough to the end, the number goes up and the next
    description has another segment in it.

    What that replaced is worth naming, because this is the clearest case in the
    demos of the layer paying for itself. Growing a world used to mean surgery
    on one — {!Camlcast_core.World.open_doorway} to give a dead end a way on,
    {!Camlcast_core.World.add_room} for what lay beyond it,
    {!Camlcast_core.World.link} to join the two, and a search for whether a room
    already had a way on so that none of it was done twice, every step of it
    careful to append and never move so that the indices things were holding
    stayed valid. None of that is here. The segments described again are matched
    against last frame's and kept, the new one is mounted, and the indices are
    whatever assembling this frame's description happened to produce.

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

(** The corridor as far as it has been built: one more segment than have been
    walked into, and a link joining each to the next.

    Written out from a number every frame, because a description does not modify
    a world — it says what the world is, and saying it with one more segment in
    it {e is} growing the corridor. *)
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
let run window = Run.on window ~controls:Bindings.escapable (walking ())
