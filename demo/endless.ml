(** {b Growing a world.} A corridor built ahead of the player as they walk.

    A world that grows is a description with more in it than last frame, and
    nothing besides. One number of state says how many segments have been built;
    {!Camlcast.Events.use_crossings} says which doorways the previous frame went
    through, so the deepest of them is how far the player has got; when that
    comes close enough to the end, the number goes up and the next description
    has another segment.

    This is the clearest case in the demos of the layer paying for itself. Grown
    against the platform, a world of this shape is surgery on one:
    {!Camlcast_core.World.open_doorway} to give a dead end a way on,
    {!Camlcast_core.World.add_room} for what lies beyond it,
    {!Camlcast_core.World.link} to join the two, and a search for whether a room
    already has a way on so none of it is done twice — every step appending and
    never moving, so held indices stay valid. None of that is here. Segments
    described again are matched against last frame's and kept, the new one is
    mounted, and the indices are whatever this frame's assembly produced.

    It builds {!Camlcast_core.Config.max_portal_depth} segments ahead — exactly
    as deep as the renderer looks through doorways — so the end of the corridor
    is never in shot. Segments alternate brick and stone to make distance
    countable. *)

open Camlcast

let height = 4.
let width = 2.5
let depth = 9.

(** How far ahead of the player the corridor is kept. The renderer looks through
    {!Camlcast.Config.max_portal_depth} doorways and no further, so building
    that many beyond the player is exactly enough to keep the end out of shot.
*)
let ahead = Config.max_portal_depth

let named index = Printf.sprintf "segment-%d" index

(** The opening between segment [index] and the one after it, as a door on each
    side.

    Kept in a table because this corridor has no last segment: a door carries
    the identity its connection is joined by, and a description rebuilt every
    frame must hand back the same door for the same gap or the world would be
    re-joined from scratch each time. Where a level with a fixed shape makes its
    doors at the top level, one that grows makes them once each, on the frame
    that first needs them, and remembers them by the same number it names its
    segments by. *)
let ways : (int, P.door * P.door) Hashtbl.t = Hashtbl.create 16

let way index =
  match Hashtbl.find_opt ways index with
  | Some pair -> pair
  | None ->
      let pair =
        ( P.door ~name:"on" ~width:2.2 ~clearance:3. (),
          P.door ~name:"back" ~width:2.2 ~clearance:3. () )
      in
      Hashtbl.add ways index pair;
      pair

let index_of name =
  match String.index_opt name '-' with
  | Some dash ->
      int_of_string_opt
        (String.sub name (dash + 1) (String.length name - dash - 1))
  | None -> None

(** One segment: a rectangle with a doorway back and, unless it is the last one
    built, another one on.

    The coat is taken from the segment's own number rather than from a counter,
    so a segment that grows a way on keeps the colour it had a moment ago rather
    than changing as the player steps in. *)
let segment ~index ~back ~onward =
  (* Each segment runs east, the arrival facing direction, so the corridor is
     straight ahead from the start. *)
  let sw = Vec.make 0. (-.width)
  and se = Vec.make depth (-.width)
  and ne = Vec.make depth width
  and nw = Vec.make 0. width in
  let coat = if index mod 2 = 0 then Surfaces.brick else Surfaces.stone in
  P.(
    (* A leg with no door cut into it is left whole, so a dead end needs no
       wall written for it: the segment that has not grown a way on yet is the
       same outline as the one that has. *)
    room ~name:(named index) ~height ~material:coat
      ~floor:
        (if back then floor Surfaces.ground
         else floor ~plane:(Plane.horizontal 0.) Surfaces.ground)
      ~ceiling:(roof Surfaces.soffit)
      ~outline:(corners [ sw; se; ne; nw ])
      ((if back then [] else [ spawn (Vec.make 2. 0.) ])
      @ (if onward then [ cut (fst (way index)) ~along:(se, ne) ] else [])
      @ if back then [ cut (snd (way (index - 1))) ~along:(nw, sw) ] else []))

(** The corridor as far as it has been built: one more segment than have been
    walked into, and a link joining each to the next.

    Written out from a number every frame, because a description does not modify
    a world — it says what the world is, and saying it with one more segment
    {e is} growing the corridor. *)
let corridor ~built =
  P.(
    world ~atmosphere:Surfaces.air
      (List.init (built + 1) (fun index ->
           segment ~index ~back:(index > 0) ~onward:(index < built))
      @ List.init built (fun index ->
          let on, back = way index in
          connect on back)))

let walking =
  Element.declare ~name:"walking" @@ fun () ->
  let built, set_built = Hook.use_state ahead in
  let crossings = Events.use_crossings () in
  Events.use_frame (fun ~dt:_ ->
      (* Every doorway the frame went through, because a single step can cross
         several; the deepest is where the player has got to. *)
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
