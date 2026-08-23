(** {b Traversal traces.} Every doorway a step went through, in order.

    {!Camlcast_core.Player.traverse} returns where the player ended up {e and} a
    list of crossings, each naming the room and threshold it left by, the room
    and threshold it arrived at, and the transform applied.
    {!Camlcast_core.Engine.move} is the same for a whole frame, with the turn
    applied first; {!Camlcast_core.Engine.step} is that with the list dropped,
    which every other demo here uses. {!Camlcast_core.Player.crossed} answers
    only whether the list is empty, which is all a world that grows needs.

    A frame can cross more than one doorway: movement resolves its two axes one
    after the other, and each leg can go through an opening of its own — so the
    list and its order are the answer, not a count.

    This demo builds a return route from the crossings. Each crossing is pushed
    onto a stack unless it is the exact reverse of the one on top, which is
    popped instead. Walking east through the doorways accumulates ticks along
    the bottom of the screen; walking back removes them one at a time. The row
    of ticks is the route home.

    Exactness matters. The rooms here are laid out in a line, but the engine
    does not require that: a link is derived from two thresholds and from
    nothing else, so a corridor can return the player somewhere it could not
    physically go. A route home built from the crossings still arrives, because
    it records what was walked rather than guessing from the geometry. *)

open Camlcast

let height = 4.
let width = 2.5
let depth = 8.
let rooms = 5
let named index = Printf.sprintf "chamber-%d" index

(* One opening between each pair of chambers, and two doors in each: a door
   belongs to the room it is cut into. Made once here rather than in the
   description, which is rebuilt every frame. *)
let ways =
  Array.init (rooms - 1) (fun _ ->
      ( P.door ~name:"on" ~width:2.2 ~clearance:3. (),
        P.door ~name:"back" ~width:2.2 ~clearance:3. () ))

(* One chamber of the corridor, with a doorway back and a doorway on, except at
   the two ends. Coats alternate so crossings are visible. *)
let chamber ~index =
  let sw = Vec.make 0. (-.width)
  and se = Vec.make depth (-.width)
  and ne = Vec.make depth width
  and nw = Vec.make 0. width in
  let coat = if index mod 2 = 0 then Surfaces.brick else Surfaces.stone in
  let first = index = 0 and last = index = rooms - 1 in
  P.(
    (* Only the first chamber's floor is written; every other is carried
       through the opening it is reached by. A leg with no door on it is left
       whole, which is what a closed outline means. *)
    room ~name:(named index) ~height ~material:coat
      ~floor:
        (if first then floor ~plane:(Plane.horizontal 0.) Surfaces.ground
         else floor Surfaces.ground)
      ~ceiling:(roof Surfaces.soffit)
      ~outline:(corners [ sw; se; ne; nw ])
      ((if first then [ spawn (Vec.make 2. 0.) ] else [])
      @ (if last then [] else [ cut (fst ways.(index)) ~along:(se, ne) ])
      @ (if first then [] else [ cut (snd ways.(index - 1)) ~along:(nw, sw) ])
      @
      if last then
        [
          sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 6. 0.);
        ]
      else []))

(** Whether crossing [a] undoes crossing [b]: the same doorway, gone through the
    other way. Both sides are compared, because a room can be reached by more
    than one of its doorways and only one of them is the way back.

    Compared by name rather than by index: names are what a description deals
    in, and the result stays right if the rooms are written down in another
    order. *)
let undoes (a : Events.crossing) (b : Events.crossing) =
  a.Events.from_room = b.Events.to_room
  && a.Events.from_doorway = b.Events.to_doorway
  && a.Events.to_room = b.Events.from_room
  && a.Events.to_doorway = b.Events.from_doorway

let record stack crossing =
  match stack with
  | top :: rest when undoes crossing top -> rest
  | _ -> crossing :: stack

(* Not (width, height): width is a chamber's, and a buffer's is a different
   number. *)
let ticks ~stack ~viewport:(_, down) =
  let unit = Int.max 3 (down / 60) in
  let deep = List.length stack in
  P.(
    (if deep = 0 then
       (* Home: a single dim tick, so the row never disappears entirely. *)
       [
         rect ~x:(2 * unit)
           ~y:(down - (5 * unit))
           ~w:(2 * unit) ~h:(3 * unit) ~color:(Color.rgb 70 80 95) ();
       ]
     else
       (* One tick per doorway between here and the way out, oldest on the
          left. *)
       List.mapi
         (fun i _ ->
           rect
             ~x:((2 * unit) + ((deep - 1 - i) * 3 * unit))
             ~y:(down - (5 * unit))
             ~w:(2 * unit) ~h:(3 * unit) ~color:(Color.rgb 235 200 110) ())
         stack)
    @ [ crosshair ~color:(Color.rgb 245 245 245) () ])

let corridor ~stack ~viewport =
  P.(
    world ~atmosphere:Surfaces.air
      (List.init rooms (fun index -> chamber ~index)
      @ List.init (rooms - 1) (fun index ->
          let on, back = ways.(index) in
          connect on back)
      @ [ hud (ticks ~stack ~viewport) ]))

let unwinding =
  Element.declare ~name:"unwinding" @@ fun () ->
  let stack, set_stack = Hook.use_state [] in
  let crossings = Events.use_crossings () in
  Events.use_frame (fun ~dt:_ ->
      (* Folded in the order they were crossed, which is the order they have to
         be undone in: a frame that went out through one doorway and back
         through another leaves the stack one deeper, not two. *)
      match crossings with
      | [] -> ()
      | _ -> set_stack (List.fold_left record stack crossings));
  corridor ~stack ~viewport:(Events.use_viewport ())

let world =
  (Mount.build (corridor ~stack:[] ~viewport:Events.still.Events.viewport))
    .Scene.world

let run window = Run.on window ~controls:Bindings.escapable (unwinding ())
