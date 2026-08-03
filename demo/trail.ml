(** {b Traversal traces.} Every doorway a step went through, in the order it
    went through them.

    {!Camlcast_core.Player.traverse} returns where the player ended up {e and} a
    list of crossings, each naming the room and threshold it left by, the room
    and threshold it arrived at, and the transform applied on the way.
    {!Camlcast_core.Engine.move} is the same thing for a whole frame, with the
    turn applied first; {!Camlcast_core.Engine.step} is that with the list
    dropped, which is what every other demo here uses.
    {!Camlcast_core.Player.crossed} answers only whether the list is empty,
    which is all a world that grows needs.

    A frame can cross more than one doorway. Movement resolves its two axes one
    after the other, and each leg can go through an opening of its own — so the
    list, and its order, are the answer rather than a count.

    What this demo builds from them is the thing the list exists for: a return
    route. Each crossing is pushed onto a stack unless it is the exact reverse
    of the one on top, in which case that one is popped. Walk east through the
    doorways and the ticks along the bottom of the screen accumulate; walk back
    and they come off one at a time. The row of ticks is where you would have to
    walk to get home.

    That it is exact matters more than it sounds. The rooms here are laid out in
    a line, but nothing in the engine says they must be — a link is derived from
    two thresholds and from nothing else, so a corridor can return you somewhere
    it could not possibly go. A route home built from the crossings still
    arrives, because it is a record of what was walked and not a guess from the
    geometry. *)

open Camlcast

let height = 4.
let width = 2.5
let depth = 8.
let rooms = 5
let named index = Printf.sprintf "chamber-%d" index

(* One chamber of the corridor, with a doorway back the way you came and one on,
   except at the two ends. Alternating coats, so it is obvious you have gone
   through something. *)
let chamber ~index =
  let sw = Vec.make 0. (-.width)
  and se = Vec.make depth (-.width)
  and ne = Vec.make depth width
  and nw = Vec.make 0. width in
  let coat = if index mod 2 = 0 then Surfaces.brick else Surfaces.stone in
  let flat = Plane.horizontal 0. in
  let first = index = 0 and last = index = rooms - 1 in
  P.(
    room ~name:(named index)
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
      ([
         wall ~height ~material:coat sw se;
         (if last then wall ~height ~material:coat se ne
          else
            doorway ~name:"on" ~width:2.2 ~opening:3. ~height ~material:coat se
              ne);
         wall ~height ~material:coat ne nw;
         (if first then wall ~height ~material:coat nw sw
          else
            doorway ~name:"back" ~width:2.2 ~opening:3. ~height ~material:coat
              nw sw);
       ]
      @
      if last then
        [
          sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 6. 0.);
        ]
      else []))

(** Is this crossing the undoing of that one — the same doorway, gone through
    the other way? Both sides are compared, because a room can be reached by
    more than one of its doorways and only one of them is the way back.

    By name rather than by index, which is what a description deals in: the same
    comparison, and one that would still be right if the rooms were written down
    in another order. *)
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
      ~spawn:(named 0, Vec.make 2. 0.)
      (List.init rooms (fun index -> chamber ~index)
      @ List.init (rooms - 1) (fun index ->
          link (named index, "on") (named (index + 1), "back"))
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
