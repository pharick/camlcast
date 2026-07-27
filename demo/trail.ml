(** {b Traversal traces.} Every doorway a step went through, in the order it
    went through them.

    {!Raycaster.Player.traverse} returns where the player ended up {e and} a
    list of crossings, each naming the room and threshold it left by, the room
    and threshold it arrived at, and the transform applied on the way.
    {!Raycaster.Engine.advance} is the same thing for a whole frame, with the
    turn applied first; {!Raycaster.Engine.step} is that with the list dropped,
    which is what every other demo here uses.

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

open Raycaster
open Result_ext

let height = 4.
let width = 2.5
let depth = 8.
let rooms = 5

type t = { player : Player.t; stack : Player.crossing list }

(* One chamber of the corridor, with a doorway back the way you came and one on,
   except at the two ends. Alternating coats, so it is obvious you have gone
   through something. *)
let chamber ~index =
  let sw = Vec.make 0. (-.width)
  and se = Vec.make depth (-.width)
  and ne = Vec.make depth width
  and nw = Vec.make 0. width in
  let coat = if index mod 2 = 0 then Surfaces.brick else Surfaces.stone in
  let cut name a b =
    Room.doorway ~name ~width:2.2 ~opening:3. ~height ~material:coat a b
  in
  let wall a b = Room.wall ~height ~material:coat a b in
  let back_jambs, back = cut "back" nw sw and on_jambs, on = cut "on" se ne in
  let floor = Plane.horizontal 0. in
  let first = index = 0 and last = index = rooms - 1 in
  Room.make
    ~thresholds:
      (List.concat
         [ (if first then [] else [ back ]); (if last then [] else [ on ]) ])
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.above floor height; material = Surfaces.soffit })
    ~sprites:
      (if last then
         [ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 6. 0.) ]
       else [])
    (List.concat
       [
         [ wall sw se ];
         (if last then [ wall se ne ] else on_jambs);
         [ wall ne nw ];
         (if first then [ wall nw sw ] else back_jambs);
       ])

let named index = Printf.sprintf "chamber-%d" index

let world =
  World.make
    ~rooms:(List.init rooms (fun index -> (named index, chamber ~index)))
    ~links:
      (List.init (rooms - 1) (fun index ->
           ((named index, "on"), (named (index + 1), "back"))))
    ~atmosphere:Surfaces.air
    ~spawn:(named 0, Vec.make 2. 0.)

let start = { player = Player.spawn world; stack = [] }

(** Is this crossing the undoing of that one — the same doorway, gone through
    the other way? Both sides are compared, because a room can be reached by
    more than one of its doorways and only one of them is the way back. *)
let undoes (a : Player.crossing) (b : Player.crossing) =
  a.Player.from_room = b.Player.to_room
  && a.Player.from_threshold = b.Player.to_threshold
  && a.Player.to_room = b.Player.from_room
  && a.Player.to_threshold = b.Player.from_threshold

let record stack crossing =
  match stack with
  | top :: rest when undoes crossing top -> rest
  | _ -> crossing :: stack

let update state ~dt:_ ~motion ~actions:_ =
  let moved = Engine.advance world state.player motion in
  {
    player = moved.Player.player;
    (* Folded in the order they were crossed, which is the order they have to be
       undone in: a frame that went out through one doorway and back through
       another leaves the stack one deeper, not two. *)
    stack = List.fold_left record state.stack moved.Player.crossings;
  }

let overlay fb state =
  let height = fb.Framebuffer.height in
  let unit = Int.max 3 (height / 60) in
  let depth = List.length state.stack in
  (* One tick per doorway between here and the way out, oldest on the left. *)
  List.iteri
    (fun i _ ->
      Paint.rect fb
        ~x:((2 * unit) + ((depth - 1 - i) * 3 * unit))
        ~y:(height - (5 * unit))
        ~w:(2 * unit) ~h:(3 * unit) ~r:235 ~g:200 ~b:110 ~alpha:255)
    state.stack;
  if depth = 0 then
    (* Home: a single dim tick, so the row never disappears entirely. *)
    Paint.rect fb ~x:(2 * unit)
      ~y:(height - (5 * unit))
      ~w:(2 * unit) ~h:(3 * unit) ~r:70 ~g:80 ~b:95 ~alpha:255;
  Paint.crosshair fb ~r:245 ~g:245 ~b:245

let run () =
  let+ _ =
    Engine.run_state ~update
      ~view:(fun state -> (world, state.player))
      ~overlay start
  in
  ()
