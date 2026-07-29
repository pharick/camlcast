(** {b Stateful doors.} A leaf hung in a doorway is [Open] or [Closed], and that
    decides both what you see and what you can walk through.

    Three doorways along one wall, all the same size, differing only in what
    hangs in them:

    - the {b left} one is a bare opening — no door at all, and the room beyond
      is drawn through it;
    - the {b middle} one has an oak door: press {b E} beside it to open or shut
      it;
    - the {b right} one has an iron door that this demo will not open. Press E
      beside it and the meter flashes red instead.

    An open door draws nothing and stops nothing, so it is indistinguishable
    from the bare opening beside it. A closed one draws as a leaf of its own
    material and refuses the step — walking into a shut door is how you find out
    it is shut.

    {b The third door is the point.} The engine has no notion of a locked door;
    it knows [Open] and [Closed] and nothing else. "Locked" is this demo's word
    for a door it declines to open, and the whole of that rule is {!locked}
    below — a list of the doorways it will not touch. An engine that carried a
    [Locked] state would have treated it exactly as [Closed] anyway, so the rule
    would have lived here regardless; this way it is written where it is
    decided.

    What the division costs is visible in {!locked} too: it has {e two} entries
    for one door, because a door has two sides and a game's record of it has to
    agree with itself just as the engine's does. {!Camlcast.World.set_door}
    keeps the engine's two sides in step for you — open the middle door, walk
    through, look back, and it is open from there — but only for the part the
    engine knows about.

    The meter along the bottom is the nearest door: empty when it is open, full
    when it is shut, red for a moment when this demo refuses to work it. *)

open Camlcast
open Result_ext

let height = 4.
let oak = Surfaces.oak
let iron = Surfaces.solid (Patterns.door ~color:(Color.rgb 96 104 118))

(** The doorways this demo will not open, as [(room, threshold)] pairs — both
    sides of the one iron door. Keeping the two in step is the bookkeeping a
    game takes on in exchange for the engine not carrying a [Locked] state. *)
let locked = [ (0, 2); (3, 0) ]

type t = {
  world : World.t;
  player : Player.t;
  refused : float;  (** seconds left of the "that one will not open" flash *)
}

(* The hall you arrive in: a wide room whose east wall is three doorways. *)
let hall =
  let sw = Vec.make (-10.) (-7.)
  and se = Vec.make 0. (-7.)
  and ne = Vec.make 0. 7.
  and nw = Vec.make (-10.) 7. in
  let cut name ~door a b =
    Room.doorway ~name ?door ~width:2.4 ~opening:3. ~height
      ~material:Surfaces.brick a b
  in
  (* All three in the east wall, cut south to north so the wall's winding is
     unbroken: bare, workable, sealed. *)
  let bare_jambs, bare = cut "bare" ~door:None se (Vec.make 0. (-2.4))
  and worked_jambs, worked =
    cut "worked"
      ~door:(Some (Door.make oak))
      (Vec.make 0. (-2.4)) (Vec.make 0. 2.4)
  and sealed_jambs, sealed =
    cut "sealed" ~door:(Some (Door.make iron)) (Vec.make 0. 2.4) ne
  in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ bare; worked; sealed ]
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.above floor height; material = Surfaces.soffit })
    (List.concat
       [
         bare_jambs;
         worked_jambs;
         sealed_jambs;
         [ wall sw se; wall ne nw; wall nw sw ];
       ])

(* What is behind each of them: the same small chamber three times over, each
   with its own way back — and its own copy of whatever hangs in it, because
   both sides of a link must agree about a door. *)
let chamber ~door =
  let sw = Vec.make 0. (-3.)
  and se = Vec.make 7. (-3.)
  and ne = Vec.make 7. 3.
  and nw = Vec.make 0. 3. in
  let jambs, back =
    Room.doorway ~name:"back" ?door ~width:2.4 ~opening:3. ~height
      ~material:Surfaces.brick nw sw
  in
  let wall a b = Room.wall ~height ~material:Surfaces.brick a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ back ]
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.above floor height; material = Surfaces.soffit })
    ~sprites:[ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 4. 0.) ]
    (jambs @ [ wall sw se; wall se ne; wall ne nw ])

let world =
  World.make
    ~rooms:
      [
        ("hall", hall);
        ("behind-bare", chamber ~door:None);
        ("behind-worked", chamber ~door:(Some (Door.make oak)));
        ("behind-sealed", chamber ~door:(Some (Door.make iron)));
      ]
    ~links:
      [
        (("hall", "bare"), ("behind-bare", "back"));
        (("hall", "worked"), ("behind-worked", "back"));
        (("hall", "sealed"), ("behind-sealed", "back"));
      ]
    ~atmosphere:Surfaces.air
    ~spawn:("hall", Vec.make (-6.) 0.)

let start = { world; player = Player.spawn world; refused = 0. }

(** The doorway with a door in it that the player is nearest to, if they are
    near enough to reach one. A game would find this by looking where the player
    is pointing — that is E6 — so this stands in for it: nearest wins, within
    arm's reach of the opening's middle.

    Openings with nothing hanging in them are skipped, there being nothing there
    to work. *)
let nearest (world : World.t) (player : Player.t) =
  let room = World.room world player.Player.room in
  let reach = 3.5 in
  Array.to_list room.Room.thresholds
  |> List.mapi (fun i (t : Room.threshold) -> (i, t))
  |> List.filter_map (fun (i, (t : Room.threshold)) ->
      if t.Room.door = None then None
      else
        let middle = Vec.scale (Vec.add t.Room.a t.Room.b) 0.5 in
        let away = Vec.length (Vec.sub middle player.Player.pos) in
        if away <= reach then Some (away, i) else None)
  |> List.sort (fun (a, _) (b, _) -> Float.compare a b)
  |> function
  | [] -> None
  | (_, i) :: _ -> Some i

let state_of world (player : Player.t) threshold =
  match
    (World.room world player.Player.room).Room.thresholds.(threshold).Room.door
  with
  | Some door -> Some door.Door.state
  | None -> None

let update state ~dt ~motion ~actions =
  let player = Engine.step state.world state.player motion in
  let fade = Float.max 0. (state.refused -. dt) in
  let tried = Input.pressed actions (Input.Key Key.e) in
  match (tried, nearest state.world player) with
  | false, _ | _, None -> { state with player; refused = fade }
  | true, Some threshold ->
      if List.mem (player.Player.room, threshold) locked then
        (* This demo's rule, and the whole of it. The door is perfectly workable
           as far as the engine is concerned. *)
        { state with player; refused = 0.9 }
      else
        let next =
          match state_of state.world player threshold with
          | Some Door.Closed -> Door.Open
          | _ -> Door.Closed
        in
        {
          world =
            World.set_door state.world ~room:player.Player.room ~threshold next;
          player;
          refused = fade;
        }

let overlay fb state =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let unit = Int.max 3 (height / 60) in
  (match nearest state.world state.player with
  | None -> ()
  | Some threshold ->
      let shut =
        state_of state.world state.player threshold = Some Door.Closed
      in
      let refusing = state.refused > 0. in
      Paint.bar fb ~x:(2 * unit)
        ~y:(height - (5 * unit))
        ~w:(width / 3) ~h:(2 * unit)
        ~fraction:(if shut then 1. else 0.06)
        ~r:(if refusing then 235 else 210)
        ~g:(if refusing then 80 else if shut then 170 else 220)
        ~b:(if refusing then 70 else if shut then 90 else 130));
  Paint.crosshair fb ~r:245 ~g:245 ~b:245

let run window =
  let+ _, ending =
    Engine.run window ~bindings:Bindings.escapable ~update
      ~view:(fun state -> (state.world, state.player))
      ~overlay start
  in
  ending
