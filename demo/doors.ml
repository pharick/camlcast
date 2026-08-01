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
    agree with itself just as the engine's does. {!Camlcast_core.World.set_door}
    keeps the engine's two sides in step for you — open the middle door, walk
    through, look back, and it is open from there — but only for the part the
    engine knows about.

    The meter along the bottom is the nearest door: empty when it is open, full
    when it is shut, red for a moment when this demo refuses to work it. *)

open Camlcast

let height = 4.
let oak = Surfaces.oak
let iron = Surfaces.solid (Patterns.door ~color:(Color.rgb 96 104 118))
let flat = Plane.horizontal 0.

(** The doorway this demo will not open, by the name both its sides share.

    It used to be a pair of [(room, threshold)] indices, one for each side, and
    keeping the two in step was the bookkeeping a game took on in exchange for
    the engine not carrying a [Locked] state. A description names its doorways,
    so there is one name for one door and nothing to keep in step. *)
let locked = [ "sealed" ]

(* The hall you arrive in: a wide room whose east wall is three doorways. All
   three cut south to north, so the wall's winding is unbroken. *)
let hall_sw = Vec.make (-10.) (-7.)
let hall_se = Vec.make 0. (-7.)
let hall_ne = Vec.make 0. 7.
let hall_nw = Vec.make (-10.) 7.
let south = Vec.make 0. (-2.4)
let north = Vec.make 0. 2.4

(** What hangs in a doorway: nothing if it has no leaf, nothing if it has been
    opened, and the leaf otherwise. Both sides of a link ask this with the same
    name, so they cannot disagree — which is the whole of what
    {!Camlcast_core.World.set_door} used to have to keep in step. *)
let leaf ~opened name =
  match name with
  | _ when List.mem name opened -> None
  | "worked" -> Some (Door.make oak)
  | "sealed" -> Some (Door.make iron)
  | _ -> None

(* What is behind each of them: the same small chamber three times over, each
   with its own way back, and its own copy of whatever hangs in it. *)
let chamber =
  Element.declare ~name:"chamber" @@ fun (name, door, reacts) ->
  let sw = Vec.make 0. (-3.)
  and se = Vec.make 7. (-3.)
  and ne = Vec.make 7. 3.
  and nw = Vec.make 0. 3. in
  let on_gaze, on_use = reacts in
  P.(
    room ~name:("behind-" ^ name)
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
      [
        path ~height ~material:Surfaces.brick [ sw; se; ne; nw ];
        doorway ~name:"back" ?door ~on_gaze ~on_use ~width:2.4 ~opening:3.
          ~height ~material:Surfaces.brick nw sw;
        sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 4. 0.);
      ])

let ways =
  [
    ("bare", hall_se, south);
    ("worked", south, north);
    ("sealed", north, hall_ne);
  ]

(* Not (width, height): a local open of P is about to put a wall's height in
   scope, and a buffer's is a different number. *)
let at ~opened ~refused ~aimed ~viewport:(across, down) ~reacts =
  let unit = Int.max 3 (down / 60) in
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("hall", Vec.make (-6.) 0.)
      ([
         room ~name:"hall"
           ~floor:(floor ~plane:flat ~material:Surfaces.ground)
           ~ceiling:
             (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
           (List.map
              (fun (name, a, b) ->
                let on_gaze, on_use = reacts name in
                doorway ~name ?door:(leaf ~opened name) ~on_gaze ~on_use
                  ~width:2.4 ~opening:3. ~height ~material:Surfaces.brick a b)
              ways
           @ [
               wall ~height ~material:Surfaces.stone hall_sw hall_se;
               wall ~height ~material:Surfaces.stone hall_ne hall_nw;
               wall ~height ~material:Surfaces.stone hall_nw hall_sw;
             ]);
       ]
      @ List.map
          (fun (name, _, _) ->
            chamber ~key:name (name, leaf ~opened name, reacts name))
          ways
      @ List.map
          (fun (name, _, _) -> link ("hall", name) ("behind-" ^ name, "back"))
          ways
      @ [
          hud
            ((match aimed with
               | None -> []
               | Some name ->
                   let shut = Option.is_some (leaf ~opened name) in
                   let refusing = refused > 0. in
                   [
                     bar ~x:(2 * unit)
                       ~y:(down - (5 * unit))
                       ~w:(across / 3) ~h:(2 * unit)
                       ~fraction:(if shut then 1. else 0.06)
                       ~color:
                         (Color.rgb
                            (if refusing then 235 else 210)
                            (if refusing then 80 else if shut then 170 else 220)
                            (if refusing then 70 else if shut then 90 else 130))
                       ();
                   ])
            @ [ crosshair ~color:(Color.rgb 245 245 245) () ]);
        ]))

let working =
  Element.declare ~name:"working" @@ fun () ->
  let opened, set_opened = Hook.use_state [] in
  let refused, set_refused = Hook.use_state 0. in
  let aimed, set_aimed = Hook.use_state None in
  Events.use_frame (fun ~dt ->
      if refused > 0. then set_refused (Float.max 0. (refused -. dt)));
  (* One pair of handlers per doorway name, given to both of its sides. Which
     is what the old version could not do: it found the nearest opening with a
     door in it and worked that, because there was nothing to hang a handler on.
     Now the doorway itself is told. *)
  let reacts name =
    ( (fun here -> set_aimed (if here then Some name else None)),
      fun _ ->
        if List.mem name locked then
          (* This demo's rule, and the whole of it. The door is perfectly
             workable as far as the engine is concerned. *)
          set_refused 0.9
        else if List.mem name opened then
          set_opened (List.filter (fun other -> other <> name) opened)
        else set_opened (name :: opened) )
  in
  at ~opened ~refused ~aimed ~viewport:(Events.use_viewport ()) ~reacts

let world =
  (Mount.build
     (at ~opened:[] ~refused:0. ~aimed:None
        ~viewport:Events.still.Events.viewport ~reacts:(fun _ ->
          ((fun _ -> ()), fun _ -> ()))))
    .Scene.world

let run window = Run.on window ~controls:Bindings.escapable (working ())
