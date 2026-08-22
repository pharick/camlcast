(** {b Stateful doors.} A leaf hung in a doorway is [Open] or [Closed], and that
    decides both what is drawn and what can be walked through.

    Three doorways along one wall, all the same size, differing only in what
    hangs in them:

    - {b left}: a bare opening — no door, and the room beyond is drawn through
      it;
    - {b middle}: an oak door. Press {b E} beside it to open or shut it;
    - {b right}: an iron door this demo will not open. Pressing E beside it
      flashes the meter red instead.

    An open door draws nothing and stops nothing, so it is indistinguishable
    from the bare opening beside it. A closed one draws as a leaf of its own
    material and refuses the step — a shut door is discovered by walking into
    it.

    {b The third door is the point.} The engine has no notion of a locked door;
    it knows [Open] and [Closed] and nothing else. "Locked" is this demo's word
    for a door it declines to open, and the whole rule is {!locked} below — a
    list of the doorways it will not touch. An engine that carried a [Locked]
    state would have treated it exactly as [Closed] anyway, so the rule would
    have lived here regardless; this way it is written where it is decided.

    Both sides of a doorway share one name, so the game's record and the
    engine's cannot disagree about a door's state — the bookkeeping
    {!Camlcast_core.World.set_door} does for a game that works in indices. Open
    the middle door, walk through, look back: it is open from there too.

    The meter along the bottom is the door under the crosshair: empty when it is
    open, full when it is shut, red for a moment when this demo refuses to work
    it. *)

open Camlcast

let height = 4.
let oak = Surfaces.oak
let iron = Surfaces.solid (Patterns.door ~color:(Color.rgb 96 104 118))
let flat = Plane.horizontal 0.

(** The doorway this demo will not open, by the name both its sides share.

    The engine carries no [Locked] state, so a game that wants one keeps the
    bookkeeping itself. A description names its doorways, which makes that
    bookkeeping a name: one name for one door, rather than a [(room, threshold)]
    index for each of its two sides and the job of keeping them in step. *)
let locked = [ "sealed" ]

(* The arrival hall: a wide room whose east wall is three doorways. All three
   cut south to north, so the wall's winding is unbroken. *)
let hall_sw = Vec.make (-10.) (-7.)
let hall_se = Vec.make 0. (-7.)
let hall_ne = Vec.make 0. 7.
let hall_nw = Vec.make (-10.) 7.
let south = Vec.make 0. (-2.4)
let north = Vec.make 0. 2.4

(** What hangs in a doorway: nothing if it has no leaf, nothing if it has been
    opened, and the leaf otherwise. Both sides of a link ask this with the same
    name, so they cannot disagree — which is the whole of what
    {!Camlcast_core.World.set_door} has to keep in step for its two indices. *)
let leaf ~opened name =
  match name with
  | _ when List.mem name opened -> None
  | "worked" -> Some (Door.make oak)
  | "sealed" -> Some (Door.make iron)
  | _ -> None

(* Behind each doorway: the same small chamber three times over, each with its
   own way back and its own copy of whatever hangs in it. *)
let chamber =
  Element.declare ~name:"chamber" @@ fun (name, hung, reacts) ->
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
        boundary ~closed:false ~height ~material:Surfaces.brick
          (corners [ sw; se; ne; nw ]);
        doorway ~name:"back" ?door:hung ~on_gaze ~on_use ~width:2.4 ~opening:3.
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
  (* One pair of handlers per doorway name, given to both of its sides, so the
     doorway itself is told. Without somewhere to hang a handler, a game is left
     finding the nearest opening with a door in it and working that. *)
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
