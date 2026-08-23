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

(** The doorway this demo will not open, by the name this demo calls it.

    The engine carries no [Locked] state, so a game that wants one keeps the
    bookkeeping itself. These names are that bookkeeping and nothing else: the
    engine stopped needing a doorway to be named when a connection began joining
    doors rather than names, and what is left is a game keying its own state the
    way any game would. One name for one opening, whichever side of it you are
    on. *)
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
  Element.declare ~name:"chamber" @@ fun (back, hung, reacts) ->
  let sw = Vec.make 0. (-3.)
  and se = Vec.make 7. (-3.)
  and ne = Vec.make 7. 3.
  and nw = Vec.make 0. 3. in
  let on_gaze, on_use = reacts in
  P.(
    room ~height ~material:Surfaces.brick
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
      ~outline:(corners [ sw; se; ne; nw ])
      [
        cut back ?leaf:hung ~on_gaze ~on_use ~along:(nw, sw);
        sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 4. 0.);
      ])

(* One opening per way, made once here rather than in the description, which is
   rebuilt every frame: a door carries the identity its connection joins by, and
   a door made per frame would be a different opening every frame. Two per way,
   because a door belongs to the room it is cut into and each of these joins
   two. *)
let ways =
  List.map
    (fun (name, a, b) ->
      ( name,
        a,
        b,
        P.door ~name ~width:2.4 ~clearance:3. (),
        P.door ~name ~width:2.4 ~clearance:3. () ))
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
      ([
         (* The east side is three legs of the outline, one per opening, and all
            three are brick where the rest of the hall is stone. *)
         room ~height ~material:Surfaces.stone
           ~floor:(floor ~plane:flat ~material:Surfaces.ground)
           ~ceiling:
             (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
           ~outline:
             [
               corner hall_sw;
               corner hall_se ~material:Surfaces.brick;
               corner south ~material:Surfaces.brick;
               corner north ~material:Surfaces.brick;
               corner hall_ne;
               corner hall_nw;
             ]
           (spawn (Vec.make (-6.) 0.)
           :: List.map
                (fun (name, a, b, front, _) ->
                  let on_gaze, on_use = reacts name in
                  cut front ?leaf:(leaf ~opened name) ~on_gaze ~on_use
                    ~along:(a, b))
                ways);
       ]
      @ List.map
          (fun (name, _, _, _, back) ->
            chamber ~key:name (back, leaf ~opened name, reacts name))
          ways
      @ List.map (fun (_, _, _, front, back) -> connect front back) ways
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
