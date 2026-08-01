(** {b Actions, holds and the cursor.} What the input layer offers a game beyond
    walking about — and, since walking about is a game's too, the table that
    says which key does it.

    The engine names no actions. It reports {e controls} — a
    {!Camlcast_core.Key}, or a mouse button — and four questions about each: is
    it down, did it go down this frame, did it come up this frame, and how long
    has it been held. "Interact" and "commit" are this demo's words, and the
    table from the one to the other is {!interact}, {!screen} and {!primary}
    below.

    - {b Hold E.} The meter fills. Let go before it is full and the {b blue}
      lamp lights; let go after and the {b green} one does. That is the whole of
      press-versus-hold: {!Camlcast_core.Input.held_for} counts from the frame
      after the press and keeps its value for the one frame
      {!Camlcast_core.Input.released} is true, so the release itself can ask how
      long the hold lasted.
    - {b Click.} The amber lamp lights. A mouse button is a control like a key.
    - {b Tab.} Releases the mouse. The cursor comes back, a white square follows
      it, and the camera stops turning with it — that is [pointing] on
      {!Camlcast_core.Engine.run}, which is how a game that opens a screen over
      its world hands the mouse to it. Press it again to take the mouse back.
    - {b IJKL, as well as WASD.} Walking is bound by a
      {!Camlcast_core.Binding.t} like everything else, and this demo hands
      {!Camlcast_core.Engine.run} one of its own with a second set of keys
      added. Both sets are live at once, and holding one of each walks at
      {e one} speed rather than two — an axis clamps what its terms add up to,
      which is what stops a table with two keys on one axis from running.

    The line across the top is printed from that table with
    {!Camlcast_core.Key.name}, not spelled out: move a key here and the words on
    screen follow it. What the player reads is the layout's name for the place,
    so the line says Z on an AZERTY board where it says W on a QWERTY one — and
    it is the same key either way, because a binding is a place.

    The cursor arrives in [update] already in the framebuffer's coordinates,
    which are the ones the overlay draws in, so the square lands under the
    pointer at any window size. *)

open Camlcast

let height = 4.

(** This demo's table from controls to what it calls them. *)
let interact = Input.Key Key.e

let screen = Input.Key Key.tab
let primary = Input.Button Input.Left

(** What to print for a control. {!Camlcast.Key.name} does the hard half — which
    key of the layout in front of the player this place is — and a game supplies
    its own word for a mouse button, because "click" is a choice about wording
    rather than about hardware. *)
let named = function
  | Input.Key key -> Key.name key
  | Input.Button Input.Left -> "click"
  | Input.Button Input.Middle -> "middle"
  | Input.Button Input.Right -> "right click"

(** The engine's table with a second set of walking keys added to it: the whole
    of rebinding is a value like this one, stated once and handed to
    {!Camlcast.Run.on}.

    [~leave] has to be asked for — {!Camlcast.Binding.default} binds no key that
    ends a run, since a game with screens in it wants Escape for closing them —
    which is what {!Camlcast_demo.Bindings} does for the demos that take it as
    it stands. *)
let bindings =
  let also axis ~positive ~negative =
    {
      axis with
      Binding.terms =
        axis.Binding.terms
        @ [
            { Binding.source = Binding.Hold (Input.Key positive); weight = 1. };
            { Binding.source = Binding.Hold (Input.Key negative); weight = -1. };
          ];
    }
  in
  Binding.make
    ~forward:
      (also Binding.default.Binding.forward ~positive:Key.i ~negative:Key.k)
    ~strafe:
      (also Binding.default.Binding.strafe ~positive:Key.l ~negative:Key.j)
    ~leave:[ Input.Key Key.escape ] ()

(** Read off the table above, so the two cannot drift apart.

    Lazy, and that is not an optimisation: {!Camlcast.Key.name} answers for the
    layout SDL knows about, and SDL only reads the real one when the video
    subsystem starts. Built at module load this line would name the US keyboard
    on every machine. Built on the first frame it names the player's. *)
let help =
  lazy
    (Printf.sprintf "%s hold   %s cursor   %s   %s%s%s%s or WASD to walk"
       (named interact) (named screen) (named primary) (Key.name Key.i)
       (Key.name Key.j) (Key.name Key.k) (Key.name Key.l))

let font = Typeface.font

(** How long E must be held for it to count as a hold rather than a press. *)
let commit_after = 1.5

(** How long a lamp stays lit after the thing it reports. *)
let lamp_time = 0.8

let flat = Plane.horizontal 0.
let sw = Vec.make (-7.) (-7.)
let se = Vec.make 7. (-7.)
let ne = Vec.make 7. 7.
let nw = Vec.make (-7.) 7.

let chamber =
  P.(
    room ~name:"room"
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
      [
        outline ~height ~material:Surfaces.stone [ sw; se; ne; nw ];
        sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure (Vec.make 3. 0.);
        sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
          (Vec.make 0. (-2.5));
      ])

let spawn = ("room", Vec.make (-4.5) 0.)

(* Not (width, height): a local open of P puts a wall's height in scope, and a
   buffer's is a different number. *)
let panel ~hold ~lamps ~pointing ~pointer ~viewport:(across, down) =
  let font = Lazy.force font in
  let unit = Int.max 3 (down / 60) in
  let margin = 2 * unit in
  let full = hold >= commit_after in
  P.(
    [
      text ~font ~x:margin ~y:margin ~color:(Color.rgb 150 156 170)
        (Lazy.force help);
      (* The hold meter, turning from amber to green as it passes the mark. *)
      bar ~x:margin
        ~y:(down - margin - (2 * unit))
        ~w:(across / 3) ~h:(2 * unit) ~fraction:(hold /. commit_after)
        ~color:
          (Color.rgb
             (if full then 120 else 230)
             (if full then 220 else 180)
             (if full then 130 else 80))
        ();
    ]
    (* Three lamps in a row, each fading out over [lamp_time]. *)
    @ List.mapi
        (fun i (left, (r, g, b)) ->
          rect
            ~x:(margin + (i * 5 * unit))
            ~y:(down - margin - (7 * unit))
            ~w:(4 * unit) ~h:(3 * unit) ~color:(Color.rgb r g b)
            ~alpha:(int_of_float (255. *. Float.min 1. (left /. lamp_time)))
            ())
        lamps
    @
    if pointing then
      let x, y = pointer in
      [
        rect ~x:(x - unit) ~y:(y - unit) ~w:(2 * unit) ~h:(2 * unit)
          ~color:(Color.rgb 250 250 250) ~alpha:255 ();
      ]
    else [ crosshair ~color:(Color.rgb 245 245 245) () ])

let fade lamp dt = Float.max 0. (lamp -. dt)

let reading =
  Element.declare ~name:"reading" @@ fun () ->
  let actions = Events.use_actions () in
  let pointing, set_pointing = Hook.use_state false in
  let tap, set_tap = Hook.use_state 0. in
  let commit, set_commit = Hook.use_state 0. in
  let click, set_click = Hook.use_state 0. in
  Events.use_key_down Key.tab (fun () -> set_pointing (not pointing));
  Events.use_frame (fun ~dt ->
      let let_go = Input.released actions interact in
      let lasted = Input.held_for actions interact in
      set_tap
        (if let_go && lasted < commit_after then lamp_time else fade tap dt);
      set_commit
        (if let_go && lasted >= commit_after then lamp_time else fade commit dt);
      set_click
        (if Input.pressed actions primary then lamp_time else fade click dt));
  P.(
    world ~atmosphere:Surfaces.air ~spawn
      [
        chamber;
        (if pointing then cursor else Element.empty);
        hud
          (panel
             ~hold:
               (if Input.down actions interact then
                  Input.held_for actions interact
                else 0.)
             ~lamps:
               [
                 (tap, (110, 170, 245));
                 (commit, (120, 220, 130));
                 (click, (240, 190, 90));
               ]
             ~pointing ~pointer:(Input.pointer actions)
             ~viewport:(Events.use_viewport ()));
      ])

let world =
  (Mount.build P.(world ~atmosphere:Surfaces.air ~spawn [ chamber ]))
    .Scene.world

let run window = Run.on window ~bindings (reading ())
