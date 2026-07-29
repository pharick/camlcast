(** {b Actions, holds and the cursor.} What the input layer offers a game beyond
    walking about — and, since walking about is a game's too, the table that
    says which key does it.

    The engine names no actions. It reports {e controls} — a {!Camlcast.Key}, or
    a mouse button — and four questions about each: is it down, did it go down
    this frame, did it come up this frame, and how long has it been held.
    "Interact" and "commit" are this demo's words, and the table from the one to
    the other is {!interact}, {!screen} and {!primary} below.

    - {b Hold E.} The meter fills. Let go before it is full and the {b blue}
      lamp lights; let go after and the {b green} one does. That is the whole of
      press-versus-hold: {!Camlcast.Input.held_for} counts from the frame after
      the press and keeps its value for the one frame {!Camlcast.Input.released}
      is true, so the release itself can ask how long the hold lasted.
    - {b Click.} The amber lamp lights. A mouse button is a control like a key.
    - {b Tab.} Releases the mouse. The cursor comes back, a white square follows
      it, and the camera stops turning with it — that is [pointing] on
      {!Camlcast.Engine.run}, which is how a game that opens a screen over its
      world hands the mouse to it. Press it again to take the mouse back.
    - {b IJKL, as well as WASD.} Walking is bound by a {!Camlcast.Binding.t}
      like everything else, and this demo hands {!Camlcast.Engine.run} one of
      its own with a second set of keys added. Both sets are live at once, and
      holding one of each walks at {e one} speed rather than two — an axis
      clamps what its terms add up to, which is what stops a table with two keys
      on one axis from running.

    The line across the top is printed from that table with
    {!Camlcast.Key.name}, not spelled out: move a key here and the words on
    screen follow it. What the player reads is the layout's name for the place,
    so the line says Z on an AZERTY board where it says W on a QWERTY one — and
    it is the same key either way, because a binding is a place.

    The cursor arrives in [update] already in the framebuffer's coordinates,
    which are the ones the overlay draws in, so the square lands under the
    pointer at any window size. *)

open Camlcast
open Result_ext

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
    {!Camlcast.Engine.run}.

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

type t = {
  player : Player.t;
  pointing : bool;
  hold : float;  (** how long E has been held, or zero *)
  tap : float;  (** seconds of lamp left, for each of the three *)
  commit : float;
  click : float;
  pointer : int * int;
}

let world =
  let sw = Vec.make (-7.) (-7.)
  and se = Vec.make 7. (-7.)
  and ne = Vec.make 7. 7.
  and nw = Vec.make (-7.) 7. in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:
        [
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 3. 0.);
          Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make 0. (-2.5));
        ]
      [ wall sw se; wall se ne; wall ne nw; wall nw sw ]
  in
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-4.5) 0.)

let start =
  {
    player = Player.spawn world;
    pointing = false;
    hold = 0.;
    tap = 0.;
    commit = 0.;
    click = 0.;
    pointer = (0, 0);
  }

let fade lamp dt = Float.max 0. (lamp -. dt)

let update state ~dt ~motion ~actions =
  let let_go = Input.released actions interact in
  let lasted = Input.held_for actions interact in
  {
    player = Engine.step world state.player motion;
    pointing =
      (if Input.pressed actions screen then not state.pointing
       else state.pointing);
    hold = (if Input.down actions interact then lasted else 0.);
    tap =
      (if let_go && lasted < commit_after then lamp_time else fade state.tap dt);
    commit =
      (if let_go && lasted >= commit_after then lamp_time
       else fade state.commit dt);
    click =
      (if Input.pressed actions primary then lamp_time else fade state.click dt);
    pointer = Input.pointer actions;
  }

let overlay fb state =
  let font = Lazy.force font in
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let unit = Int.max 3 (height / 60) in
  let margin = 2 * unit in
  Font.draw fb font (Lazy.force help) ~x:margin ~y:margin
    ~color:(Color.rgb 150 156 170);
  (* The hold meter, turning from amber to green as it passes the mark. *)
  let full = state.hold >= commit_after in
  Paint.bar fb ~x:margin
    ~y:(height - margin - (2 * unit))
    ~w:(width / 3) ~h:(2 * unit)
    ~fraction:(state.hold /. commit_after)
    ~r:(if full then 120 else 230)
    ~g:(if full then 220 else 180)
    ~b:(if full then 130 else 80);
  (* Three lamps in a row, each fading out over [lamp_time]. *)
  List.iteri
    (fun i (left, (r, g, b)) ->
      let alpha = int_of_float (255. *. Float.min 1. (left /. lamp_time)) in
      Paint.rect fb
        ~x:(margin + (i * 5 * unit))
        ~y:(height - margin - (7 * unit))
        ~w:(4 * unit) ~h:(3 * unit) ~r ~g ~b ~alpha)
    [
      (state.tap, (110, 170, 245));
      (state.commit, (120, 220, 130));
      (state.click, (240, 190, 90));
    ];
  if state.pointing then begin
    let x, y = state.pointer in
    Paint.rect fb ~x:(x - unit) ~y:(y - unit) ~w:(2 * unit) ~h:(2 * unit) ~r:250
      ~g:250 ~b:250 ~alpha:255
  end
  else Paint.crosshair fb ~r:245 ~g:245 ~b:245

let run window =
  let+ _, ending =
    Engine.run window ~bindings ~update
      ~view:(fun state -> (world, state.player))
      ~overlay
      ~pointing:(fun state -> state.pointing)
      start
  in
  ending
