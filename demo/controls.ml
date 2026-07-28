(** {b Actions, holds and the cursor.} What the input layer offers a game beyond
    walking about.

    The engine names no actions. It reports {e controls} — a key, by its
    scancode, or a mouse button — and four questions about each: is it down, did
    it go down this frame, did it come up this frame, and how long has it been
    held. "Interact" and "commit" are this demo's words, and the table from the
    one to the other is the four lines at the top of [update].

    - {b Hold E.} The meter fills. Let go before it is full and the {b blue}
      lamp lights; let go after and the {b green} one does. That is the whole of
      press-versus-hold: {!Raycaster.Input.held_for} counts from the frame after
      the press and keeps its value for the one frame
      {!Raycaster.Input.released} is true, so the release itself can ask how
      long the hold lasted.
    - {b Click.} The amber lamp lights. A mouse button is a control like a key.
    - {b Tab.} Releases the mouse. The cursor comes back, a white square follows
      it, and the camera stops turning with it — that is [pointing] on
      {!Raycaster.Engine.run_state}, which is how a game that opens a screen
      over its world hands the mouse to it. Press it again to take the mouse
      back.

    The cursor arrives in [update] already in the framebuffer's coordinates,
    which are the ones the overlay draws in, so the square lands under the
    pointer at any window size. *)

open Raycaster
open Result_ext
open Tsdl

let height = 4.

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
  (* This demo's table from controls to what it calls them. *)
  let interact = Input.Key Sdl.Scancode.e
  and screen = Input.Key Sdl.Scancode.tab
  and primary = Input.Button Input.Left in
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
    pointer = actions.Input.pointer;
  }

let overlay fb state =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let unit = Int.max 3 (height / 60) in
  let margin = 2 * unit in
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

let run () =
  let+ _, ending =
    Engine.run_state ~escape:true ~update
      ~view:(fun state -> (world, state.player))
      ~overlay
      ~pointing:(fun state -> state.pointing)
      start
  in
  ending
