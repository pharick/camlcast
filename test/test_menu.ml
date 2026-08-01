(** The launcher's list, driven without a window.

    It used to drive {!Camlcast_demo.Menu.update}, a pure function of the state
    and one frame's input. The list is a component now and its cursor is its
    own, so what is driven here is a mount and what is read back is the row the
    list {e highlights} — which is what a player sees, and which turns out to be
    a better thing to assert than an index was: these cases name demos.

    Only the drawing needs a window, and the drawing is the part with nothing to
    decide. *)

open Camlcast_core
open Camlcast
open Camlcast_demo
open Support

let tick = 1. /. 60.
let count = List.length Catalogue.demos
let highlight = Color.rgb 70 90 120

let font =
  match Typeface.load () with
  | Ok font -> font
  | Error (`Msg m) -> failwith ("the menu test could not read its font: " ^ m)

(* Successive frames on one mount, each with whatever is held down — carried
   from the last, so a key held over two frames reads as held on the second and
   not as pressed again. *)
type driver = {
  mount : Mount.t;
  mutable actions : Input.actions;
  chosen : Catalogue.t option ref;
  description : P.t;
}

let driving ?from () =
  let chosen = ref None in
  {
    mount = Mount.create ();
    actions = Input.untouched;
    chosen;
    description = Menu.list (font, from, chosen);
  }

let play ?(held = []) driver =
  driver.actions <-
    Input.advance driver.actions
      ~down:(fun control -> List.mem control held)
      ~mouse:(0., 0.) ~pointer:(0, 0) ~dt:tick;
  Mount.render driver.mount
    (Element.provide Events.context
       { Events.still with Events.dt = tick; actions = driver.actions }
       [ driver.description ])

let holding key = [ Input.Key key ]

(* Press a key, then render again. A handler runs after the frame it was
   triggered on, so the frame the key went down on still shows what was
   selected before it — see Camlcast.Hook for why that is the rule. The old
   test drove a pure update that answered immediately; this is the one place
   where the difference is visible, and waiting a frame is what a player does
   without noticing. *)
let press key driver =
  ignore (play ~held:(holding key) driver);
  play driver

(** Which demo the list is offering: the row it has drawn a highlight behind,
    read back by finding the text drawn at the same height. *)
let showing (scene : Scene.t) =
  match
    List.find_map
      (function
        | Prim.Rect { y; color; _ } when color = highlight -> Some y | _ -> None)
      scene.Scene.hud
  with
  | None -> None
  | Some y ->
      List.find_map
        (function
          | Prim.Text { y = row; text; _ } when row = y + 1 ->
              Some (List.hd (String.split_on_char ' ' text))
          | _ -> None)
        scene.Scene.hud

let named index = (List.nth Catalogue.demos index).Catalogue.name

let selection =
  [
    case "opens on the first demo" (fun () ->
        Alcotest.(check (option string))
          "the top row"
          (Some (named 0))
          (showing (play (driving ()))));
    case "coming back from a demo opens on it" (fun () ->
        (* The launcher plays a demo and shows the list again, and the list has
           to land on what was just played rather than sending the player down
           it again to find their place. *)
        let third = List.nth Catalogue.demos 2 in
        Alcotest.(check (option string))
          "that demo's own row" (Some third.Catalogue.name)
          (showing (play (driving ~from:third ()))));
    case "a demo the catalogue does not name opens at the top" (fun () ->
        let stranger =
          { (List.nth Catalogue.demos 0) with Catalogue.name = "nowhere" }
        in
        Alcotest.(check (option string))
          "the top row"
          (Some (named 0))
          (showing (play (driving ~from:stranger ()))));
    case "down moves to the next" (fun () ->
        let driver = driving () in
        ignore (play driver);
        Alcotest.(check (option string))
          "the second row"
          (Some (named 1))
          (showing (press Key.down driver)));
    case "up from the first wraps to the last" (fun () ->
        let driver = driving () in
        ignore (play driver);
        Alcotest.(check (option string))
          "the bottom row"
          (Some (named (count - 1)))
          (showing (press Key.up driver)));
    case "a held key moves once, not every frame" (fun () ->
        let driver = driving () in
        ignore (play driver);
        (* Three frames with it held: the first is a press and the other two are
           not, so the cursor moves once. *)
        ignore (play ~held:(holding Key.down) driver);
        ignore (play ~held:(holding Key.down) driver);
        ignore (play ~held:(holding Key.down) driver);
        Alcotest.(check (option string))
          "still the second row"
          (Some (named 1))
          (showing (play driver)));
  ]

let choosing =
  [
    case "nothing is chosen by standing still" (fun () ->
        let driver = driving () in
        ignore (play driver);
        ignore (play driver);
        Alcotest.(check bool) "nothing" true (!(driver.chosen) = None));
    case "enter settles on what is highlighted" (fun () ->
        let driver = driving () in
        ignore (play driver);
        ignore (press Key.down driver);
        ignore (play ~held:(holding Key.return) driver);
        Alcotest.(check (option string))
          "the second demo"
          (Some (named 1))
          (Option.map
             (fun (d : Catalogue.t) -> d.Catalogue.name)
             !(driver.chosen)));
    case "and says the run is over" (fun () ->
        let driver = driving () in
        ignore (play driver);
        Alcotest.(check bool) "not yet" false (play driver).Scene.finished;
        Alcotest.(check bool)
          "and now" true (press Key.return driver).Scene.finished);
  ]

let backdrop =
  [
    case "the backdrop turns on its own" (fun () ->
        let driver = driving () in
        let facing scene = (Option.get scene.Scene.camera).Player.dir in
        let first = facing (play driver) in
        for _ = 1 to 30 do
          ignore (play driver)
        done;
        Alcotest.(check bool)
          "it is not where it started" true
          (Vec.length (Vec.sub first (facing (play driver))) > 0.01));
    case "and the list is drawn over a room" (fun () ->
        let scene = play (driving ()) in
        Alcotest.(check int)
          "one room, four walls" 4
          (Room.wall_count (World.room scene.Scene.world 0)));
  ]

let () =
  Alcotest.run "Menu"
    [ ("selection", selection); ("choosing", choosing); ("backdrop", backdrop) ]
