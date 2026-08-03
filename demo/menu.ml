(** {b The list of demos, on screen.} What a bundled copy opens with, because a
    window that was double-clicked has no command line behind it and the
    executable's printed catalogue is somewhere nobody will look.

    The list is drawn over one room, turning slowly, and the same room whichever
    entry is under the cursor. Showing the highlighted demo's own world instead
    would mean building a world on every press of an arrow key, which is a
    stutter in the one place the program should feel immediate. The backdrop is
    built once, before the window opens.

    Leaving is two different things and the menu has to tell them apart. Escape
    and Enter both end this run, and so does closing the window; {!choose}
    reports the difference back through {!Camlcast_core.Engine.ending} so that
    the launcher can show the list again after a demo but stop altogether when
    the player has shut the window. *)

open Camlcast
open Camlcast_core.Result_ext

let demos = Array.of_list Catalogue.demos

(** How fast the backdrop turns, in radians per second. Slow enough to read the
    list over. *)
let turn_rate = 0.25

(** The one room the list is drawn over. Nothing in it is read from a file — the
    three surfaces are generated from {!Patterns} — so unlike a demo's world it
    cannot fail to be built, and the menu has nothing to fall back to. *)
let backdrop ~angle ~taken ~over =
  let height = 4. in
  let flat = Plane.horizontal 0. in
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("room", Vec.make 0. 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          [
            boundary ~height ~material:Surfaces.stone
              (corners
                 [
                   Vec.make (-4.) (-4.);
                   Vec.make 4. (-4.);
                   Vec.make 4. 4.;
                   Vec.make (-4.) 4.;
                 ]);
          ];
        (* The eye is the description's here rather than the runtime's: the
           backdrop turns on its own and the controls are the list's, so a walk
           nobody asked for would be a walk fighting the arrow keys. *)
        camera ~room:"room" ~pos:(Vec.make 0. 0.) ~angle ();
        hud over;
        (* Chosen: the frame it was chosen on is drawn, and then the run stops.
           Which is what ~finished used to say, said the way everything else
           here is said. *)
        (if taken then finish else Element.empty);
      ])

(** Where the list opens. [None] is the top, which is where a launcher starts.
    [Some demo] is that demo's own row, so that coming back from one lands on
    what was just played rather than sending the player down the list again to
    find their place.

    A demo the catalogue does not name opens at the top. Nothing can hand one
    over that it did not first take from {!Camlcast_demo.Catalogue.demos}, so
    this is a total function rather than a case anybody has to think about. *)
let row_of = function
  | None -> 0
  | Some (demo : Catalogue.t) ->
      let rec find i =
        if i >= Array.length demos then 0
        else if demos.(i).Catalogue.name = demo.Catalogue.name then i
        else find (i + 1)
      in
      find 0

(** The list, over a curtain dark enough to read against whatever world is
    turning behind it.

    Only as many rows as the buffer has room for are drawn, and the window
    slides to keep the selection inside it. The framebuffer is a fraction of the
    window ({!Camlcast_core.Renderer.internal_size}) and shrinks with it, so
    "they all fit" is true at the size this opens at and not a thing to rely on.

    Not (width, height): a local open of P puts a wall's height in scope. *)
let listing font ~selected ~viewport:(across, down) =
  let line = font.Font.height + 2 in
  let margin = line in
  let ink = Color.rgb 200 200 200 in
  let bright = Color.rgb 255 255 255 in
  let dim = Color.rgb 140 140 140 in
  let top = margin + (2 * line) in
  let rows = Int.max 1 ((down - top - top) / line) in
  let count = Array.length demos in
  let first =
    if count <= rows then 0
    else Int.max 0 (Int.min (count - rows) (selected - (rows / 2)))
  in
  P.(
    [
      rect ~x:0 ~y:0 ~w:across ~h:down ~color:(Color.rgb 0 0 0) ~alpha:170 ();
      text ~font ~x:margin ~y:margin ~color:bright "camlcast-demo";
    ]
    @ List.concat
        (List.init
           (Int.min rows (count - first))
           (fun row ->
             let index = first + row in
             let demo = demos.(index) in
             let y = top + (row * line) in
             let here = index = selected in
             (if here then
                [
                  rect ~x:(margin / 2) ~y:(y - 1) ~w:(across - margin) ~h:line
                    ~color:(Color.rgb 70 90 120) ~alpha:220 ();
                ]
              else [])
             @ [
                 text ~font ~x:margin ~y
                   ~color:(if here then bright else ink)
                   (Printf.sprintf "%-9s %s" demo.Catalogue.name
                      demo.Catalogue.blurb);
               ]))
    (* Which way there is more list, for a window too short to show all of it. *)
    @ (if first > 0 then
         [ text ~font ~x:(across - margin) ~y:top ~color:dim "^" ]
       else [])
    @ (if first + rows < count then
         [
           text ~font ~x:(across - margin)
             ~y:(top + ((rows - 1) * line))
             ~color:dim "v";
         ]
       else [])
    @ [
        text ~font ~x:margin
          ~y:(down - margin - font.Font.height)
          ~color:dim
          (Printf.sprintf "%s %s  choose      %s  run      %s  quit"
             (Key.name Key.up) (Key.name Key.down) (Key.name Key.return)
             (Key.name Key.escape));
      ])

(** The list itself. [chosen] is where it puts what was picked: a run says how
    it ended and not what it decided, so a description that decides something
    hands it back the way any OCaml value does. *)
let list =
  Element.declare ~name:"list" @@ fun (font, from, chosen) ->
  let selected, set_selected = Hook.use_state (row_of from) in
  let angle, set_angle = Hook.use_state 0. in
  let taken, set_taken = Hook.use_state false in
  let count = Array.length demos in
  Events.use_frame (fun ~dt -> set_angle (angle +. (turn_rate *. dt)));
  Events.use_pressed (Input.Key Key.down) (fun () ->
      set_selected ((selected + 1) mod count));
  Events.use_pressed (Input.Key Key.up) (fun () ->
      set_selected ((selected - 1 + count) mod count));
  List.iter
    (fun control ->
      Events.use_pressed control (fun () ->
          chosen := Some demos.(selected);
          set_taken true))
    [ Input.Key Key.return; Input.Key Key.kp_enter; Input.Key Key.space ];
  backdrop ~angle ~taken
    ~over:(listing font ~selected ~viewport:(Events.use_viewport ()))

(** Show the list on [window] and wait. [None] means the player wants no demo at
    all — Escape, or the window shut — and either way the launcher stops.

    [from] is the demo just played, if there was one: the list opens on it. See
    {!row_of}. *)
let choose ?from window =
  let* font = Typeface.load () in
  let chosen = ref None in
  let+ ending =
    Run.on window ~controls:Bindings.escapable (list (font, from, chosen))
  in
  match ending with Run.Closed -> None | Run.Returned -> !chosen
