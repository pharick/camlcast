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
    reports the difference back through {!Camlcast.Engine.ending} so that the
    launcher can show the list again after a demo but stop altogether when the
    player has shut the window. *)

open Camlcast
open Result_ext

let demos = Array.of_list Catalogue.demos

(** The one room the list is drawn over. Nothing in it is read from a file — the
    three surfaces are generated from {!Patterns} — so unlike a demo's world it
    cannot fail to be built, and the menu has nothing to fall back to. *)
let backdrop =
  let height = 4. in
  let sw = Vec.make (-4.) (-4.)
  and se = Vec.make 4. (-4.)
  and ne = Vec.make 4. 4.
  and nw = Vec.make (-4.) 4. in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:(Room.floor ~plane:floor ~material:Surfaces.ground)
      ~ceiling:
        (Room.roof ~plane:(Plane.above floor height) ~material:Surfaces.soffit)
      [ wall sw se; wall se ne; wall ne nw; wall nw sw ]
  in
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make 0. 0.)

type t = {
  selected : int;
  chosen : int option;  (** set by Enter, and the whole of [finished] *)
  player : Player.t;  (** turning on the spot in {!backdrop} *)
}

(** How fast the backdrop turns, in radians per second. Slow enough to read the
    list over. *)
let turn_rate = 0.25

let start = { selected = 0; chosen = None; player = Player.spawn backdrop }

(** Where the list opens. [None] is the top, which is where a launcher starts.
    [Some demo] is that demo's own row, so that coming back from one lands on
    what was just played rather than sending the player down the list again to
    find their place.

    A demo the catalogue does not name opens at the top. Nothing can hand one
    over that it did not first take from {!Camlcast_demo.Catalogue.demos}, so
    this is a total function rather than a case anybody has to think about. *)
let start_on = function
  | None -> start
  | Some (demo : Catalogue.t) ->
      let rec find i =
        if i >= Array.length demos then start
        else if demos.(i).Catalogue.name = demo.Catalogue.name then
          { start with selected = i }
        else find (i + 1)
      in
      find 0

let update state ~dt ~motion:_ ~actions =
  let count = Array.length demos in
  let moved =
    if Input.pressed actions (Input.Key Key.down) then 1
    else if Input.pressed actions (Input.Key Key.up) then -1
    else 0
  in
  (* Wraps, so a long list is reachable from either end. *)
  let selected = (state.selected + moved + count) mod count in
  let taken =
    List.exists
      (fun key -> Input.pressed actions (Input.Key key))
      [ Key.return; Key.kp_enter; Key.space ]
  in
  {
    selected;
    chosen = (if taken then Some selected else None);
    player =
      Engine.step backdrop state.player
        { Input.still with Input.turn = turn_rate *. dt };
  }

let view state = (backdrop, state.player)

(** The list, over a curtain dark enough to read against whatever world is
    turning behind it.

    Only as many rows as the buffer has room for are drawn, and the window
    slides to keep the selection inside it. The framebuffer is a fraction of the
    window ({!Camlcast.Renderer.internal_size}) and shrinks with it, so "they
    all fit" is true at the size this opens at and not a thing to rely on. *)
let overlay font fb state =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let line = font.Font.height + 2 in
  let margin = line in
  Paint.rect fb ~x:0 ~y:0 ~w:width ~h:height ~color:(Color.rgb 0 0 0)
    ~alpha:170;
  let ink = Color.rgb 200 200 200 in
  let bright = Color.rgb 255 255 255 in
  let dim = Color.rgb 140 140 140 in
  Font.draw fb font "camlcast-demo" ~x:margin ~y:margin ~color:bright;
  let top = margin + (2 * line) in
  let bottom = margin + (2 * line) in
  let rows = Int.max 1 ((height - top - bottom) / line) in
  let count = Array.length demos in
  let first =
    if count <= rows then 0
    else Int.max 0 (Int.min (count - rows) (state.selected - (rows / 2)))
  in
  for row = 0 to Int.min rows (count - first) - 1 do
    let index = first + row in
    let demo = demos.(index) in
    let y = top + (row * line) in
    let here = index = state.selected in
    if here then
      Paint.rect fb ~x:(margin / 2) ~y:(y - 1) ~w:(width - margin) ~h:line
        ~color:(Color.rgb 70 90 120) ~alpha:220;
    Font.draw fb font
      (Printf.sprintf "%-9s %s" demo.Catalogue.name demo.Catalogue.blurb)
      ~x:margin ~y
      ~color:(if here then bright else ink)
  done;
  (* Which way there is more list, for a window too short to show all of it. *)
  if first > 0 then Font.draw fb font "^" ~x:(width - margin) ~y:top ~color:dim;
  if first + rows < count then
    Font.draw fb font "v" ~x:(width - margin)
      ~y:(top + ((rows - 1) * line))
      ~color:dim;
  Font.draw fb font
    (Printf.sprintf "%s %s  choose      %s  run      %s  quit" (Key.name Key.up)
       (Key.name Key.down) (Key.name Key.return) (Key.name Key.escape))
    ~x:margin
    ~y:(height - margin - font.Font.height)
    ~color:dim

(** Show the list on [window] and wait. [None] means the player wants no demo at
    all — Escape, or the window shut — and either way the launcher stops.

    [from] is the demo just played, if there was one: the list opens on it. See
    {!start_on}. *)
let choose ?from window =
  let* font = Typeface.load () in
  let+ state, ending =
    Engine.run window
      (Engine.game ~update ~view ~overlay:(overlay font)
         ~finished:(fun state -> state.chosen <> None)
         ~bindings:Bindings.escapable ())
      (start_on from)
  in
  match ending with
  | Engine.Closed -> None
  | Engine.Left -> Option.map (fun index -> demos.(index)) state.chosen
