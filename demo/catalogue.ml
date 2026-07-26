(** Every demo the executable can run.

    One small world per engine feature, rather than one world with every feature
    in it. {!Level} answers "what can this thing do"; these answer "how is that
    one thing done", and each is a file short enough to read in a sitting with
    the feature it demonstrates as the only thing in it.

    A demo carries the world it starts from as well as the function that runs
    it, so the test suite can check every one of them without opening a
    window. *)

open Raycaster

type t = {
  name : string;  (** what you type after [camlcast-demo] *)
  blurb : string;  (** one line, for the listing *)
  world : World.t;  (** what it starts from; [endless] then grows it *)
  run : unit -> (unit, [ `Msg of string ]) result;
}

let demos =
  [
    {
      name = "masonry";
      blurb = "materials: a colour and a pattern, chosen apart";
      world = Masonry.world;
      run = Masonry.run;
    };
    {
      name = "gallery";
      blurb = "decals on the walls, sprites standing in the room";
      world = Gallery.world;
      run = Gallery.run;
    };
    {
      name = "glass";
      blurb = "see-through walls and the translucent pass";
      world = Glass.world;
      run = Glass.run;
    };
    {
      name = "slopes";
      blurb = "inclined floors and roofs, and a seamless threshold";
      world = Slopes.world;
      run = Slopes.run;
    };
    {
      name = "daylight";
      blurb = "the open sky, and two rooms under different ones";
      world = Daylight.world;
      run = Daylight.run;
    };
    {
      name = "haze";
      blurb = "atmosphere: the fade into fog and where the light falls";
      world = Haze.world;
      run = Haze.run;
    };
    {
      name = "portals";
      blurb = "doorways: the same room, joined in two places";
      world = Portals.world;
      run = Portals.run;
    };
    {
      name = "changing";
      blurb = "replacing a room: a sign that moves, rebuilt every frame";
      world = Changing.world;
      run = Changing.run;
    };
    {
      name = "endless";
      blurb = "the grow hook: a corridor built as you walk it";
      world = Endless.world;
      run = Endless.run;
    };
    {
      name = "trail";
      blurb = "traversal traces: a return route built from the doorways";
      world = Trail.world;
      run = Trail.run;
    };
    {
      name = "phases";
      blurb = "run_state: a phase, a clock, and a light going out";
      world = Phases.world;
      run = Phases.run;
    };
    {
      name = "overlay";
      blurb = "drawing over the finished world";
      world = Overlay.world;
      run = Overlay.run;
    };
    {
      name = "controls";
      blurb = "press versus hold, buttons, and letting go of the mouse";
      world = Controls.world;
      run = Controls.run;
    };
    {
      name = "showcase";
      blurb = "the five-room level, with all of the above at once";
      world = Level.default;
      run = (fun () -> Engine.run Level.default);
    };
  ]

let find name = List.find_opt (fun demo -> demo.name = name) demos
