(** Every demo the executable can run.

    One small world per engine feature, rather than one world with every feature
    in it. {!Level} answers "what can this thing do"; these answer "how is that
    one thing done", and each is a file short enough to read in a sitting with
    the feature it demonstrates as the only thing in it.

    A demo carries the world it starts from as well as the function that runs
    it, so the test suite can check every one of them without opening a window.
*)

open Raycaster

type t = {
  name : string;  (** what you type after [camlcast-demo] *)
  blurb : string;  (** one line, for the listing *)
  world : World.t Lazy.t;
      (** what it starts from; [endless] then grows it.

          Behind a [lazy] because [loading] builds its world out of files, which
          can fail. Eager, one missing picture would stop [--list] from listing
          anything; deferred, it stops only the demo that needed it. *)
  run : unit -> (unit, [ `Msg of string ]) result;
}

let demos =
  [
    {
      name = "masonry";
      blurb = "materials: one pattern function, applied at several colours";
      world = lazy Masonry.world;
      run = Masonry.run;
    };
    {
      name = "gallery";
      blurb = "decals on the walls, sprites standing in the room";
      world = lazy Gallery.world;
      run = Gallery.run;
    };
    {
      name = "glass";
      blurb = "see-through walls and the translucent pass";
      world = lazy Glass.world;
      run = Glass.run;
    };
    {
      name = "slopes";
      blurb = "inclined floors and roofs, and a seamless threshold";
      world = lazy Slopes.world;
      run = Slopes.run;
    };
    {
      name = "daylight";
      blurb = "the open sky, and two rooms under different ones";
      world = lazy Daylight.world;
      run = Daylight.run;
    };
    {
      name = "haze";
      blurb = "atmosphere: the fade into fog and where the light falls";
      world = lazy Haze.world;
      run = Haze.run;
    };
    {
      name = "portals";
      blurb = "doorways: the same room, joined in two places";
      world = lazy Portals.world;
      run = Portals.run;
    };
    {
      name = "changing";
      blurb = "replacing a room: a sign that moves, rebuilt every frame";
      world = lazy Changing.world;
      run = Changing.run;
    };
    {
      name = "floating";
      blurb = "sprites off the floor, and frames chosen rather than made";
      world = lazy Floating.world;
      run = Floating.run;
    };
    {
      name = "dust";
      blurb = "a chamber of falling dust: every mote moved every frame";
      world = lazy Dust.world;
      run = Dust.run;
    };
    {
      name = "chalk";
      blurb = "marking a wall where the crosshair is, on the face you see";
      world = lazy Chalk.world;
      run = Chalk.run;
    };
    {
      name = "endless";
      blurb = "the grow hook: a corridor built as you walk it";
      world = lazy Endless.world;
      run = Endless.run;
    };
    {
      name = "doors";
      blurb = "doors that open and shut, on both sides of the link at once";
      world = lazy Doors.world;
      run = Doors.run;
    };
    {
      name = "targets";
      blurb = "what the crosshair is on, through the doorway in front of you";
      world = lazy Targets.world;
      run = Targets.run;
    };
    {
      name = "trail";
      blurb = "traversal traces: a return route built from the doorways";
      world = lazy Trail.world;
      run = Trail.run;
    };
    {
      name = "phases";
      blurb = "run_state: a phase, a clock, and a light going out";
      world = lazy Phases.world;
      run = Phases.run;
    };
    {
      name = "overlay";
      blurb = "drawing over the finished world";
      world = lazy Overlay.world;
      run = Overlay.run;
    };
    {
      name = "controls";
      blurb = "press versus hold, buttons, and letting go of the mouse";
      world = lazy Controls.world;
      run = Controls.run;
    };
    {
      name = "text";
      blurb = "a bitmap font: wrapping, measuring, clipping and colour";
      world = lazy Text.world;
      run = Text.run;
    };
    {
      name = "loading";
      blurb = "art read from files, beside the generated kind";
      world = Loading.world;
      run = Loading.run;
    };
    {
      name = "showcase";
      blurb = "the five-room level, with all of the above at once";
      world = lazy Level.default;
      run = (fun () -> Engine.run Level.default);
    };
  ]

let find name = List.find_opt (fun demo -> demo.name = name) demos
