(** Every demo the executable can run.

    One small world per engine feature. {!Level} shows everything at once; each
    of the others isolates one feature in a single short file.

    The demos are content, not engine: nothing in the engine depends on this
    package, and installing the engine installs none of it. They stay in this
    repository because together they exercise every engine feature, so an engine
    change that breaks one breaks a world you can walk through.

    A demo carries the world it starts from as well as the function that runs
    it, so the test suite can check every one without opening a window.

    Adding a demo means four changes: its file in this directory, an entry in
    {!demos} below (which also enrols it in [test_demos]), a row in README.md's
    table, and a line on [doc/demo/index.mld]. *)

(* Not `open Camlcast`: this file names every demo module, and one of them is
   called Overlay, colliding with camlcast's module of that name. Qualifying
   the two values needed from Camlcast is cheaper than renaming a demo. *)
open Camlcast_core

type t = {
  name : string;  (** the argument to [camlcast-demo] *)
  blurb : string;
      (** one line, for the listing. README.md's table and [doc/demo/index.mld]
          copy it verbatim, and [test_demos] checks that they still do. *)
  world : World.t Lazy.t;
      (** the starting world; [endless] then grows it.

          Lazy because [loading] builds its world out of files, which can fail.
          Forced eagerly, one missing picture would stop [--list] from listing
          anything; deferred, it stops only the demo that needed it. *)
  run : Camlcast.Run.window -> (Camlcast.Run.ending, [ `Msg of string ]) result;
      (** plays the demo on the launcher's window and reports how the player
          left it: {!Menu} shows itself again on the same window after
          [Returned], and stops after [Closed]. *)
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
      blurb = "the extend hook: a corridor built as you walk it";
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
      name = "barred";
      blurb = "a door and a transom you can see through, and cannot walk past";
      world = lazy Barred.world;
      run = Barred.run;
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
      blurb = "a component with a phase, a clock, and a light going out";
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
      blurb = "binding keys, press versus hold, and letting go of the mouse";
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
      (* The level at rest, which is what the suites check. [Level.run] starts
         from this and then has a state, but everything it does to the world
         could have been authored directly. *)
      world = lazy Level.default;
      run = Level.run;
    };
  ]

let find name = List.find_opt (fun demo -> demo.name = name) demos

(** Run one demo, converting {!Reading.Unreadable} — the only exception its art
    can raise — into the [`Msg] channel the launcher reports on.

    A world read off the disk is forced inside {!t.run}, deep in a frame where a
    [result] has nowhere to go, so {!Loading}, {!Typeface} and {!Text} raise
    {!Reading.Unreadable}, and {!Camlcast_core.Result_ext.with_resource}
    deliberately lets exceptions through rather than making an [Error] of them.
    Without this handler a missing picture printed OCaml's fatal-error banner
    and stopped with its exit code, bypassing the [camlcast-demo:] prefix and
    exit code every other failure gets. This is the seam between the raising
    side and the reporting side; it takes a thunk rather than wrapping {!t.run}
    so the seam can be tested without a window.

    Only {!Reading.Unreadable} is caught, an exception this package owns. An
    earlier version caught [Failure] (the loaders used [failwith]), and
    [Failure] belongs to nobody: a [List.nth] off the end of a list, anywhere
    inside a demo's frame, was reported as unreadable art under a message naming
    a file that was not the problem.

    [Invalid_argument] is deliberately not caught. It marks the other kind of
    mistake — a world that does not join up, a font atlas the wrong shape — and
    should stop the program with the exception named. *)
let attempt f =
  try f () with Reading.Unreadable message -> Error (`Msg message)
