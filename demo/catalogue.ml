(** Every demo the executable can run.

    One small world per engine feature, rather than one world with every feature
    in it. {!Level} answers "what can this thing do"; these answer "how is that
    one thing done", and each is a file short enough to read in a sitting with
    the feature it demonstrates as the only thing in it.

    A demo carries the world it starts from as well as the function that runs
    it, so the test suite can check every one of them without opening a window.

    Adding a demo is adding it in four places: its file in this directory, an
    entry in {!demos} below — which also enrols it in [test_demos] — a row in
    README.md's table, and a line on [doc/demo/index.mld]. *)

(* Not `open Camlcast`: this file names every demo module, and one of them is
   called Overlay — as is the module in camlcast that turns a described HUD into
   pixels. Qualifying the two things wanted from it is cheaper than renaming a
   demo after a collision nobody outside this file will ever have. *)
open Camlcast_core

type t = {
  name : string;  (** what you type after [camlcast-demo] *)
  blurb : string;  (** one line, for the listing *)
  world : World.t Lazy.t;
      (** what it starts from; [endless] then grows it.

          Behind a [lazy] because [loading] builds its world out of files, which
          can fail. Eager, one missing picture would stop [--list] from listing
          anything; deferred, it stops only the demo that needed it. *)
  run : Camlcast.Run.window -> (Camlcast.Run.ending, [ `Msg of string ]) result;
      (** plays it on the launcher's window, and says how the player left it —
          {!Menu} shows itself again on that same window after a demo that was
          [Returned], and stops after one that was [Closed] *)
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
      (* The level at rest, which is what the suites check: [Level.run] starts
         from this and then has a state, but nothing it does to a world is
         anything a world could not have been authored as. *)
      world = lazy Level.default;
      run = Level.run;
    };
  ]

let find name = List.find_opt (fun demo -> demo.name = name) demos

(** Run one demo, turning the only exception its art can raise back into the
    [`Msg] channel the launcher answers on.

    A world read off the disk is forced inside {!t.run} — deep in a frame, where
    a result would have nowhere to go — so {!Loading}, {!Typeface} and {!Text}
    raise {!Reading.Unreadable} instead, and
    {!Camlcast_core.Result_ext.with_resource} deliberately lets an exception
    through rather than making an [Error] of it. Between the two the launcher
    had nothing to say: a missing picture printed OCaml's own fatal-error banner
    and stopped with its exit code, past the [camlcast-demo:] prefix every other
    failure wears and past the code that goes with it. This is where the raising
    side ends and the reporting side starts, and it takes a thunk rather than
    wrapping {!t.run} so that the seam can be tested without a window.

    One exception and ours, so this catches what it meant to. It caught
    [Failure] while the loaders raised it with [failwith], and [Failure] belongs
    to nobody: a [List.nth] off the end of a list, anywhere inside a demo's
    frame, arrived here dressed as a demo whose art could not be read — reported
    calmly, under a message naming a file that was never the trouble, and with
    the real mistake nowhere in it. It now goes out as itself.

    [Invalid_argument] is still not caught, and is the other kind of mistake — a
    world that does not join up, a font atlas the wrong shape. Stopping with one
    of those named is the honest report of it. *)
let attempt f =
  try f () with Reading.Unreadable message -> Error (`Msg message)
