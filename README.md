# CamlCast — a raycasting engine

[![CI](https://github.com/pharick/camlcast/actions/workflows/ci.yml/badge.svg)](https://github.com/pharick/camlcast/actions/workflows/ci.yml)
[![Release](https://github.com/pharick/camlcast/actions/workflows/release.yml/badge.svg)](https://github.com/pharick/camlcast/actions/workflows/release.yml)
[![Docs](https://img.shields.io/badge/docs-pharick.github.io%2Fcamlcast-blue)](https://pharick.github.io/camlcast/)
[![OCaml](https://img.shields.io/badge/OCaml-%E2%89%A5%205.2-ec6813)](https://ocaml.org)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

![A walk through the showcase level: out of the twelve-sided plaza under an open sky, into a hall whose roof climbs away from its floor, through a door into a cellar with dust turning in it, back across the plaza and into a garden under a later sky, with a grille gate pulled down behind](doc/images/tour.gif)

A first-person raycasting engine in OCaml on top of SDL2 (`tsdl`). A world is a
graph of rooms built from walls at any angle, each authored in its own coordinate
frame with its own inclined floor and its own ceiling or open sky, joined at
doorways you both see through and walk through. Walls carry their own heights and
materials, can be see-through, and can be hung with pictures; sprites stand in
the world facing the player; and there is mouse look with pitch. The floor,
ceiling and sky are cast per pixel by a small software renderer and the walls are
painted over them back to front.

This repository is the engine and the demos it is written against. Where to
start depends on what you came for:

- **Play the demos** — [Running](#running) below, or download a
  [bundle](#bundles) and install nothing.
- **Build a game on the engine** —
  [Making a game on CamlCast](https://pharick.github.io/camlcast/making-a-game.html):
  from an empty directory to a game, one feature at a time.
- **Learn how a raycaster draws** —
  [Building the engine from scratch](https://pharick.github.io/camlcast/building-the-engine.html):
  the picture rebuilt by hand, every derivation written out.
- **Hack on the engine itself** — [the modules](#the-engine-in-one-page),
  [Tests](#tests), and [HACKING.md](HACKING.md).

## What it looks like

<!-- Raw HTML because a markdown table has to have a header row, and this one has
     nothing to say in it. The widths are attributes rather than a stylesheet
     because GitHub strips those from a README, and without them a pair of
     1024-pixel screenshots would size the table past the column. -->
<table width="100%">
  <tr>
    <td width="50%"><img src="doc/images/daylight.png" width="100%" alt="An open evening sky over a low wall"></td>
    <td width="50%"><img src="doc/images/slopes.png" width="100%" alt="A tiled floor climbing to a raised doorway"></td>
  </tr>
  <tr>
    <td><img src="doc/images/portals.png" width="100%" alt="Two doorways showing one room"></td>
    <td><img src="doc/images/gallery.png" width="100%" alt="Pictures hung on a wall, figures standing in front of them"></td>
  </tr>
  <tr>
    <td><img src="doc/images/glass.png" width="100%" alt="The next room seen through a grille and a tinted window"></td>
    <td><img src="doc/images/haze.png" width="100%" alt="A corridor of pillars fading into its own haze"></td>
  </tr>
  <tr>
    <td><img src="doc/images/dust.png" width="100%" alt="A chamber of falling dust motes"></td>
    <td><img src="doc/images/text.png" width="100%" alt="A page of wrapped and clipped text over a room"></td>
  </tr>
</table>

## Running

From a fresh clone, once. The system libraries come first — SDL2 and its image
codecs are the one dependency opam cannot build for you:

```sh
sudo apt install libsdl2-dev libsdl2-image-dev   # Debian / Ubuntu
brew install sdl2 sdl2_image                     # macOS — and see the note below

opam switch create . 5.5.0 --no-install          # this repo uses a local switch
eval $(opam env --switch=. --set-switch)
opam install . --deps-only --with-test --with-doc
```

**macOS:** add `--no-depexts` to the `opam install` line. Homebrew ships
`sdl2-compat` under the name `sdl2`, which opam's dependency check cannot see;
installing the libraries yourself and telling opam to stop looking is the whole
fix. **Windows:** use the MSYS2 environment. Step 0 of
[the guide](https://pharick.github.io/camlcast/making-a-game.html) has the
longer story on both.

Then:

```sh
dune exec camlcast-demo                    # the list, on screen
dune exec camlcast-demo -- --list          # the same list, printed
dune exec camlcast-demo portals            # straight to one of them
dune test                                  # all suites
```

Run with no arguments, `camlcast-demo` opens a menu of every demo, drawn over
one slowly turning room: the arrow keys move the highlight, Enter (or Space)
runs it, Escape comes back to the list. Run with a demo's name, it launches
straight into that demo — Escape then ends the program, because there is no
menu behind it to come back to. (In a later shell,
`eval $(opam env --switch=. --set-switch)` from the repository puts the switch
back on your path.)

The demos that read art from files find it relative to the executable, which
under dune is `_build/default/assets/` — `dune build` puts it there. Installed,
it is `share/camlcast-demo/assets/` under the same prefix as the binary: the
share directory is named after the executable, so a game called `wanderer` reads
`share/wanderer`. Set `CAMLCAST_ASSETS` to a directory to look there instead,
and only there.

## The demos

One small world per engine feature, and one that has all of them at once. Each is
a single file under `demo/`, short enough to read in a sitting, with the feature
it demonstrates as the only thing in it — and each spawns you facing the thing it
is about.

| demo       | what it shows                                                  |
| ---------- | -------------------------------------------------------------- |
| `masonry`  | materials: one pattern function, applied at several colours    |
| `gallery`  | decals on the walls, sprites standing in the room              |
| `glass`    | see-through walls and the translucent pass                     |
| `slopes`   | inclined floors and roofs, and a seamless threshold            |
| `daylight` | the open sky, and two rooms under different ones               |
| `haze`     | atmosphere: the fade into fog and where the light falls        |
| `portals`  | doorways: the same room, joined in two places                  |
| `changing` | replacing a room: a sign that moves, rebuilt every frame       |
| `floating` | sprites off the floor, and frames chosen rather than made      |
| `dust`     | a chamber of falling dust: every mote moved every frame        |
| `chalk`    | marking a wall where the crosshair is, on the face you see     |
| `endless`  | the `extend` hook: a corridor built as you walk it             |
| `doors`    | doors that open and shut, on both sides of the link at once    |
| `barred`   | a door and a transom you can see through, and cannot walk past |
| `targets`  | what the crosshair is on, through the doorway in front of you  |
| `trail`    | traversal traces: a return route built from the doorways       |
| `phases`   | `Engine.run`: a phase, a clock, and a light going out          |
| `overlay`  | drawing over the finished world                                |
| `controls` | binding keys, press versus hold, and letting go of the mouse   |
| `text`     | a bitmap font: wrapping, measuring, clipping and colour        |
| `loading`  | art read from files, beside the generated kind                 |
| `showcase` | the five-room level, with all of the above at once             |

`demo/catalogue.ml` is the list itself, and `showcase` is `demo/level.ml`. Every
one of these worlds is checked by `test_demos`: that you can stand where it
spawns you, that its rooms enclose themselves, that every room is reachable, and
that no floor steps across a doorway. Adding a demo to `Catalogue.demos` is also
adding it to that suite.

## The libraries

The engine holds no content — not one colour, pattern, picture or room. What it
has instead are the types those things are values of, so a game supplies its own
and two games can share an engine without sharing a look.

| directory | library         | what it is                                                      |
| --------- | --------------- | --------------------------------------------------------------- |
| `lib/`    | `camlcast`      | **what a game opens**: the parts a world is described with       |
| `loom/`   | `camlcast.loom` | the declarative runtime: elements, reconciling, hooks, store     |
| `core/`   | `camlcast.core` | the platform: geometry, ray casting, rendering, SDL              |
| `demo/`   | `camlcast-demo` | the demos and the art they are made of, run by `camlcast-demo`  |

A game opens `Camlcast` and nothing else. What is not in that module is the
platform underneath — `Engine`, `Renderer`, `Framebuffer`, `World`, `Player` —
which is reachable by adding `camlcast.core` to a dune file and saying so. The
boundary is a decision with a diff rather than something autocomplete finds for
you.

`camlcast.loom` depends on nothing but the standard library and knows nothing of
walls; `camlcast` is the only library that knows both it and the platform. The
guides and the demos teach the layer: `camlcast.core` is what a game reaches for
when it genuinely needs a `World` or a `Renderer`, and the rest of the time it
is the floor under the room rather than the room.

Nothing in the engine depends on `demo/`, which is the point: it is content, and
it lives outside the library it is content for. It stays in this repository
because between them the demos exercise every corner of the engine — decals,
see-through walls, sloped floors, the open sky, growth, overlays and input — so a
change that breaks any of them breaks something you can walk through here.

They are two opam packages for the same reason. `camlcast` is the engine: a
library that reads no file and puts nothing in a prefix's `share`. `camlcast-demo`
is `bin/demo.ml` and the pictures it needs, which a program that reads its art
off the disk has to carry wherever it is installed.

## Using the engine

Pin it, since it is not on opam yet, and add `(libraries camlcast)` to your
`dune`:

```sh
opam pin add camlcast git+https://github.com/pharick/camlcast.git
```

(The demos are the second package and depend on the first, so a copy of them
wants both pinned — `opam install .` from a checkout does that in one step.)

A game **describes** its world: it says what the world should be right now,
every frame, from nothing, and the runtime works out what changed. A level is
OCaml code rather than a file in some format, so the smallest complete game is
one room and a call. This is
[`examples/described_room.ml`](examples/described_room.ml), compiled with the
rest of the tree so that it cannot drift from the engine:

```ocaml
open Camlcast

(* A pattern is a pure function from a texel coordinate to a colour. This one is
   a check; Color.level scales all three channels together, so it moves the
   brightness without touching the hue. *)
let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let height = 4.
let ground_plane = Plane.horizontal 0.

let level =
  (* P holds the parts a world is made of, and is written round a description
     rather than opened over the file: its names are short and ordinary, so a
     local open puts them exactly where a world is being written. *)
  P.(
    world ~atmosphere:Atmosphere.default ~spawn:("room", Vec.make (-4.5) 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:ground_plane ~material:ground)
          ~ceiling:
            (roof ~plane:(Plane.above ground_plane height) ~material:stone)
          [
            (* The corners in whichever order reads best. boundary measures the
               loop and winds it so the room is on the inside, so a boundary
               wound the wrong way round — and a room black from within —
               cannot be written down. *)
            boundary ~height ~material:stone
              (corners
                 [
                   Vec.make (-6.) (-6.);
                   Vec.make 6. (-6.);
                   Vec.make 6. 6.;
                   Vec.make (-6.) 6.;
                 ]);
          ];
      ])

let () =
  match Run.play ~title:"A described room" level with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
```

Distances are in cells: one cell is one texture repeat, and the eye stands half
a cell up, so a 12-cell room under a 4-cell ceiling reads as a hall.

A part of a world that needs to remember something is a **component** — a
function from props to a description, holding state with hooks:

```ocaml
let torch =
  Element.declare ~name:"torch" @@ fun pos ->
  let lit, set_lit = Hook.use_state true in
  Events.use_pressed (Input.Key Key.e) (fun () -> set_lit (not lit));
  P.sprite ~size:0.8 ~image:(if lit then flame else stub) pos
```

Components compose by being functions, so a higher-order component that takes
children and puts something around them is ordinary OCaml.
**[Making a game on CamlCast](https://pharick.github.io/camlcast/making-a-game.html)**
walks through all of it a feature at a time, with the demo that isolates each
one. The complete programs beside `described_room.ml` are in
[`examples/`](examples/) — including `game.ml` and `described_fuse.ml`, which
are the same small game written against the platform and against the layer, and
are worth reading side by side.

## Controls

The engine names no key of its own. Walking, looking, fullscreen and leaving the
run all come out of a `Binding.t` the game hands to `Engine.run`;
`Binding.default` is what the demos walk on, and it is a default and not a rule.

| key / device | action                                |
| ------------ | ------------------------------------- |
| `W` / `S`    | walk forward / back                   |
| `A` / `D`    | strafe left / right                   |
| mouse        | look around (yaw and pitch)           |
| `←` / `→`    | turn left / right (keyboard fallback) |
| `↑` / `↓`    | look up / down (keyboard fallback)    |
| `F11`        | toggle fullscreen                     |
| `Esc`        | leave the run (see below)             |

That last row is the one the engine will not assume. `Binding.default` binds
_no_ key that ends a run, because a game with screens in it wants `Esc` for
closing them; `Engine.run_world` adds it itself, since a bare world has nothing
else to end it with.

Rebinding is a value — [`examples/rebind.ml`](examples/rebind.ml) moves walking
onto `I` and `K` and adds Escape as the way out, leaving everything unsaid as
the default has it:

```ocaml
let bindings =
  Binding.make
    ~forward:
      {
        Binding.speed = 3.6 (* cells a second at full ask *);
        terms =
          [
            { Binding.source = Binding.Hold (Input.Key Key.i); weight = 1. };
            { Binding.source = Binding.Hold (Input.Key Key.k); weight = -1. };
          ];
      }
    ~leave:[ Input.Key Key.escape ] ()
```

An axis adds up terms, and the two kinds of term are added differently: a held
key is a **rate**, summed and paid out at the axis's speed over the frame, while
the mouse is a **displacement**, added as it stands. That distinction — and the
seam a gamepad would arrive through — is step 13 of
[the guide](https://pharick.github.io/camlcast/making-a-game.html).

Some demos bind keys of their own beyond the table: `phases` starts on Space;
`chalk` marks on `C` and picks the mark with `1` and `2`; `doors`, `targets`
and the showcase put `E` to work on whatever is at hand; `controls` binds a
second full set of walking keys and prints them with `Key.name`.

The mouse is captured in relative mode, so the cursor is hidden and never reaches
a screen edge.

The window is resizable and `F11` toggles borderless fullscreen. Reshaping it is
**Hor+**: the vertical field of view is fixed, so dragging the window wider
reveals more of the world to the sides instead of magnifying what was already on
screen, and pixels stay square at every shape.

## Bundles

Pushing a `v*` tag builds a bundle per platform and attaches it to a GitHub
release: a `.app` for macOS on Apple silicon and on Intel, a tarball for Linux
x64, a folder for Windows x64. Each carries its own SDL2 and image codecs, so
nothing has to be installed to run one. `tools/bundle-*.sh` build them, and can
be run on a laptop against a `dune build` tree.

A bundle opens on the list of demos, since a window that was double-clicked has
no command line behind it — that is the whole reason the list is drawn as well as
printed.

The Linux tarball needs **glibc 2.39 or newer** (Ubuntu 24.04, Debian 13,
Fedora 40). The `.app` is signed ad-hoc but not notarized, so a Mac that
downloaded it refuses the first launch: open System Settings → Privacy &
Security and choose "Open Anyway" — the Control-click trick that used to work
was removed in macOS 15 — or, once:

```sh
xattr -dr com.apple.quarantine camlcast-demo.app
```

Why the glibc floor is what it is, and how a `.app` decides how old a macOS it
runs on, is in [HACKING.md](HACKING.md).

## The engine in one page

**`core/`** is twenty-nine modules, each depending only on the ones before it:
`Config`, `Key` and `Vec` at the bottom, `Room`, `World` and `Ray` in the
middle, `Renderer`, `Clock` and `Engine` on top. The maths lives in the module
docs themselves: `Ray` for why the distance it reports is free of fish-eye,
`Plane` for the equation that casts a sloped floor per pixel, `Viewport` for the
projection and the resize rules, `Transform` for why linked doorways pair in
reverse, `World` for the portal machinery.

**`loom/`** is the declarative runtime, and depends on nothing but the standard
library: `Element` is what a component returns, `Reconcile` matches this frame's
description against last frame's tree, `Hook` keeps state in a row of slots
reached through OCaml 5 effect handlers, `Context` hands values down a subtree
with a `Type.Id` witness instead of a cast, and `Host` is the whole of the seam
— two types and one function.

**`lib/`** is what a game opens: `P` for the parts a world is made of, `Check`
for what is wrong with a level before anyone walks into it, `Aim` for what the
crosshair is on, `Run` for the loop.

The annotated list is the
**[documentation landing page](https://pharick.github.io/camlcast/)**.

## Documentation

The modules are documented with odoc comments (`(** ... *)`), and the maths
derivations live there rather than in this file. Every push to `main` publishes
them, along with all three guides:
**[pharick.github.io/camlcast](https://pharick.github.io/camlcast/)**.

- **[Making a game on CamlCast](https://pharick.github.io/camlcast/making-a-game.html)**
  — from an empty directory to a game, one feature at a time.
- **[Building the engine from scratch](https://pharick.github.io/camlcast/building-the-engine.html)**
  — how the picture is drawn, rebuilt by hand with every derivation written out.
- **[Building the layer from scratch](https://pharick.github.io/camlcast/building-the-layer.html)**
  — how a description of a world becomes one, and how a component keeps state
  across a frame that rebuilt everything. The second half of the one above.

To read them from a working tree instead:

```sh
opam install odoc            # once
dune build @doc
python3 tools/pages-site.py  # lays the tree out, and copies doc/images/ in
open _site/index.html
```

`tools/pages-site.py` is what CI publishes from, and it is needed rather than
optional:
dune's `documentation` stanza has no way to carry assets, so the screenshots the
guides are illustrated with reach the site through `tools/pages-site.py` and not
through `@doc`. Building `@doc` also prints a small **expected** set of
warnings — one per `@raise` tag naming a standard-library exception;
[HACKING.md](HACKING.md) has why they cannot be silenced, and why anything else
in that output is a reference that has gone stale.

The pictures the guides and this file are illustrated with live in `doc/images/`.

## Tests

[Alcotest](https://github.com/mirage/alcotest), a suite per engine module —
near enough: `Config` is constants, and `Door` and `Framebuffer` are exercised
through the modules built on them — plus suites for the demo package's own
machinery, all in `test/`. They share `Support`, a small library of its own in
the same directory, which holds a hand-checkable 4x4 square room, a pair of
rooms joined through a doorway, and the custom testables — a failing `Vec`
check prints `(3, 2.5)` rather than a bare `false`.

The suites are two stanzas, one per package, so that each package's tests build
from that package alone: `dune runtest` runs all of them, and the `-p` build
opam does runs only the ones belonging to the package it is building.

```sh
dune exec test/test_player.exe -- --verbose   # one suite
dune exec test/test_ray.exe -- test hits      # one group
```

**Nothing here opens a window.** `Framebuffer.offscreen` builds a buffer with no
streaming texture behind it, and `Renderer.draw_frame` fills a buffer with no SDL
call in it. So `test_paint` and `test_font` draw and read the pixels back, and
`test_renderer` renders whole frames: where a billboard lands, what a low wall
hides of it, and what a doorway trims it to are checked on the pixels rather than
only in the arithmetic that feeds them.

`test_level.ml` is the closest thing to an integration test: it checks the
showcase world the way a player meets it, so an engine change that breaks
portals, sloped floors or the sky fails there rather than in a unit suite.

## Hacking

[HACKING.md](HACKING.md) is the contributor's page: the development setup in
full, the formatting pin, what CI checks, the expected `@doc` warnings, how the
release bundles are put together, and the four places a new demo has to appear.

## License

MIT — see [LICENSE](LICENSE).
