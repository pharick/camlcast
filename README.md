# CamlCast — a raycasting engine

[![CI](https://github.com/pharick/camlcast/actions/workflows/ci.yml/badge.svg)](https://github.com/pharick/camlcast/actions/workflows/ci.yml)
[![Release](https://github.com/pharick/camlcast/actions/workflows/release.yml/badge.svg)](https://github.com/pharick/camlcast/actions/workflows/release.yml)
[![Docs](https://img.shields.io/badge/docs-pharick.github.io%2Fcamlcast-blue)](https://pharick.github.io/camlcast/)
[![OCaml](https://img.shields.io/badge/OCaml-%E2%89%A5%205.2-ec6813)](https://ocaml.org)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

![A walk through the showcase level: a twelve-sided plaza under an open sky, a hall with a climbing roof, a cellar with falling dust, and a garden under a different sky, with a grille gate closing behind](doc/images/tour.gif)

A first-person raycasting engine in OCaml on SDL2 (`tsdl`). A world is a graph
of rooms joined at doorways that can be seen through and walked through. Each
room is authored in its own coordinate frame, with its own inclined floor and
its own ceiling or open sky. Walls sit at any angle, carry their own heights
and materials, can be see-through, and can be hung with pictures. Sprites
stand in the world facing the player, and mouse look includes pitch. A small
software renderer casts the floor, ceiling and sky per pixel, then paints the
walls over them back to front.

This repository holds the engine and the demos it is written against. Where to
start:

- **Play the demos** — [Running](#running) below, or download a
  [bundle](#bundles) and install nothing.
- **Build a game on the engine** —
  [Making a game on CamlCast](https://pharick.github.io/camlcast/making-a-game.html):
  from an empty directory to a game, one feature at a time.
- **Learn how a raycaster draws** —
  [Building the engine from scratch](https://pharick.github.io/camlcast/building-the-engine.html):
  every derivation written out.
- **Hack on the engine itself** — [the modules](#the-engine-in-one-page),
  [Tests](#tests), and [HACKING.md](HACKING.md).

## What it looks like

<!-- Raw HTML: a markdown table requires a header row, and this table has none.
     Widths are attributes because GitHub strips stylesheets from a README;
     without them two 1024-pixel screenshots would overflow the column. -->
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

From a fresh clone, once. SDL2 and its image codecs are system libraries; opam
cannot build them for you:

```sh
sudo apt install libsdl2-dev libsdl2-image-dev   # Debian / Ubuntu
brew install sdl2 sdl2_image                     # macOS — and see the note below

opam switch create . 5.5.0 --no-install          # this repo uses a local switch
eval $(opam env --switch=. --set-switch)
opam install . --deps-only --with-test --with-doc
```

**macOS:** add `--no-depexts` to the `opam install` line. opam cannot detect
Homebrew's SDL2, so install the libraries yourself and skip the check;
[HACKING.md](HACKING.md) explains the cause. **Windows:** use the MSYS2
environment. Step 0 of
[the guide](https://pharick.github.io/camlcast/making-a-game.html) covers
setup on both.

Then:

```sh
dune exec camlcast-demo                    # the list, on screen
dune exec camlcast-demo -- --list          # the same list, printed
dune exec camlcast-demo portals            # straight to one of them
dune test                                  # all suites
```

With no arguments, `camlcast-demo` opens a menu of the demos drawn over one
slowly turning room: the arrow keys move the highlight, Enter or Space runs
one, Escape returns to the list. With a demo's name it launches that demo
directly, and Escape then ends the program, since there is no menu behind it.
In a later shell, `eval $(opam env --switch=. --set-switch)` from the
repository restores the switch.

Demos that read art from files look relative to the executable, which under
dune is `_build/default/assets/`; `dune build` puts it there. Installed, it is
`share/camlcast-demo/assets/` under the same prefix as the binary. The share
directory is named after the executable, so a game called `wanderer` reads
`share/wanderer`. Set `CAMLCAST_ASSETS` to a directory to look there instead,
and only there.

## The demos

One small world per engine feature, and one with all of them at once. Each is
a single file under `demo/` containing only the feature it demonstrates, and
each spawns the player facing that feature.

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
| `phases`   | a component with a phase, a clock, and a light going out       |
| `overlay`  | drawing over the finished world                                |
| `controls` | binding keys, press versus hold, and letting go of the mouse   |
| `text`     | a bitmap font: wrapping, measuring, clipping and colour        |
| `loading`  | art read from files, beside the generated kind                 |
| `showcase` | the five-room level, with all of the above at once             |

`demo/catalogue.ml` is the list itself, and `showcase` is `demo/level.ml`.
`test_demos` checks every world in the list: the spawn point is standable,
rooms enclose themselves, every room is reachable, and no floor steps across a
doorway. Adding a demo to `Catalogue.demos` also adds it to that suite.

## The libraries

The engine ships no content — no colours, patterns, pictures or rooms — only
the types those things are values of. A game supplies its own, so two games
can share the engine without sharing a look.

| directory | library         | what it is                                                      |
| --------- | --------------- | --------------------------------------------------------------- |
| `lib/`    | `camlcast`      | **what a game opens**: the parts a world is described with       |
| `loom/`   | `camlcast.loom` | the declarative runtime: elements, reconciling, hooks, store     |
| `core/`   | `camlcast.core` | the platform: geometry, ray casting, rendering, SDL              |
| `demo/`   | `camlcast-demo` | the demos and the art they are made of, run by `camlcast-demo`  |

A game opens `Camlcast` and nothing else. The platform underneath — `Engine`,
`Renderer`, `Framebuffer`, `World`, `Player` — is not in that module; reaching
it means adding `camlcast.core` to a dune file.

`camlcast.loom` depends only on the standard library and knows nothing of
walls; `camlcast` is the only library that knows both loom and the platform.
The guides and the demos teach the layer. Reach for `camlcast.core` when a
game genuinely needs a `World` or a `Renderer`.

Nothing in the engine depends on `demo/`: the demos are content, and they live
outside the library they are content for (see `demo/catalogue.ml`). They stay
in this repository because together they exercise every corner of the engine —
decals, see-through walls, sloped floors, the open sky, growth, overlays and
input — so a change that breaks one breaks a world you can walk through here.

They are two opam packages for the same reason. `camlcast` is the engine: a
library that reads no file and puts nothing in a prefix's `share`.
`camlcast-demo` is `bin/demo.ml` and the pictures it needs, which a program
that reads its art off the disk must carry wherever it is installed.

## Using the engine

Pin it, since it is not on opam yet, and add `(libraries camlcast)` to your
`dune`:

```sh
opam pin add camlcast git+https://github.com/pharick/camlcast.git
```

(The demos are the second package and depend on the first; `opam install .`
from a checkout pins both in one step.)

A game **describes** its world: it says what the world should be right now,
every frame, from nothing, and the runtime computes what changed. A level is
OCaml code rather than a data file, so the smallest complete game is one room
and a call. This is
[`examples/step01_room.ml`](examples/step01_room.ml) — step 1 of the guide,
compiled with the rest of the tree so that it cannot drift from the engine:

```ocaml
open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let height = 4.
let flat = Plane.horizontal 0.

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      ~spawn:("vault", Vec.make (-4.5) 0.)
      [
        room ~name:"vault"
          ~floor:(floor ~plane:flat ~material:ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
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
  match Run.play ~title:"The Undercroft" level with
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

Components are functions, so they compose as ordinary OCaml: a higher-order
component takes children and puts something around them.
**[Making a game on CamlCast](https://pharick.github.io/camlcast/making-a-game.html)**
walks through the engine one feature at a time, naming the demo that isolates
each. Every step is a complete program in [`examples/`](examples/) —
`step01_room.ml` through `step26_shipping.ml`, each the whole game as that
step leaves it, all compiled with the tree so none of them can drift.

## Controls

The engine names no key of its own. Walking, looking, fullscreen and leaving
the run all come from a `Binding.t` the game hands to `Engine.run`. The demos
use `Binding.default`:

| key / device | action                                |
| ------------ | ------------------------------------- |
| `W` / `S`    | walk forward / back                   |
| `A` / `D`    | strafe left / right                   |
| mouse        | look around (yaw and pitch)           |
| `←` / `→`    | turn left / right (keyboard fallback) |
| `↑` / `↓`    | look up / down (keyboard fallback)    |
| `F11`        | toggle fullscreen                     |
| `Esc`        | leave the run (see below)             |

`Binding.default` binds _no_ key that ends a run, because a game with screens
in it wants `Esc` for closing them. `Engine.run_world` adds `Esc` itself,
since a bare world has nothing else to end it with.

Rebinding is a value.
[`examples/step23_controls.ml`](examples/step23_controls.ml) walks on `I` and
`K` beside `W` and `S`, puts the left mouse button to work beside `E`, and
binds Escape again — a supplied part replaces the default's part rather than
adding to it:

```ocaml
let controls =
  let hold key weight =
    { Binding.source = Binding.Hold (Input.Key key); weight }
  in
  Controls.make
    ~bindings:
      (Binding.make
         ~forward:
           {
             Binding.speed = 3.6;
             terms =
               [
                 hold Key.w 1.; hold Key.s (-1.); hold Key.i 1.;
                 hold Key.k (-1.);
               ];
           }
         ~leave:[ Input.Key Key.escape ] ())
    ~use:[ Input.Key Key.e; Input.Button Input.Left ]
    ~map:[ Input.Key Key.f3; Input.Key Key.m ]
    ()
```

An axis sums its terms, and the two kinds of term are summed differently: a
held key is a **rate**, paid out at the axis's speed over the frame, while the
mouse is a **displacement**, added as it stands. Step 23 of
[the guide](https://pharick.github.io/camlcast/making-a-game.html) covers the
distinction and the seam a gamepad would arrive through.

Some demos bind keys beyond the table: `phases` starts on Space; `chalk` marks
on `C` and picks the mark with `1` and `2`; `doors`, `targets` and the
showcase use `E` on whatever is at hand; `controls` binds a second full set of
walking keys and prints them with `Key.name`.

The mouse is captured in relative mode: the cursor is hidden and never reaches
a screen edge.

The window is resizable and `F11` toggles borderless fullscreen. Resizing is
**Hor+**: the vertical field of view is fixed, so dragging the window wider
reveals more of the world to the sides instead of magnifying what was on
screen, and pixels stay square at every shape.

## Bundles

Pushing a `v*` tag builds a bundle per platform and attaches it to a GitHub
release: a `.app` for macOS on Apple silicon and on Intel, a tarball for Linux
x64, a folder for Windows x64. Each carries its own SDL2 and image codecs, so
nothing has to be installed to run one. `tools/bundle-*.sh` build them, and
can be run on a laptop against a `dune build` tree.

A bundle opens on the list of demos: a window that was double-clicked has no
command line to name one. That is also why the list is drawn as well as
printed.

The Linux tarball needs **glibc 2.39 or newer** (Ubuntu 24.04, Debian 13,
Fedora 40). The `.app` is signed ad-hoc but not notarized, so a Mac that
downloaded it refuses the first launch. Open System Settings → Privacy &
Security and choose "Open Anyway" (the Control-click bypass was removed in
macOS 15), or run once:

```sh
xattr -dr com.apple.quarantine camlcast-demo.app
```

[HACKING.md](HACKING.md) explains how the glibc floor and the `.app`'s
minimum macOS version are determined.

## The engine in one page

**`core/`** is twenty-nine modules, each depending only on the ones before it:
`Config`, `Key` and `Vec` at the bottom, `Room`, `World` and `Ray` in the
middle, `Renderer`, `Clock` and `Engine` on top. The maths lives in the module
docs: `Ray` for why the distance it reports is free of fish-eye, `Plane` for
the equation that casts a sloped floor per pixel, `Viewport` for the
projection and the resize rules, `Transform` for why linked doorways pair in
reverse, `World` for the portal machinery.

**`loom/`** is the declarative runtime, depending only on the standard
library: `Element` is what a component returns, `Reconcile` matches this
frame's description against last frame's tree, `Hook` keeps state in a row of
slots reached through OCaml 5 effect handlers, `Context` hands values down a
subtree with a `Type.Id` witness instead of a cast, and `Host` is the whole of
the seam — two types and one function.

**`lib/`** is what a game opens: `P` for the parts a world is made of, `Check`
for what is wrong with a level before anyone walks into it, `Aim` for what the
crosshair is on, `Run` for the loop.

The annotated list is the
**[documentation landing page](https://pharick.github.io/camlcast/)**.

## Documentation

The modules are documented with odoc comments (`(** ... *)`); the maths
derivations live there rather than in this file. Every push to `main`
publishes them, along with all three guides:
**[pharick.github.io/camlcast](https://pharick.github.io/camlcast/)**.

- **[Making a game on CamlCast](https://pharick.github.io/camlcast/making-a-game.html)**
  — from an empty directory to a game, one feature at a time.
- **[Building the engine from scratch](https://pharick.github.io/camlcast/building-the-engine.html)**
  — how the picture is drawn, with every derivation written out.
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

`tools/pages-site.py` is required, not optional: dune's `documentation` stanza
cannot carry assets, so the screenshots in the guides reach the site through
the script and not through `@doc`. `dune build @doc` also prints a small
**expected** set of warnings, one per `@raise` tag naming a standard-library
exception; [HACKING.md](HACKING.md) explains why they cannot be silenced.
Anything else in that output is a reference that has gone stale.

The pictures the guides and this file use live in `doc/images/`.

## Tests

[Alcotest](https://github.com/mirage/alcotest), a suite per engine module,
except `Config` (constants) and `Door` and `Framebuffer` (exercised through
the modules built on them), plus suites for the demo package's own machinery,
all in `test/`. They share `Support`, a small library in the same directory,
which holds a hand-checkable 4x4 square room, a pair of rooms joined through a
doorway, and the custom testables — a failing `Vec` check prints `(3, 2.5)`
rather than a bare `false`.

The suites are two stanzas, one per package, so that each package's tests
build from that package alone: `dune runtest` runs all of them, and the `-p`
build opam does runs only the ones belonging to the package it is building.

```sh
dune exec test/test_player.exe -- --verbose   # one suite
dune exec test/test_ray.exe -- test hits      # one group
```

**Nothing here opens a window.** `Framebuffer.offscreen` builds a buffer with
no streaming texture behind it, and `Renderer.draw_frame` fills a buffer with
no SDL call in it. `test_paint` and `test_font` draw and read the pixels back,
and `test_renderer` renders whole frames: where a billboard lands, what a low
wall hides of it, and what a doorway trims it to are checked on the pixels
rather than only in the arithmetic that feeds them.

`test_level.ml` is the closest thing to an integration test: it checks the
showcase world the way a player meets it, so an engine change that breaks
portals, sloped floors or the sky fails there rather than in a unit suite.

## Hacking

[HACKING.md](HACKING.md) is the contributor's page: the development setup in
full, the formatting pin, what CI checks, the expected `@doc` warnings, how
the release bundles are put together, and the four places a new demo has to
appear.

## License

MIT — see [LICENSE](LICENSE).
