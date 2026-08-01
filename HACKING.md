# Hacking on CamlCast

Notes for working on this repository — the setup in full, what CI holds you to,
and the corners of the build that are deliberate rather than accidental.
[README.md](README.md) covers using the thing; this file covers changing it.

## Setup

SDL2 and its image codecs are system libraries, installed by your package
manager rather than by opam — `libsdl2-dev libsdl2-image-dev` on Debian or
Ubuntu, `sdl2 sdl2_image` from Homebrew, the mingw64 packages under MSYS2.
Then, from the checkout:

```sh
opam switch create . 5.5.0 --no-install
eval $(opam env --switch=. --set-switch)
opam install . --deps-only --with-test --with-doc --with-dev-setup
dune build && dune runtest
```

`--with-dev-setup` is what brings in the pinned `ocamlformat` and
`ocaml-lsp-server`; leave it off and the build still works, but `dune fmt`
will not. On macOS add `--no-depexts` to the install line: Homebrew ships
`sdl2-compat` under the name `sdl2`, which opam's dependency check cannot see,
so you install the libraries yourself and tell opam to stop looking.

The engine's floor is OCaml 5.1 — for `Array.find_index`, which `World` uses
to resolve a room's name to its index — and CI builds and tests at 5.1 as well
as at the version development happens on, so the bound in `dune-project` is
checked rather than merely asserted.

## Formatting

`dune build @fmt` is a CI check; `dune fmt` applies it. The version is pinned —
`.ocamlformat` says `0.29.0` and the opam dev-setup dependency pins the same —
because ocamlformat's output changes between releases, and an unpinned
formatter turns "the repository is formatted" into "the repository is formatted
the way whoever last ran it had installed". A mismatched binary refuses to run
rather than quietly reflowing the tree.

## The examples

`examples/` holds the complete programs the guides quote. They compile with the
default build and belong to no package, so `dune build` fails the moment the
engine moves under them — which is the point: a snippet on a page cannot rot
while the program it is quoted from still builds. Changing one means changing
the page that quotes it, and the other way round.

They come in pairs on purpose:

| against the layer | against the platform | what it shows |
| --- | --- | --- |
| `described_room.ml` | `room.ml` | one room. `test_stage` renders both and compares every pixel |
| `described_fuse.ml` | `game.ml` | a small game with a phase and a clock |
| | `doorways.ml`, `rebind.ml` | two rooms; rebinding, on the older API |

`described_room.ml` is what README.md quotes and step 1 of the guide.
`described_fuse.ml` beside `game.ml` is the shortest account of what the layer
is for: there, one record holds a phase, a clock and a player advanced by one
`update`; here the phase and the clock belong to the component that uses them
and the player belongs to the runtime.

## The docs and the site

Every push to `main` publishes the odoc pages and all three guides to
[pharick.github.io/camlcast](https://pharick.github.io/camlcast/). Locally:

```sh
dune build @doc
python3 tools/pages-site.py
open _site/index.html
```

`tools/pages-site.py` is needed rather than optional: dune's `documentation`
stanza has no way to carry assets, so the screenshots the guides are
illustrated with reach the site through it and not through `@doc`. Opening
`_build/default/_doc/_html/index.html` directly still works, with every picture
in it broken.

`doc/index.mld` is the landing page and the two guides are `.mld` pages beside
it; `doc/demo/index.mld` is the demos' own, in its own directory because it
belongs to the other package — a `.mld` page can only name what its package's
libraries bring in scope, and the engine does not depend on the demos. Both
libraries have a public name, which is what makes `@doc` pick their modules up
(odoc skips private libraries), so odoc writes one directory per package and
`tools/pages-site.py` roots the site at `camlcast` and carries `camlcast-demo`
across beneath it.

### The expected `@doc` warnings

`@doc` prints a warning for every `@raise` tag naming a standard-library
exception — `Invalid_argument` wherever one is refused, and
`Fun.Finally_raised` in `Result_ext`. odoc reads a `@raise` argument as a
reference, and the classic `@doc` alias puts only this project's packages and
their direct dependencies on its resolution path, so nothing in `Stdlib` can
resolve there; writing `Stdlib.Invalid_argument` does not help, and
`[Invalid_argument]` silences it only by demoting the tag to a code span and
losing the raise contract with it. The site builds and every link inside it
resolves. **That set of warnings is the expected output** — anything else in it
is a real reference that has gone stale. `@doc-new`, the odoc 3 driver alias
that would put `Stdlib` in scope, does not build in this tree.

## The demos, and what migrating them found

All twenty-two demos are descriptions. Migrating them was the parity check, and
it worked as one: five primitives were found by rewriting a demo that had always
needed them, rather than by design.

| found by | what it closed |
| --- | --- |
| `slopes`, `floating`, `level` | `P.opening` / `P.through` — carrying a floor across a doorway |
| `barred`, `level` | `P.threshold` — a lintel of a different material from its wall |
| `controls` | `P.cursor` — freeing the mouse instead of capturing it |
| `targets` | `P.highlight` and `Aim.ring` — the projection needed the viewport |
| `targets`, `level` | `Events.aim` — what kind of thing the crosshair is on |

Two more came from the audit before it: `on_use` takes an `Aim.spot`, because
`chalk` marks a wall where the crosshair is; and `Events.use_crossed`, because
`trail` builds a route home from the doorways a frame went through and
`Engine.step` throws those away.

Four tests stopped meaning what they meant, and each says so where it is rather
than being quietly made to compile:

- `dust` asserted that a moving room *shares* the walls of the room it moved
  from. False now by design; `bench/frame.exe` is why that is affordable.
- `endless` asserted that graph surgery was done right. There is no surgery.
- `trail` and `menu` read private state. They read what the player sees now — the
  ticks on the HUD, the row the list highlights.

`test_menu` also found the one place a component differs visibly from the pure
`update` it replaced: a handler runs *after* the frame it fired on, so the frame
a key goes down on still shows what was selected before it.

| demo | what it needs | where |
| --- | --- | --- |
| `masonry`, `loading` | materials, art from disk | `P.outline`, `Texture`, `Asset` |
| `gallery` | decals and sprites | `P.decal`, `P.sprite` |
| `glass`, `barred` | see-through materials, a door you see through | `P.wall`, `P.doorway ~door` |
| `slopes` | inclined floors and roofs | `P.floor`, `P.roof` |
| `daylight` | the open sky, per room | `P.open_sky` |
| `haze` | atmosphere | `P.world ~atmosphere` |
| `portals`, `doors` | doorways, links, doors that open | `P.doorway`, `P.link`, `on_use` |
| `changing` | a room rebuilt every frame | describing it differently |
| `floating`, `dust` | sprites off the floor, sprites that move | `P.sprite ~base`, `use_frame` |
| `chalk` | marking a wall where you point | `on_use` and its `Aim.spot` |
| `endless` | a world that grows | rooms appear; `Run.carry` holds the player |
| `targets` | what the crosshair is on, through a doorway | `on_gaze` |
| `trail` | the doorways a frame went through | `Events.use_crossed` |
| `phases` | a phase, a clock, an ending | `use_state`, `use_frame`, `P.finish` |
| `overlay`, `text` | drawing over the world, a bitmap font | `P.hud`, `P.text` |
| `controls` | binding controls | `Run.play ~controls` |
| `showcase` | all of the above at once | all of the above |

What the layer is still compared against is not a demo but `examples/room.ml`,
the hand-built world the README quotes: `test_stage` renders that and its
described twin and compares every pixel. A reference has to be something that
was not rewritten, and that one was not.

## Benchmarks

```sh
dune exec bench/frame.exe                    # what a frame costs
dune exec --profile release bench/frame.exe  # and what it costs shipped
```

`bench/` is an executable and not a test: `dune build` compiles it so it cannot
rot, and `dune runtest` never runs it. A benchmark answers a question somebody
asked, and spending a minute of every CI run on one nobody reads is how a suite
comes to be ignored.

`bench/frame.ml` is the one that settled whether the declarative layer needs to
cache what it assembles. It does not — describing the largest world this engine
has costs a seventh of one percent of drawing it — and the file says so with the
numbers, so the next person to wonder can re-run it rather than re-argue it.

## Bundles

`tools/bundle-macos.sh`, `bundle-linux.sh` and `bundle-windows.sh` each turn a
`dune build` tree into something a machine with neither OCaml nor SDL can run:
the macOS one walks `otool -L` into a `.app` and ad-hoc signs it, the Linux one
walks `ldd` into a tarball with a launcher that sets `LD_LIBRARY_PATH`, the
Windows one copies the mingw64 DLLs beside the executable. A `v*` tag runs all
three on CI and attaches the results to a release.

How old a Mac the `.app` runs on is decided by the machine that built it, not
chosen: the bundled libraries are Homebrew bottles, built for the runner's own
macOS. `bundle-macos.sh` reads the answer back out of the finished bundle and
records it as `LSMinimumSystemVersion`, so the exact version is in the `.app`'s
`Info.plist` and in the build log, rather than being promised anywhere.

The Linux tarball has the same question and answers it the other way, by
choosing. It carries SDL2 and the image codecs but deliberately not glibc or the
dynamic loader — a binary has to use the loader it was built against — so the
runner's glibc is the floor for everyone who downloads it. The release job is
therefore pinned to a runner image rather than tracking the newest one, and the
tarball needs **glibc 2.39 or newer** (Ubuntu 24.04, Debian 13, Fedora 40). That
floor moves only when the pin in `.github/workflows/release.yml` does.

## Adding a demo

A demo is added in four places, and the suite holds you to the first two:

1. its file in `demo/` — one feature, short enough to read in a sitting. It
   exposes `level` (or a component), `world` for the catalogue and the suites,
   and `run window = Run.on window ...`;
2. an entry in `Catalogue.demos`, which also enrols it in `test_demos` (spawn
   is standable, rooms enclose themselves, every room reachable, no floor step
   across a doorway);
3. a row in README.md's table — the blurb there is the catalogue's `blurb`
   string, verbatim, so write it once and paste it;
4. a line on `doc/demo/index.mld`.

## CI

Three workflows:

- **CI** (`ci.yml`) — every push to `main` and every pull request: build and
  test on the development compiler, `dune build @fmt`, `dune build @doc`, and a
  separate job that builds and tests at OCaml 5.1, the floor. On pushes to
  `main` the built site deploys to GitHub Pages.
- **Platforms** (`platforms.yml`) — on demand: build and test on macOS (both
  architectures) and Windows.
- **Release** (`release.yml`) — on a `v*` tag: the three bundle scripts, and a
  GitHub release with their output attached.
