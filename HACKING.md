# Hacking on CamlCast

Notes for working on this repository: the setup in full, what CI checks, and
the corners of the build that are deliberate. [README.md](README.md) covers
using the engine; this file covers changing it.

## Setup

SDL2 and its image codecs are system libraries, installed by your package
manager rather than by opam: `libsdl2-dev libsdl2-image-dev` on Debian or
Ubuntu, `sdl2 sdl2_image` from Homebrew, the mingw64 packages under MSYS2.
Then, from the checkout:

```sh
opam switch create . 5.5.0 --no-install
eval $(opam env --switch=. --set-switch)
opam install . --deps-only --with-test --with-doc --with-dev-setup
dune build && dune runtest
```

`--with-dev-setup` installs the pinned `ocamlformat` and `ocaml-lsp-server`;
without it the build still works, but `dune fmt` does not.

On macOS add `--no-depexts` to the install line. Homebrew's `sdl2` is an alias
for the `sdl2-compat` formula, and opam checks for the depext by name against
`brew list`, which reports only the formula. The `sdl2` the depext asks for
never appears there, so the check fails no matter what is installed. Install
the libraries yourself and tell opam to stop looking; the fix that would make
that unnecessary is for `conf-sdl2` to ask for `sdl2-compat` on macOS, filed as
[ocaml/opam-repository#30337](https://github.com/ocaml/opam-repository/issues/30337).

The engine's floor is OCaml 5.2, required for `-H`, the hidden include that
makes `(implicit_transitive_deps false)` hide directories instead of dropping
them. The code itself asks for less: the newest feature used is
`Array.find_index` (5.1), which `World` uses to resolve a room's name to its
index. CI builds and tests at 5.2 as well as at the development version, so
the bound in `dune-project` is checked rather than merely asserted.

## Formatting

`dune build @fmt` is a CI check; `dune fmt` applies it. The version is pinned
in two places — `.ocamlformat` says `0.29.0` and the opam dev-setup dependency
pins the same — because ocamlformat's output changes between releases. A
mismatched binary refuses to run rather than quietly reflowing the tree.

## Comments

A comment justifies or explains the code as it stands. It does not narrate how
the code got there.

- **Yes**: "`:with-test` rather than `:with-dev-setup`, because CI's floor job
  installs `--with-test` alone and then builds the workspace, `bench/`
  included."
- **No**: "This used to be a constant in `Config`." "The old version kept a room
  by index." "There was no check for this at all." "Every step of this rewrite
  has said the same thing."

The reason is not taste. A claim about the past is unfalsifiable from the tree:
nothing compiles against it, no test covers it, and a reader cannot check it
without the history the comment is standing in for. It rots in silence, and one
wrong line teaches people to stop trusting the ones beside it. `git log` and
`git blame` hold the history, and hold it accurately.

Almost every historical comment is a current-state comment that gave up too
early. "These used to be five constants" is really "these are a value because
they are most of what distinguishes one place from another" — the argument
survives the next edit, the date stamp does not. Where a past mistake is the
whole point of a guard, describe the mistake the guard prevents rather than the
day someone made it.

Three things this does not forbid:

- The behaviour of something outside this repo, stated in the present: "dune
  does not turn these warnings on".
- Runtime pasts. "the wall it used to be" is about a value one call ago, not
  about the repository.
- Step-to-step continuity in `examples/` and the guides, where the reader has
  just read the previous version and the comparison is the lesson.

## The examples

`examples/` holds one complete program per step of the making-a-game guide,
`step01_room.ml` through `step26_shipping.ml` — each the whole game as that
step leaves it, the guide quoting only what each step adds. They compile with
the default build and belong to no package, so `dune build` fails the moment
the engine changes under them: a snippet on a page cannot go stale while the
program it is quoted from still builds. Changing an example means changing the
page that quotes it, and the other way round. A step's number lives in exactly
three synced places: filename, header comment, guide heading.

`step01_room.ml` is what README.md quotes, and `step23_controls.ml` is its
rebinding snippet. Two steps read from `assets/` (`step13_words.ml` onward for
the font, `step26_shipping.ml` for pictures), which is one more reason the
default build copies `assets/` into `_build`. `step22_check.ml` proves the
level without a window — `dune exec examples/step22_check.exe -- --check` —
and is the guide's worked example of `Check` and `Mount`.

## The docs and the site

Every push to `main` publishes the odoc pages and all three guides to
[pharick.github.io/camlcast](https://pharick.github.io/camlcast/). Locally:

```sh
dune build @doc
python3 tools/pages-site.py
open _site/index.html
```

`tools/pages-site.py` is required, not optional: dune's `documentation` stanza
cannot carry assets, so the screenshots in the guides reach the site through
the script and not through `@doc`. Opening
`_build/default/_doc/_html/index.html` directly still works, with every
picture in it broken.

`doc/index.mld` is the landing page and the two guides are `.mld` pages beside
it. `doc/demo/index.mld` is the demos' own page, in its own directory because
it belongs to the other package: a `.mld` page can only name what its
package's libraries bring in scope, and the engine does not depend on the
demos. Both libraries have a public name, which is what makes `@doc` pick
their modules up (odoc skips private libraries). odoc writes one directory per
package, and `tools/pages-site.py` roots the site at `camlcast` and carries
`camlcast-demo` across beneath it.

### The expected `@doc` warnings

`@doc` prints a warning for every `@raise` tag naming a standard-library
exception: `Invalid_argument` wherever one is refused, and
`Fun.Finally_raised` in `Result_ext`. odoc reads a `@raise` argument as a
reference, and the classic `@doc` alias puts only this project's packages and
their direct dependencies on its resolution path, so nothing in `Stdlib` can
resolve there. Writing `Stdlib.Invalid_argument` does not help, and
`[Invalid_argument]` silences the warning only by demoting the tag to a code
span, losing the raise contract with it. The site builds and every link inside
it resolves. **That set of warnings is the expected output** — anything else
in it is a real reference that has gone stale. `@doc-new`, the odoc 3 driver
alias that would put `Stdlib` in scope, does not build in this tree.

## The demos

All twenty-two demos are descriptions, and between them they reach every
primitive the layer has. What each one is a demo of:

| demo | what it needs | where |
| --- | --- | --- |
| `masonry`, `loading` | materials, art from disk | `P.boundary`, `Texture`, `Asset` |
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

Four of the demo suites assert something worth knowing before reading them:

- `dust` — a moving room does not share the walls of the room it moved from.
  `bench/frame.exe` is why that is affordable.
- `endless` — the world grows by describing more rooms, so there is no graph
  surgery to get right.
- `trail`, `menu` — they read what the player sees, the ticks on the HUD and
  the row the list highlights, rather than private state.
- `menu` also pins the one place a component is visibly not a pure `update`: a
  handler runs *after* the frame it fired on, so the frame a key goes down on
  still shows what was selected before it.

The layer's reference is not a demo but the guide's one room, hand-built
against the platform and restated inline in `test_stage.ml`, which renders it
beside its described twin and compares every pixel. It is a valid reference
because it names nothing in the layer: it is what the layer has to reproduce.

## Benchmarks

```sh
dune exec bench/frame.exe                    # what a frame costs
dune exec --profile release bench/frame.exe  # and what it costs shipped
```

`bench/` is an executable and not a test: `dune build` compiles it so it
cannot rot, and `dune runtest` never runs it, so CI spends no time on a
benchmark nobody is reading.

`bench/frame.ml` settled whether the declarative layer needs to cache what it
assembles. It does not: describing the largest world this engine has costs a
seventh of one percent of drawing it. The file records the numbers, so the
question can be re-run rather than re-argued.

## Bundles

`tools/bundle-macos.sh`, `bundle-linux.sh` and `bundle-windows.sh` each turn a
`dune build` tree into something a machine with neither OCaml nor SDL can run:
the macOS one walks `otool -L` into a `.app` and ad-hoc signs it, the Linux
one walks `ldd` into a tarball with a launcher that sets `LD_LIBRARY_PATH`,
the Windows one copies the mingw64 DLLs beside the executable. A `v*` tag runs
all three on CI and attaches the results to a release.

How old a Mac the `.app` runs on is decided by the machine that built it, not
chosen: the bundled libraries are Homebrew bottles, built for the runner's own
macOS. `bundle-macos.sh` reads the answer back out of the finished bundle and
records it as `LSMinimumSystemVersion`, so the exact version is in the
`.app`'s `Info.plist` and in the build log rather than promised anywhere.

The Linux tarball has the same question and answers it the other way, by
choosing. It carries SDL2 and the image codecs but deliberately not glibc or
the dynamic loader — a binary must use the loader it was built against — so
the runner's glibc is the floor for everyone who downloads it. The release job
is therefore pinned to a runner image rather than tracking the newest one, and
the tarball needs **glibc 2.39 or newer** (Ubuntu 24.04, Debian 13, Fedora
40). That floor moves only when the pin in `.github/workflows/release.yml`
does.

## Adding a demo

A demo is added in four places, and the suite checks all four:

1. its file in `demo/` — one feature, short enough to read in a sitting. It
   exposes `level` (or a component), `world` for the catalogue and the suites,
   and `run window = Run.on window ...`;
2. an entry in `Catalogue.demos`, which also enrols it in `test_demos` (spawn
   is standable, rooms enclose themselves, every room reachable, no floor step
   across a doorway);
3. a row in README.md's table — the blurb there is the catalogue's `blurb`
   string, verbatim, so write it once and paste it;
4. a line on `doc/demo/index.mld`, carrying that same blurb and at most one
   further sentence.

`test_demos` compares the last two against the catalogue, ignoring line breaks
and the markup each page puts round an identifier. A blurb reworded in one
place and not the others fails the suite.

## CI

Three workflows:

- **CI** (`ci.yml`) — every push to `main` and every pull request: build and
  test on the development compiler, `dune build @fmt`, `dune build @doc`, and
  a separate job that builds and tests at OCaml 5.2, the floor. On pushes to
  `main` the built site deploys to GitHub Pages.
- **Platforms** (`platforms.yml`) — on demand: build and test on macOS (both
  architectures) and Windows.
- **Release** (`release.yml`) — on a `v*` tag: the three bundle scripts, and a
  GitHub release with their output attached.
