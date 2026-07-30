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
opam install . --deps-only --with-test --with-doc --with-dev-setup
eval $(opam env)
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

`examples/` holds the complete programs the guides quote — `room.ml` is step 1
of the making-a-game guide and the example in README.md, `doorways.ml` step 5,
`game.ml` step 12, `rebind.ml` step 13. They compile with the default build and
belong to no package, so `dune build` fails the moment the engine moves under
them — which is the point: a snippet on a page cannot rot while the program it
is quoted from still builds. Changing one of them means changing the page that
quotes it, and the other way round.

## The docs and the site

Every push to `main` publishes the odoc pages and both guides to
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

1. its file in `demo/` — one feature, short enough to read in a sitting;
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
