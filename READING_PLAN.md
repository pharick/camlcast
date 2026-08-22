# Reading CamlCast: a guided route through the source

A step-by-step plan for reading this repository until the architecture is in
your head: what to read, in what order, what to look for in each file, and
where to learn the concepts a file assumes. [README.md](README.md) explains how
to *use* the project and [HACKING.md](HACKING.md) how to *change* it; this file
is for *understanding* it.

The plan is seven phases. Each phase ends with something you can check —
a question you should now be able to answer — so you know whether to move on
or reread. Total effort is roughly 15–25 focused hours for the whole thing;
the [fast path](#the-fast-path) at the end is ~3 hours if you only need the
shape of it.

---

## The map before the territory

CamlCast is **three libraries and one pile of content**, and the whole
architecture is the discipline about which of them may know about which:

```
            ┌────────────────────────────────────────────┐
            │  demo/ + bin/   (camlcast-demo — content)  │
            │  one small world per engine feature        │
            └───────────────────┬────────────────────────┘
                                │ opens
            ┌───────────────────▼────────────────────────┐
            │  lib/   (camlcast — what a game opens)     │
            │  P, components, Events, Run, Check, Aim    │
            └─────────┬────────────────────────┬─────────┘
                      │ depends on             │ depends on
    ┌─────────────────▼──────────┐   ┌─────────▼─────────────────────┐
    │  loom/  (camlcast.loom)    │   │  core/  (camlcast.core)       │
    │  the declarative runtime:  │   │  the platform: geometry, ray  │
    │  elements, reconciling,    │   │  casting, software renderer,  │
    │  hooks, context, store     │   │  input, SDL window & loop     │
    │  — knows nothing of walls  │   │  — knows nothing of components│
    └────────────────────────────┘   └───────────────────────────────┘
```

In React's vocabulary (the analogy the source itself uses): `core/` is the
DOM, `loom/` is React plus the reconciler, and `lib/` is react-dom — the one
place that knows both. `loom/` depends on nothing but the OCaml standard
library; a game opens `Camlcast` (from `lib/`) and nothing else.

Two ideas carry the whole engine, and you will meet them over and over:

1. **The world is a graph of rooms joined at portals.** Not a grid. Each room
   is authored in its own coordinate frame from wall *segments* at any angle,
   with an inclined floor and an inclined ceiling or open sky; doorways join
   rooms, and a `Transform` carries the camera and the ray from one room's
   coordinates into the next. Rendering and walking both cross these seams.

2. **A game describes; the runtime reconciles.** A game rebuilds a complete
   *description* of its world every frame, from scratch, as plain data. The
   loom matches this frame's description against last frame's instance tree,
   keeps what is the same (so component state survives), and hands the
   settled result to a host that assembles an actual `World`.

Hold those two, and every file below is a detail of one or the other.

---

## How to read (method, before order)

- **Read the `.mli`, not the `.ml`, first — always.** Every module in `core/`
  and `loom/` and `lib/` has an interface file, and the project's convention
  is that the *documentation and the math derivations live in the `.mli`*
  (odoc comments). Many questions end there. Open the `.ml` only when you
  want to see how a documented contract is met.
- **Read the `dune` files as prose.** This repository's build files are
  essays: `loom/dune` argues why its `(libraries)` line is empty,
  `dune-project` explains `implicit_transitive_deps`, `lib/dune` states the
  React analogy. They are the architecture documentation, enforced.
- **Keep the matching test open in a split.** Nearly every module has a
  suite: `core/vec.mli` pairs with `test/test_vec.ml`, and so on. A test
  shows the module used honestly, and shows what the author considered the
  edge cases. Nothing in `test/` opens a window, so every suite is also
  runnable while you read: `dune exec test/test_ray.exe -- --verbose`.
- **Keep the guides beside the clusters.** The three guides under
  `doc/` (published at [pharick.github.io/camlcast](https://pharick.github.io/camlcast/))
  were written against this exact code. Where a phase below says "guide:
  Phase 4", that is `doc/building-the-engine.mld`'s Phase 4, and it derives
  on paper what the module then does in code. Prefer them to external
  tutorials — external links in this plan are for *background* concepts only.
- **Run what you read.** `dune exec camlcast-demo <name>` puts almost every
  subsystem on screen in isolation; the [demo table in README.md](README.md#the-demos)
  maps features to demos.

---

## Phase 0 — Orientation (no source code yet)

*~1 hour. Goal: know what the project is, how it is laid out, and why.*

Read, in order:

| # | file | what to take from it |
|---|------|----------------------|
| 0.1 | `README.md` | The whole pitch. Especially the sections **The libraries**, **The engine in one page**, and the demo table. |
| 0.2 | `dune-project` | The two opam packages and why they are two; `implicit_transitive_deps false` and what boundary it enforces. |
| 0.3 | `core/dune`, `loom/dune`, `lib/dune`, `demo/dune` | Four short comment-essays. `loom/dune`'s empty `(libraries)` is the single most load-bearing line in the repo — the reconciler may not know what a pixel is. |
| 0.4 | `doc/index.mld` | What each of the four libraries is and where its module index lives, **Where to start** — the reading path into `core/` that this plan expands — and **The house rules**, the API conventions (`make`, `load`, `with_x`/`add_x`/`set_x`, units) that make one module's shape predict all the others. |
| 0.5 | `HACKING.md` | Skim. Note "The demos, and what migrating them found" — it is a compressed history of *why* the layer's API has the members it has. |

If you can build, also do: `dune exec camlcast-demo` and walk through
`showcase`, then two or three small demos (`portals`, `slopes`, `glass`).
Seeing portals and sloped floors *before* reading their code gives the code
something to attach to.

**Checkpoint.** You can answer: *Why are there two opam packages? What may
`loom/` depend on? What does a game `open`?*

---

## Phase 1 — What game code looks like (top-down glance)

*~1 hour. Goal: see the API you will spend Phases 2–4 explaining.*

The engine is best read bottom-up, but only after one top-down look, so the
bottom has a visible purpose.

| # | file | what to take from it |
|---|------|----------------------|
| 1.1 | `examples/step01_room.ml` | The smallest complete game: one room, one call to `Run.play`. ~40 lines. This is the program Phase 2–4 exist to serve. |
| 1.2 | `examples/step08_state.ml` | The first *component*: `Element.declare`, `Hook.use_state`, an event. The declarative idea in one screen. |
| 1.3 | `demo/masonry.ml` | A real demo file: materials from one pattern function. Note the shape every demo shares: `level` (or a component), `world`, `run`. |
| 1.4 | `demo/catalogue.ml` | The list of all demos — your index for later. Each entry's `blurb` tells you which demo isolates which engine feature. |

**Checkpoint.** You can answer: *What does a game hand to the engine each
frame — a world, or a description of one? Where does state live if the
description is rebuilt from scratch?* (Answers: a description; in the
instance tree the reconciler keeps — which is Phase 3's subject.)

---

## Phase 2 — The platform: `core/` bottom-up

*~6–9 hours; it is ~10,000 lines including interfaces, but evenly
documented. Goal: understand how a frame is drawn and how a player moves.*

`core/` is thirty modules with a strict property, stated in `doc/index.mld`:
**each module depends only on the ones under it**. So reading them in
the order below means never meeting a name you have not already read. The
modules cluster naturally; take a cluster per sitting.

Throughout, the companion guide is **`doc/building-the-engine.mld`**
("Building the engine from scratch") — it rebuilds this exact renderer by
hand in eight phases with every derivation written out. Cluster headers below
name the matching guide phases.

### Cluster A — Ground rules (small, fast)

*Guide: Phase 0.*

| # | files | what to look for |
|---|-------|------------------|
| 2.1 | `core/config.mli` (+ `.ml`) | Every tunable constant in one place: eye height, portal depth, fog. Skim; return whenever a number appears elsewhere. |
| 2.2 | `core/key.mli` | Keys are *positions on the board*, not letters, so bindings survive keyboard layouts. |
| 2.3 | `core/result_ext.mli` | `let*` / `let+` over `result`, and acquire/use/release for SDL resources. **Concept:** OCaml binding operators — see [Real World OCaml, "Error Handling"](https://dev.realworldocaml.org/error-handling.html). Test: `test/test_result_ext.ml`. |
| 2.4 | `core/extent.mli` | Validating that a pixel rectangle is an array-sized length, with the overflow-proof division trick. Test: `test/test_extent.ml`. |
| 2.5 | `core/vec.mli` | Immutable 2-D vectors; dot and cross. The doc comments fix the angle convention everything else uses. Test: `test/test_vec.ml`. |

### Cluster B — Geometry of rooms and seams

*Guide: Phase 1 (vectors, the idea of raycasting), Phase 4.1 (planes),
Phase 7.1 (rigid motions).*

| # | files | what to look for |
|---|-------|------------------|
| 2.6 | `core/transform.mli` (+ `.ml`) | Rigid motions between room-local coordinate frames — the machinery that makes portals possible. The `.mli` derives why linked doorways pair *in reverse*. Test: `test/test_transform.ml`. |
| 2.7 | `core/plane.mli` (+ `.ml`) | An inclined floor/ceiling as a plane, and the one-line equation that casts it per pixel. This equation is the heart of Phase 2's rendering clusters. Test: `test/test_plane.ml`. |

**Background, if new to you:** any linear-algebra refresher on 2-D rotation
matrices and change-of-basis; e.g. 3Blue1Brown's
[Essence of Linear Algebra](https://www.3blue1brown.com/topics/linear-algebra)
(chapters on linear transformations and change of basis).

### Cluster C — What surfaces look like

*Guide: Phase 3 (colour, shading, fog, textures).*

| # | files | what to look for |
|---|-------|------------------|
| 2.8 | `core/color.mli` | 8-bit RGB, shading and blending — the only colour representation in the engine. Test: `test/test_color.ml`. |
| 2.9 | `core/bitmap.mli` | Decoding PNG/JPEG to plain bytes via `tsdl-image`; *the one place pixel formats appear*. Tests: `test/test_bitmap.ml`, `test/test_image.ml`; fixtures live in `test/fixtures/`. |
| 2.10 | `core/asset.mli` | Where files are found at runtime (relative to the executable, `CAMLCAST_ASSETS` override). Explains README's note about `_build/default/assets/`. Test: `test/test_asset.ml`. |
| 2.11 | `core/texture.mli` (+ `.ml`) | Procedural patterns and the wrapping value noise they are built from; `generate` vs `load`. Test: `test/test_texture.ml`. **Concept:** value noise — [Scratchapixel's "Value Noise"](https://www.scratchapixel.com/lessons/procedural-generation-virtual-worlds/procedural-patterns-noise-part-1/introduction.html). |
| 2.12 | `core/material.mli` | Thin: what a surface is made of. Test: `test/test_material.ml`. |
| 2.13 | `core/door.mli` | A leaf hung in a doorway: open/closed and of what material. |
| 2.14 | `core/atmosphere.mli` | Fog, haze, where the light comes from — "the air a world is seen through". Demo: `haze`. Test: `test/test_atmosphere.ml`. |
| 2.15 | `core/sky.mli` | The open sky for roofless rooms. Demo: `daylight`. Test: `test/test_sky.ml`. |
| 2.16 | `core/image.mli` | Full-colour images with alpha, for decals and sprites. |

### Cluster D — Pixels on a surface

*Guide: Phase 2.4 (a software framebuffer).*

| # | files | what to look for |
|---|-------|------------------|
| 2.17 | `core/framebuffer.mli` | A CPU pixel buffer and the SDL streaming texture it uploads through. **`Framebuffer.offscreen`** is why the whole test suite runs windowless — remember it when you read `test/`. |
| 2.18 | `core/paint.mli` | Clipped rectangles, lines and pictures over a finished frame (the HUD's substrate). Test: `test/test_paint.ml` reads pixels back. |
| 2.19 | `core/font.mli` | A bitmap font on a fixed grid: measuring, wrapping, clipping. Demo: `text`. Test: `test/test_font.ml`. |

### Cluster E — The world model (the heart; slow down here)

*Guide: Phase 2.1 (a room as segments), Phase 7 (rooms and doorways).*

| # | files | what to look for |
|---|-------|------------------|
| 2.20 | `core/room.mli` (+ `.ml`, 613 lines) | One room: wall segments, thresholds, floor/ceiling surfaces, sprites, decals, collision. **The winding rule is stated once, here, at the top** — everything else in the engine points to it. Test: `test/test_room.ml`; `test/support.ml` holds the hand-checkable 4×4 room used everywhere. |
| 2.21 | `core/world.mli` (+ `.ml`, 507 lines) | Named rooms, linked portals, spawn, atmosphere — and the three primitives a world *grows* by. The `.mli` documents the portal machinery. Demos: `portals`, `endless`. Tests: `test/test_world.ml`, `test/test_level.ml`. |
| 2.22 | `core/ray.mli` (+ `.ml`, 163 lines) | Ray-versus-segment intersection, every wall the ray crosses, farthest first — and the derivation of why the reported distance is **free of fish-eye**. The single most classic piece of raycaster math in the repo. Test: `test/test_ray.ml`. |

**Background on raycasting**, if the idea itself is new:

- [Lode Vandevenne's raycasting tutorial](https://lodev.org/cgtutor/raycasting.html)
  — the canonical grid-based DDA raycaster. Read it to understand the family
  of technique; then notice everything CamlCast does *differently*: segments
  at any angle instead of grid cells, so intersection is ray-vs-segment, not
  DDA.
- Fabien Sanglard's
  [*Game Engine Black Book: Wolfenstein 3D*](https://fabiensanglard.net/gebbwolf3d/)
  (free PDF) — the historical grid raycaster, for depth.
- For the *portal* idea (rooms joined at doorways, rendered recursively into
  the opening), the closest classical relative is the Build engine /
  sector-and-portal school: Fabien Sanglard's
  [Build engine internals](https://fabiensanglard.net/duke3d/build_engine_internals.php).
  CamlCast's twist — each room in its *own* coordinate frame with a
  `Transform` across each link — is documented in `core/world.mli` and
  `core/transform.mli` themselves.

### Cluster F — The camera and the player's hands

*Guide: Phase 2.3 (player and viewport), Phase 2.6 (movement with sliding),
Phase 5 (mouse look, resizing).*

| # | files | what to look for |
|---|-------|------------------|
| 2.23 | `core/player.mli` (+ `.ml`) | The camera as position + unit `dir` + unit `right`, plus pitch. Movement and collision (wall sliding) live here. Test: `test/test_player.ml`. |
| 2.24 | `core/viewport.mli` (+ `.ml`) | Window size → camera geometry: the projection, eye height, pitch *shear*, and the **Hor+** resize rules (vertical FOV fixed; wider window shows more world). Explains README's "Controls" notes. Test: `test/test_viewport.ml`. |
| 2.25 | `core/sight.mli` (+ `.ml`, 306 lines) | What the crosshair is on, traced through doorways with per-texel alpha respected. Feeds `lib/aim.ml` later. Demo: `targets`. Test: `test/test_sight.ml`. |
| 2.26 | `core/input.mli` | Keyboard and mouse *as they are*: controls, edges (`pressed`/`released`), holds, analog reads. No meanings assigned. Test: `test/test_input.ml`. |
| 2.27 | `core/binding.mli` (+ `.ml`) | Meanings: a game's table from controls to movement axes. The rate-vs-displacement distinction (held key = rate paid out over the frame; mouse = displacement added as-is) is documented here and in README. `Binding.motion` is the one pure function. Demo: `controls`. Test: `test/test_binding.ml`. |

### Cluster G — The frame, the clock, the loop

*Guide: Phase 2.5, Phase 4, Phase 6 (transparency, decals, sprites,
compositing), Phase 8.*

| # | files | what to look for |
|---|-------|------------------|
| 2.28 | `core/renderer.mli` (+ `.ml`, 800 lines — the biggest file in the repo) | The software renderer. Read the `.mli` overview first, then the `.ml` top-down: floor/ceiling/sky cast per pixel; opaque walls painted over them back-to-front with decals; then sprites and see-through walls *together*, farthest first (so a sprite can stand in front of one window and behind another); portal recursion bounded by `Config.max_portal_depth`. Test: `test/test_renderer.ml` renders whole frames offscreen and checks pixels. |
| 2.29 | `core/clock.mli` | Frame pacing arithmetic, apart from any window. **Concept:** game loops and timesteps — Glenn Fiedler's ["Fix Your Timestep!"](https://gafferongames.com/post/fix_your_timestep/). Test: `test/test_clock.ml`. |
| 2.30 | `core/engine.mli` (+ `.ml`) | Window lifetime, fullscreen, focus, and `Engine.run` — the game loop over a caller-supplied state type (`update` / `view` / optional `overlay`, `pointing`, `finished`, `bindings`). Also `Engine.run_world`, the loop over a bare `World` — *the floor the declarative layer stands on*, and what a game reaches for when it needs the platform directly. Demo: `phases`. Tests: `test/test_engine.ml`, `test/test_dynamics.ml`. |

**Checkpoint.** You can answer: *Why is the wall-distance free of fish-eye?
How does the renderer draw a room seen through a doorway, and what stops the
recursion? Why does a resize reveal more world instead of stretching it? In
what order are sprites and transparent walls composited, and why together?*

---

## Phase 3 — The declarative runtime: `loom/`

*~3–4 hours; ~2,000 lines and none of the files is long, but two are dense.
Goal: understand how state survives a description rebuilt every frame.*

`loom/` depends on **nothing but the OCaml standard library** — reread
`loom/dune` for why, because every design choice in this directory follows
from that line. The companion guide is **`doc/building-the-layer.mld`**
("Building the layer from scratch"), whose final section — "What to read in
the source" — gives the dependency order this phase follows.

**Concepts to have in hand before starting** (each is central here):

- **React's model** — elements vs instances, reconciliation, hooks. Best
  compact sources: Rodrigo Pombo's
  [*Build your own React*](https://pomb.us/build-your-own-react/) (didactic
  reimplementation, ~1h), and from the React docs
  [Preserving and Resetting State](https://react.dev/learn/preserving-and-resetting-state)
  and [Rules of Hooks](https://react.dev/reference/rules/rules-of-hooks).
  You will find every one of those ideas here, in OCaml, sharpened.
- **OCaml 5 effect handlers** — hooks here are *not* implemented with global
  mutable current-fiber pointers as in React; they are delivered through
  effects. Read the
  [OCaml manual chapter on effect handlers](https://ocaml.org/manual/5.2/effectshandlers.html)
  first; the deeper story is *Retrofitting Effect Handlers onto OCaml*
  (Sivaramakrishnan et al., PLDI 2021,
  [paper](https://arxiv.org/abs/2104.00250)).
- **Type witnesses** — `Stdlib.Type.Id` gives contexts type safety without
  casts. Read the small
  [`Type.Id` documentation](https://ocaml.org/manual/5.2/api/Type.Id.html);
  the point is a runtime *proof* that two types are equal.
- **Functors** — the reconciler is one functor over a host module. Real World
  OCaml, [Functors](https://dev.realworldocaml.org/functors.html).

Then read, in order (`.mli` first throughout; the matching suite for the
whole directory is `test/test_loom.ml`, which drives everything against a
**mock host of plain strings** — that test file is itself worth reading as a
demonstration of why the empty `(libraries)` line matters):

| # | file | what to look for |
|---|------|------------------|
| 3.1 | `loom/path.mli` | A node's identity *and* its error-message label, in one value — and why those are different readings of the same thing. |
| 3.2 | `loom/element.mli` | What a component returns: pure description, holding no state, equal when describing the same thing. `Element.declare` and why a component's identity is its `render` function's physical identity. |
| 3.3 | `loom/trace.mli` | The reconciler will *say what it did*, so tests assert on decisions, not pixels. Free when unused. |
| 3.4 | `loom/hook.mli` (+ `.ml`, 337 lines) | The slot row: first `use_state` gets the first slot, **order is identity** — hence "the rule" (unconditional, same order, every render). Then the `.ml`: the effect handlers that deliver slots, the tag check that catches a moved hook, when a setter takes effect, why effects run after the frame. The densest file in the repo alongside `reconcile.ml`. |
| 3.5 | `loom/context.mli` | Values handed down a subtree without threading props — and the `Type.Id` witness that replaces React's any-cast. |
| 3.6 | `loom/store.mli` | Redux in 72 lines: immutable state, actions, one reducer, subscriptions. For state that belongs to no component. Background: [Redux — Core Concepts](https://redux.js.org/introduction/core-concepts). |
| 3.7 | `loom/reconcile.mli` (+ `.ml`, 405 lines) | The whole of the matching, in one functor: what counts as "the same thing" (component: same render function; host node: same type at the same place; keys override position), what is kept, what is torn down. Guide Phase 2 ("The tree that survives") is the walkthrough. |
| 3.8 | `loom/host.mli` | **The seam: two types and one function.** Note what is deliberately absent (no create/update/destroy mutation stream — the host gets the finished forest) and why that inverts React's host config. |

**Checkpoint.** You can answer: *Why must hooks be called unconditionally and
in the same order? What makes two components "the same" across frames? How
does a context read avoid an unsafe cast? What exactly does a host have to
provide?* (Property tests in `test_loom.ml` state the invariants crisply:
mounting A then B leaves the same instances as mounting B directly.)

---

## Phase 4 — The layer: `lib/` (where the halves meet)

*~3–4 hours; ~3,500 lines. Goal: see a description become a `World` and a
key-press become a handler call.*

`lib/` is the only library that knows both `loom/` and `core/`. Read in this
order:

| # | file | what to look for |
|---|------|------------------|
| 4.1 | `lib/camlcast.mli` | The public surface — the one module a game opens. Mostly re-exports; read it as a table of contents for this phase. Note what is *not* in it (`Engine`, `Renderer`, `World`, `Player`). |
| 4.2 | `lib/prim.mli` (+ `.ml`, 86 lines) | The primitives — "this engine's `div` and `span`": world, room, boundary, wall, doorway, sprite, decal, hud, text… All inert descriptions. `may_contain` is the nesting rule. |
| 4.3 | `lib/p.mli` (+ `.ml`) | The constructors a game actually writes (`P.world`, `P.room`, `P.boundary`, `P.sprite`, …) — the vocabulary layer over `Prim`. Compare with `examples/step01_room.ml` from Phase 1; it should now read as obvious. |
| 4.4 | `lib/scene.mli` | What one frame of description settles into: a world, maybe a camera, over/not, pointer state, HUD list, and every handler hung on something aimable. A record, and the `.mli` explains why. |
| 4.5 | `lib/nesting.mli` | `Prim.may_contain` applied to a whole tree — shared between `Host` and `Check` so the runtime and the checker cannot drift (the `.mli` tells the story of when they did). |
| 4.6 | `lib/host.mli` (+ `.ml`, 161 lines) | **The other half of Phase 3's seam:** `Camlcast_loom.Host.HOST` implemented for the raycaster. Once a frame, the settled forest of `Prim`s → the same `Room.make` / `World.make` a hand-written level always called. |
| 4.7 | `lib/mount.mli` | The instance tree held between frames; render a description in, get a `Scene.t` out. **No window here** — this is why levels are provable in tests. Test: `test/test_stage.ml` renders a described room beside its hand-built twin and compares every pixel (the layer's ground truth). |
| 4.8 | `lib/events.mli` | How a component reads the frame it is rendered for: `use_frame`, `use_pressed`, `use_held`, `aim`, `use_crossed` — all reads of a context that `Run` binds around each render. Test: `test/test_events.ml`. |
| 4.9 | `lib/aim.mli` (+ `.ml`) | From `core/sight`'s "wall 3 of room 2" to "the thing *your component* described" — and `on_use` / `on_gaze` handlers. Demos: `targets`, `chalk`. Test: `test/test_aim.ml`. |
| 4.10 | `lib/controls.mli` | One record for everything the loop acts on by itself: walk/look bindings, leave, use, map. |
| 4.11 | `lib/overlay.mli` | `Scene.hud` primitives → `Paint`/`Font` calls. One short fold. Test: `test/test_overlay.ml`. |
| 4.12 | `lib/debug_map.mli` | The room from above with its mistakes marked (winding ticks, unlinked doorways in red). Read after `Check` or before — it is the visual twin of it. Test: `test/test_debug_map.ml`. |
| 4.13 | `lib/check.mli` (+ `.ml`, 643 lines) | Static analysis for levels: unreachable rooms, spawn inside a wall, floor steps across doorways, double-linked doorways — errors named in the game's own terms. Guide: making-a-game Step 22. Test: `test/test_check.ml`. |
| 4.14 | `lib/run.mli` (+ `.ml`, 217 lines) | **The capstone.** `Run.play` / `Run.on` = one call to `Engine.run` with a `Mount` kept beside it: render description → `Scene` → world+player to `Engine`'s `view`, handlers wired to input, `Run.carry` holding the player across a growing world. When this file reads as a summary rather than news, the architecture is yours. |

**Checkpoint.** Trace, on paper, one frame end to end: SDL event → `Input` →
`Events.context` → components re-render → `Reconcile` against the mount →
`Host.assemble` → `Scene` → `Renderer.draw_frame` → `Framebuffer` upload.
Then trace one key-press to a `use_pressed` handler and note *when* the
handler runs (after the frame it fired on — `HACKING.md` explains why that
one-frame delay is the only visible difference from a pure update function).

---

## Phase 5 — Content as curriculum: examples, demos, integration tests

*~2–4 hours, à la carte. Goal: see every feature exercised, and how the
engine wants to be used.*

- **`examples/step01_room.ml` → `step26_shipping.ml`** beside the
  **making-a-game guide** (`doc/making-a-game.mld`). Each file is the
  *complete* game as that step leaves it, compiled with the tree so it cannot
  rot. If you did Phases 2–4 you can skim; the ones worth reading closely:
  `step18_effect.ml` (`use_effect` and the outside world), `step19` vs
  `step20` (shared state by hand, then with the store), `step22_check.ml`
  (proving a level without a window), `step25_launcher.ml` (`Run.on`, runs
  within runs).
- **Demos**, in roughly this order of ideas:
  `masonry` → `gallery` → `glass` → `slopes` → `daylight` → `haze` →
  `portals` → `doors` → `barred` → `changing` → `floating` → `dust` →
  `chalk` → `targets` → `trail` → `endless` → `phases` → `overlay` →
  `text` → `loading` → `controls` → `menu.ml` + `catalogue.ml` + `bin/demo.ml`
  (the launcher: `Run.on` in anger) → **`level.ml`** (the five-room showcase,
  386 lines, everything at once). Shared demo helpers: `demo/patterns.ml`,
  `surfaces.ml`, `pictures.ml`, `typeface.ml`, `bindings.ml`, `reading.ml`.
- **Integration tests:** `test/test_level.ml` (the showcase checked the way a
  player meets it), `test/test_demos` via `demo/catalogue.ml` enrolment
  (spawn standable, rooms enclosed, all reachable, no floor step across a
  doorway), `test/test_stage.ml` (described vs hand-built, pixel for pixel),
  `test/test_menu.ml` (the one visible reconciler-vs-pure-update difference).
- **`bench/frame.ml`** — the benchmark that settled "does describing a world
  every frame need a cache?" (no: ~0.14% of drawing it). Read the numbers in
  the file; `HACKING.md`'s Benchmarks section explains why it is an
  executable and not a test.

---

## Phase 6 — The build, the tests' own machinery, CI, tools

*~1 hour. Optional unless you plan to contribute.*

| # | file(s) | what to take from it |
|---|---------|----------------------|
| 6.1 | `test/support.ml`, `test/dune` | The shared 4×4 hand-checkable room, the two-room fixture, custom Alcotest testables; why the suites are two stanzas (one per package). |
| 6.2 | root `dune` | How `assets/` reaches `_build` and `share/`; the odoc alias. |
| 6.3 | `.github/workflows/ci.yml` | Build+test on dev compiler *and* at the OCaml 5.2 floor; `@fmt`; `@doc`; Pages deploy. |
| 6.4 | `.github/workflows/release.yml`, `platforms.yml`, `tools/bundle-*.sh` | A `v*` tag → per-platform self-contained bundles; how each platform's library-carrying problem is solved (`otool` walk / `ldd` walk / DLL copy). |
| 6.5 | `tools/pages-site.py`, `tools/odoc-refs.py`, `tools/make_art.py` | Why the docs site needs a script (dune's `documentation` stanza cannot carry images); the art generator. |
| 6.6 | `camlcast.opam.template`, opam files | The depexts story (macOS `sdl2-compat`). |

Background, as needed: the
[dune documentation](https://dune.readthedocs.io/) (especially
[`implicit_transitive_deps`](https://dune.readthedocs.io/en/stable/reference/dune-project/implicit_transitive_deps.html)),
the [Alcotest](https://github.com/mirage/alcotest) and
[QCheck2](https://c-cube.github.io/qcheck/) READMEs, and the
[tsdl](https://erratique.ch/software/tsdl/doc/Tsdl/index.html) /
[SDL2 wiki](https://wiki.libsdl.org/SDL2/FrontPage) pages for anything
SDL-shaped in `Framebuffer` and `Engine`.

---

## The fast path

If you have one afternoon, not a week:

1. `README.md` — "The libraries" + "The engine in one page" (15 min)
2. The four library `dune` files (15 min)
3. `examples/step01_room.ml`, then `examples/step08_state.ml` (15 min)
4. `core/ray.mli`, `core/plane.mli`, `core/transform.mli` — the three
   derivations (45 min)
5. `core/world.mli` — portals and growth (20 min)
6. `core/renderer.mli`, then the compositing walk in `renderer.ml` (30 min)
7. `loom/host.mli` — the seam, two types and one function (10 min)
8. `loom/hook.mli` and `loom/reconcile.mli` — interfaces only (30 min)
9. `lib/run.ml` — the capstone, one call to `Engine.run` with a mount
   beside it (20 min)

That is the architecture; everything else is detail hanging off it.

---

## Concept index — external resources in one place

All optional; the in-repo guides cover this project's own versions of each
idea and should be preferred where they overlap.

| concept | where it bites in this repo | resource |
|---|---|---|
| Raycasting (grid/DDA classic) | `Ray`, `Renderer`, guide Phase 1–2 | [Lodev's tutorial](https://lodev.org/cgtutor/raycasting.html); [Game Engine Black Book: Wolfenstein 3D](https://fabiensanglard.net/gebbwolf3d/) |
| Portal / sector rendering | `World`, `Transform`, `Renderer` | [Build engine internals](https://fabiensanglard.net/duke3d/build_engine_internals.php) |
| Perspective projection, FOV, shear | `Viewport` | guide Phase 2.3 & 5.1 (self-contained) |
| Value noise / procedural texture | `Texture` | [Scratchapixel: procedural patterns & noise](https://www.scratchapixel.com/lessons/procedural-generation-virtual-worlds/procedural-patterns-noise-part-1/introduction.html) |
| Game loop & timestep | `Clock`, `Engine` | [Fix Your Timestep!](https://gafferongames.com/post/fix_your_timestep/) |
| React model: elements, reconciliation, keys | `Element`, `Reconcile` | [Build your own React](https://pomb.us/build-your-own-react/); [Preserving and Resetting State](https://react.dev/learn/preserving-and-resetting-state) |
| Hooks and their rule | `Hook` | [Rules of Hooks](https://react.dev/reference/rules/rules-of-hooks) |
| Redux-style store | `Store` | [Redux core concepts](https://redux.js.org/introduction/core-concepts) |
| OCaml 5 effect handlers | `Hook` (delivery mechanism) | [OCaml manual: effects](https://ocaml.org/manual/5.2/effectshandlers.html); [Retrofitting Effect Handlers onto OCaml](https://arxiv.org/abs/2104.00250) |
| Type witnesses (`Type.Id`) | `Context` | [Stdlib `Type.Id` docs](https://ocaml.org/manual/5.2/api/Type.Id.html) |
| Functors, `.mli` discipline, binding operators | `Reconcile`, everywhere | [Real World OCaml](https://dev.realworldocaml.org/) — Functors, Files/Modules/Programs, Error Handling |
| Property-based testing | `test_loom.ml` | [QCheck2](https://c-cube.github.io/qcheck/) |
| SDL2 via OCaml | `Framebuffer`, `Engine`, `Input` | [tsdl docs](https://erratique.ch/software/tsdl/doc/Tsdl/index.html); [SDL2 wiki](https://wiki.libsdl.org/SDL2/FrontPage) |
| Dune boundaries | every `dune` file | [dune docs](https://dune.readthedocs.io/en/stable/) |
