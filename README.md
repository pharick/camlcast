# CamlCast — a raycasting engine

A small first-person raycasting engine in OCaml on top of SDL2 (`tsdl`). It
started Wolfenstein-style — an axis-aligned grid with flat floors — and has
since grown past that: rooms are built from walls at any angle, the floor is an
inclined plane, the ceiling is an inclined plane or open sky, walls have their
own heights and textures, and there is mouse look with pitch. The floor, ceiling
and sky are cast per pixel by a small software renderer; the walls are painted
over them back to front.

## Running

```sh
eval $(opam env --switch=. --set-switch)   # this repo uses a local switch
dune exec camlcast
dune test                                  # all suites
```

## Controls

| key / device | action                                |
| ------------ | ------------------------------------- |
| `W` / `S`    | walk forward / back                   |
| `A` / `D`    | strafe left / right                   |
| mouse        | look around (yaw and pitch)           |
| `←` / `→`    | turn left / right (keyboard fallback) |
| `↑` / `↓`    | look up / down (keyboard fallback)    |
| `F11`        | toggle fullscreen                     |
| `Esc`        | quit                                  |

The mouse is captured in relative mode, so the cursor is hidden and never
reaches a screen edge; `Esc` releases it and quits.

## Resizing and fullscreen

The window is resizable and `F11` toggles borderless fullscreen. No size is
cached anywhere: each frame asks SDL for the current drawable size and builds
a `Viewport` from it, which also covers HiDPI displays and the fullscreen
transition without a single resize event being handled.

Reshaping the window is **Hor+**: the vertical field of view is fixed, so
dragging the window wider reveals more of the world to the sides instead of
magnifying what was already on screen, and pixels stay square at every shape.
`Config.fov` is therefore the horizontal field of view at
`Config.reference_aspect` (4:3). `Viewport`'s docstring carries the
derivation.

## Rooms of any shape

A grid raycaster can only place walls on cell edges, so every wall faces north,
south, east or west. Here a `World` is instead a set of arbitrary line
**segments**, so a room can have any number of walls facing any direction. The
default level is a bounded arena of several distinct areas — a plaza of pillars,
an enclosed hall you walk into through a doorway, a triangular nook, a garden of
low walls you can see over, a lone monolith — none of it axis aligned, built
from the `World.path` and `World.regular_polygon` helpers. `Ray.cast` intersects
the ray with each wall segment directly (there is no grid to step through) and
keeps the ones it crosses; `Ray`'s docstring carries the cross-product
derivation. Movement collides with the segments too, refusing any step whose
path would cross a wall.

## Sloped floor, and ceiling or sky

The floor is an inclined `Plane`, `z = a*x + b*y + c`, so it need not be
horizontal — the default level's floor tilts into a shallow wedge. The ceiling
is an _optional_ inclined plane: a level either has a roof, or is left open to
the **sky** (`World.ceiling = None`, the default). Drawing all of this needs the
surface, and its distance, decided **per pixel**, which is why the renderer is
software: for each pixel `Plane.view_distance` solves a one-line equation for
how far away the floor (or ceiling) is along that line of sight. `Plane`'s
docstring derives the equation.

The floor and ceiling are textured the same way the walls are: at each pixel's
world point a greyscale `Texture` is sampled — tiled every world unit — tinted
by the surface's base colour, then faded by fog. Because the pattern is anchored
in world space, its features foreshorten and their rows tilt with the surface,
which is what makes the incline read at a glance.

The **sky** is different: it is an infinitely far backdrop, so its colour
depends only on the direction looked in — the azimuth of the column's ray and
how high up the pixel sits — never on where you stand. So it does not slide past
as you walk, only wheels around as you turn and tilts as you look up. It is a
horizon-to-zenith gradient with a soft sun; `Sky` holds it.

## Wall heights and textures

Each wall carries its own height, and because the eye sits half a cell up a
short wall drops below its neighbours while a tall one rises above them. For that
to read correctly a ray cannot stop at the first wall it meets, so `Ray.cast`
returns _every_ wall it crosses, farthest-first, and the renderer paints them
back to front — a near wall covers the ones behind it, a tall one still shows
over a short one, and both cover the background behind them. Under a roof, a wall
too tall for the sloped ceiling is capped to it so it never pokes through; under
open sky it simply rises to its full height with sky above.

Walls are textured. The patterns — brick, bevelled panel, stone, checker — are
generated in code, so the project stays free of binary assets and every pattern
is a pure, testable function of its texel coordinates. They are **greyscale**: a
texel is a brightness, not a colour. The colour arrives at draw time, when the
renderer tints the sampled texel by the wall's palette colour, dimmed by fog and
by how squarely the wall faces the light — so one pattern can dress a wall of any
colour. (There is no floor casting of the wall _tops_, so you see the walls'
faces, not their flat tops.)

## Transparency, decals and sprites

Three kinds of extra detail sit on top of the walls:

- **See-through walls.** A `Texture` carries a per-texel _alpha_, so a wall can
  be a grille (solid bars, clear gaps) or a leaded window (translucent panes).
  Where a texel is not solid the wall is _blended_ rather than written, unveiling
  the room behind.
- **Decals.** Pictures — an `Image` (full colour with its own alpha, unlike a
  greyscale `Texture`) — hung on a wall at a given position and size. The wall
  pass blends each decal over its own texture, in the same light, so paintings
  and posters sit on the wall.
- **Sprites.** Objects and characters placed in the world as billboards — flat
  `Image`s that always face the player. A sprite is projected onto the sloped
  floor at its position and blended in wherever it stands nearer than the wall.

The opaque walls come first and record their distance into a **per-pixel** depth
buffer (per pixel, so a short wall only hides its own strip — the top of a
sprite or window behind it still shows). Then the translucent things — the
sprites and the see-through walls — are composited together, farthest first,
each hidden where an opaque wall is nearer. Sorting them together is what lets a
sprite cover a window in front of it and show through one behind it.

The default level shows all three: a wall hung with a painting and a poster, a
grille and a window each with something behind them, and a barrel and a couple of
figures standing about. `Renderer` and `Image` carry the details.

## Looking up and down

A raycaster has no true vertical rotation — every ray stays in the ground plane,
which is what keeps walls vertical. Pitch is faked by shearing the whole image
up or down: the horizon, the row the eye looks along, slides away from the
middle of the window, and the planes and every wall are measured from it, so
they all follow. The mouse drives both yaw and pitch; `Viewport` and `Player`
carry the derivation and the clamp that stops the shear from tipping too far.

## Modules

Each module is self-contained and depends only on the ones above it.

| module              | responsibility                                                                                                             |
| ------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `Config`            | all tunable constants                                                                                                      |
| `Result_ext`        | `let*` / `let+` and a short-circuiting range loop for fallible SDL calls                                                   |
| `Vec`               | immutable 2-D vectors, with the dot and cross products the geometry needs                                                  |
| `Plane`             | an inclined floor/ceiling plane, and the per-pixel casting equation                                                        |
| `World`             | wall segments (with decals), the floor, the optional ceiling, sprites, collision; level-building helpers                   |
| `Ray`               | ray-versus-segment intersection; every wall the ray crosses, farthest first                                                |
| `Player`            | camera pose: `pos` + unit `dir` + unit `right` + `pitch`; movement with wall sliding                                       |
| `Viewport`          | window size → camera geometry, projection, eye height and the pitch shear; the resize rules                                |
| `Texture`           | procedural greyscale wall and plane patterns, with a per-texel alpha for see-through walls                                 |
| `Color` / `Palette` | colours, blending, orientation shading, fog, and the wall and plane tables                                                 |
| `Sky`               | the open sky drawn where there is no ceiling — a directional gradient with a sun                                           |
| `Image`             | full-colour images with alpha, for wall decals and sprites                                                                 |
| `Input`             | SDL keyboard and mouse-look → engine intent                                                                                |
| `Framebuffer`       | a CPU pixel buffer (with alpha blending) and per-pixel depth, and the streaming texture it uploads through                 |
| `Renderer`          | the software renderer: floor/ceiling/sky, opaque walls with decals, then sprites and see-through walls composited by depth |
| `Engine`            | window lifetime, fullscreen state, and the game loop                                                                       |

## Documentation

The modules are documented with odoc comments (`(** ... *)`), and the maths
derivations live there rather than in this file.

```sh
opam install odoc            # once
dune build @doc
open _build/default/_doc/_html/index.html
```

`doc/index.mld` is the landing page; `lib` is published as
`camlcast.raycaster`, which is what makes `@doc` pick its modules up (odoc
skips private libraries).

For a from-scratch walkthrough that rebuilds the whole engine feature by feature
— with the concepts, the derivations and the code — see
**[`TUTORIAL.md`](TUTORIAL.md)**.

## Tests

[Alcotest](https://github.com/mirage/alcotest), one executable per module in
`test/`. They share `test/support.ml`, which holds a hand-checkable 4x4 square
room (walls two cells from the centre in every direction) and the custom
testables — a failing `Vec` check prints `(3, 2.5)` rather than a bare `false`.

```sh
dune exec test/test_player.exe -- --verbose   # one suite
dune exec test/test_ray.exe -- test hits      # one group
```

`Input`, `Framebuffer` and `Renderer` are not covered directly: they all need a
live SDL surface. Their pure logic lives in `Plane` (the casting equation),
`Viewport` (the projection and resize rules) and `Palette` (the shading), which
are tested on their own.

## How the rendering works, in one paragraph

Each frame is drawn into a CPU pixel buffer, one column at a time. For a column
we take the ray's horizontal direction — built as `dir + right * k` with `dir` a
unit vector, so the distance it yields is _perpendicular to the camera plane_
and needs no fish-eye correction. First the background: for a pixel below the
horizon `Plane.view_distance` solves `d = (eye_z - base) / (row_factor +
gradient)` for how far along that line of sight the floor is, giving a world
point where a world-space texture is sampled, tinted and fogged; above the
horizon it is the ceiling the same way, or — with no roof — the sky, whose colour
comes from the column's azimuth and the pixel's elevation alone. Then the opaque
walls: `Ray.cast` intersects the ray with every wall segment and returns the
crossings farthest-first; each wall is drawn from its foot on the sloped floor up
to its height (capped at the ceiling, if any), its texture sampled per pixel and
tinted, its decals blended on top, painted over the background and the walls
behind it — and its distance kept in a **per-pixel** depth buffer. After every
column, one more pass composites the translucent things — the billboarded
sprites and the see-through walls — together, farthest first, each hidden per
pixel where an opaque wall is nearer (so a short wall in front hides only the
lower part of what is behind, and its top still shows). The buffer is uploaded
once and the GPU scales it to the window. `Plane`, `Sky`, `Ray` and `Viewport`
carry the full derivations in their docstrings.

## License

MIT — see [LICENSE](LICENSE).
