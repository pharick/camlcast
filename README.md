# CamlCast — a raycasting engine

A first-person raycasting engine in OCaml on top of SDL2 (`tsdl`). It started
Wolfenstein-style — an axis-aligned grid with flat floors — and has since grown
past that: a world is a graph of rooms built from walls at any angle, each in its
own coordinate frame with its own inclined floor and its own ceiling or open sky,
joined at doorways you both see and walk through. The floor, ceiling and sky are
cast per pixel by a small software renderer; the walls are painted over them back
to front.

This repo is the engine and the demos it was written against. A game built on it
lives in its own repo and brings its own content — see
[Building a game on it](#building-a-game-on-it).

## Running

```sh
eval $(opam env --switch=. --set-switch)   # this repo uses a local switch
dune exec camlcast-demo                    # what there is to look at
dune exec camlcast-demo portals            # one of them
dune test                                  # all suites
```

The demos that read art from files find it relative to the executable, which
under dune is `_build/default/assets/` — `dune build` puts it there. Set
`CAMLCAST_ASSETS` to a directory to look there instead, and only there.

## The demos

One small world per engine feature, and one that has all of them at once. Each
is a single file under `demo/`, short enough to read in a sitting, with the
feature it demonstrates as the only thing in it — and each spawns you facing
the thing it is about.

| demo       | what it shows                                                     |
| ---------- | ----------------------------------------------------------------- |
| `masonry`  | materials: one pattern function, applied at several colours       |
| `gallery`  | decals fixed to walls, sprites standing in the room               |
| `glass`    | see-through walls and the translucent pass                        |
| `slopes`   | inclined floors and roofs, and a threshold with no seam           |
| `daylight` | the open sky, and two rooms under different ones                  |
| `haze`     | atmosphere: the fade into the distance, and where the light falls |
| `portals`  | doorways: one room, built once, joined in two places              |
| `changing` | replacing a room: an animated sign, rebuilt every frame           |
| `floating` | sprites off the floor, and frames chosen rather than made         |
| `dust`     | a chamber of falling dust: every mote moved every frame           |
| `chalk`    | marking a wall where the crosshair is, on the face you see        |
| `endless`  | the `grow` hook: a corridor built as you walk down it             |
| `doors`    | doors that open and shut, and a lock that is the game's own rule  |
| `targets`  | what the crosshair is on, through the doorway in front of you     |
| `trail`    | traversal traces: a return route built from the doorways crossed  |
| `phases`   | `run_state`: a phase, a clock, and a light going out              |
| `overlay`  | drawing over the finished world                                   |
| `controls` | press versus hold, mouse buttons, and letting go of the cursor    |
| `loading`  | art read from files, beside the generated kind                    |
| `text`     | a bitmap font: wrapping, measuring, clipping and colour           |
| `showcase` | the five-room level, with all of the above at once                |

Every one of these worlds is checked by `test_demos`: that you can stand where
it spawns you, that its rooms enclose themselves, that every room is reachable,
and that no floor steps across a doorway. Adding a demo to `Catalogue.demos` is
also adding it to that suite.

## Two libraries

The engine holds no content — not one colour, pattern, picture or room. What it
has instead are the types those things are values of, so a game supplies its own
and two games can share an engine without sharing a look.

| directory | library              | what it is                                                     |
| --------- | -------------------- | -------------------------------------------------------------- |
| `lib/`    | `camlcast.raycaster` | the engine: geometry, ray casting, rendering, SDL              |
| `demo/`   | `camlcast.demo`      | the demos and the art they are made of, run by `camlcast-demo` |

Nothing in the engine depends on `demo/`, which is the point: it is content, and
it lives outside the library it is content for. It stays in this repo because
between them the demos exercise every corner of the engine — decals,
see-through walls, sloped floors, the open sky, growth, overlays and input — so
a change that breaks any of them breaks something you can walk through here.

A wall carries its `Material` — the `Texture` it wears, which carries its own
colours — by value, the way a decal has always carried its `Image`. A material
is where a surface's other properties will go as they arrive; today the pattern
is the only one. That replaced an integer id
looked up in a global table, which had two faults: it put the whole look of the
game in one module every level had to share, and an id nobody had defined fell
through to a default grey instead of failing. A world likewise carries its
`Atmosphere`: how fast it fades things out, what colour it fades them to, and
where its light comes from. Those five numbers are most of what tells one place
from another.

## Building a game on it

`Engine.run_state` runs a state of whatever type the game likes, and asks five
things of it: `update`, given the frame's length, the movement asked for over it
and what the player is pressing and holding through it; `view`, which world and
which player to draw this frame from; `overlay`, anything drawn over the finished
picture before it reaches the screen; `pointing`, whether the mouse is working a
cursor over something the game has drawn instead of looking around; and
`finished`, whether the run is over. Everything else a game keeps — phases,
doors, a journal, a random seed, a record of what it has already built — stays on
the game's side of the line, and the engine stays a pure function of what it is
handed. Time passes only while the window has focus, so a game left behind
another window is not handed the minutes it spent there when it comes back.

Input arrives as controls rather than as actions: `Input.pressed`,
`Input.released`, `Input.down` and `Input.held_for`, over a `Key` named by its
`Sdl.Scancode` or a mouse `Button`. What "interact" or "journal" mean is a game's
own table from the one to the other — the engine knows only that something went
down, came up, or has been held for so many seconds. Answering `true` to
`pointing` releases the mouse capture and puts a real cursor back on the screen,
whose position arrives in `update` in the framebuffer's own coordinates, which
are the ones `overlay` draws in.

`Engine.run` is that loop over the only state the engine used to be able to
hold: a `World` and the player walking it, plus an optional `grow : World.t ->
Player.t -> World.t`, which it calls whenever the player crosses into another
room. A fixed level needs nothing more. `Esc` quitting is this wrapper's rule
rather than the engine's, because a game with screens in it wants that key for
closing them.

A game supplies the rest by construction: its own `Material`s and `Texture`s, its
own `Atmosphere`, its own `Room`s assembled from `Room.wall`, `Room.doorway` and
`Room.path`, joined with the three append-only primitives `World.open_doorway`,
`World.add_room` and `World.link`. Most of the demos do this statically, rooms
written out by hand; `endless` does it incrementally with those three
primitives, which is how a game grows a building under the player's feet through
the same `grow` hook.

To depend on the engine from another project, pin it:

```sh
opam pin add camlcast git+https://github.com/pharick/camlcast.git
```

then `(libraries camlcast.raycaster)` in your `dune`.

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

Two demos add to this: `phases` starts on `space`, and `controls` uses `E`,
`Tab` and the left mouse button — each says so at the top of its own file.

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

## Rooms and doorways

A `Room` is authored in its own local coordinates from arbitrary wall
**segments**, so rooms may overlap numerically while remaining separate places.
A `World` names those rooms and links named thresholds. The link derives a rigid
rotation and translation, reversing the paired endpoints because boundary
half-edges describe the same opening from opposite sides. Threshold endpoints
must therefore follow the room boundary's winding direction.

An open threshold is a portal: the neighbouring room is rendered recursively,
clipped exactly to the doorway, and walking through transforms the camera pose
into its frame. A solid threshold instead shows the leaf of a door, while
walking into it still crosses the link — so a closed door is a room you have to
enter to find out about. Either way the threshold's **lintel**
fills the strip of wall left standing above the opening — without it you would
see over the top of a closed door. Portal recursion is depth-capped and ends in
haze, so cyclic room graphs terminate.

A doorway is a gap in a room's boundary, so nothing in that room stops a step
taken through it. `World.can_step` therefore also carries any step that comes
near a threshold into the neighbour's frame and asks again there, otherwise you
clip through the far side's wall while straddling the opening. A leaf makes no
difference to that: a door has to be walked into to be gone through, and asking
the neighbour cannot make it solid, because a leaf is a threshold and collision
measures against walls. And because a step is resolved one axis at a time,
whichever leg goes through a doorway carries the pose into the next room before
the other leg is taken — a diagonal through an opening finishes in the room it
ended up in, checked against that room's walls.

`Room.path`, `Room.regular_polygon` and `Room.doorway` build the geometry —
`doorway` splits a wall around a gap and returns the jambs together with the
threshold that fills it, so a boundary and its openings cannot drift apart.
`World.make` rejects the authoring mistakes that would make a link meaningless:
unknown names, a threshold linked twice or not at all, linked thresholds that
differ in length or height, and linked thresholds that disagree about a door —
a leaf on one side and an opening on the other would be a door you could see
through from behind. A world can also be **grown** rather than
authored: `open_doorway`, `add_room` and `link` each append and nothing else, so
every index anything is holding — a player's room, a portal's twin — keeps
meaning what it meant. Between cutting a doorway and linking it, the doorway
leads nowhere; the renderer fills it with haze and collision treats it as solid,
so a half-built world is a playable one rather than a crash waiting for someone
to walk towards it. `Ray.cast` intersects the ray with each wall
segment directly (there is no grid to step through) and keeps the ones it
crosses, while `Ray.openings` does the same for thresholds; `Ray`'s docstring
carries the cross-product derivation. Movement collides with the segments too,
refusing any step whose path would cross a wall.

## Sloped floor, and ceiling or sky

The floor is an inclined `Plane`, `z = a*x + b*y + c`, so it need not be
horizontal — the demo world's floor tilts into a shallow wedge. The ceiling is
an _optional_ inclined plane chosen per room: one room may have a roof while its
neighbour is open to the **sky** (`Room.ceiling = Open sky`). Drawing all of this
needs the surface, and its distance, decided **per pixel**, which is why the
renderer is software: for each pixel `Plane.view_distance` solves a one-line
equation for how far away the floor (or ceiling) is along that line of sight.
`Plane`'s docstring derives the equation.

Each room has its own floor, and nothing forces two of them to meet: where they
disagree, the doorway between them has a visible step in it. `World.seam_gap`
reports that disagreement, and `Plane.through` avoids it — it re-expresses one
room's plane in a neighbour's frame, so the demo world's five rooms all share
one continuous floor and every seam is zero by construction.

The floor and ceiling are textured the same way the walls are: at each pixel's
world point a `Texture` is sampled — tiled every world unit — and faded by fog.
Because the pattern is anchored
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

Walls are textured. A pattern is either generated in code — a pure, testable
function of its texel coordinates, which is what the demos and every test here
use — or read from a PNG or JPEG with `Texture.load`. Both produce the same
`Texture.t`, and the renderer has no way to ask which it was given. A texel is a
**colour**, so a wall can have more than one in it: brick with grey mortar
between it, lead around a pale pane. Between the pattern and the screen there is
nothing but the air — fog, and how squarely the wall faces the light.

One pattern still dresses many colours, by taking them as arguments before its
texel coordinates and being applied partially:

```ocaml
let brick ~color ~mortar ~u ~v =
  if in_mortar ~u ~v then Color.level mortar 210
  else Color.level color (225 + grain ~u ~v)

let red  = Texture.generate (brick ~color:clay  ~mortar:lime)
and grey = Texture.generate (brick ~color:slate ~mortar:lime)
```

`Color.level` is the join: patterns compute in the 0..255 that `Texture.noise`
and the hashes beside it speak, and it turns that back into a colour by scaling
all three channels together — moving value without touching hue. What it costs
is an array per colour where two materials used to share one.

The demos' patterns are masonry: brick, bevelled panel, stone, checker. A
game's could be the opposite — say three octaves of wrapping value noise in a
band about forty levels wide, with no courses, no joints and no grout lines. The
engine has no opinion either way: `Texture.generate` takes a function from texel
coordinates to a colour, and that is the whole of the interface.

The noise lattice wraps at the texture's size, which is what makes that second
kind possible at all: a wall's pattern tiles once per world unit, so a field that
did not wrap would put a hard seam down every wall, one per cell — and a pattern
with no courses to hide behind would leave that seam the only thing in the place
with a shape. (There is no floor casting of the wall _tops_, so you see the
walls' faces, not their flat tops.)

## Transparency, decals and sprites

Three kinds of extra detail sit on top of the walls:

- **See-through walls.** A `Texture` carries a per-texel _alpha_, so a wall can
  be a grille (solid bars, clear gaps) or a leaded window (translucent panes).
  Where a texel is not solid the wall is _blended_ rather than written, unveiling
  the room behind.
- **Decals.** Pictures — an `Image`, which is a `Texture`'s colour with a
  rectangle's freedom of shape — hung on a wall at a given position and size.
  The wall
  pass blends each decal over its own texture, in the same light, so paintings
  and posters sit on the wall. An `Image` is a rectangle rather than a square,
  because a poster is: it is indexed across by its width and down by its height,
  and either extent may be whatever it was drawn at. A decal is on one **face**
  of its wall and is not visible from the other, which is what makes a chalk
  mark a mark rather than a hole. `Room.add_decal` puts one on at run time, in
  the wall's own coordinates — which are exactly the ones `Sight` reports for
  whatever the crosshair is on, so aiming and marking need no conversion
  between them. A decal is lit exactly as the wall under it is, unless it is
  given a **`glow`**: how much of its own light it makes, from paint at `0.` to
  undimmable at `1.`. That is what lets a mark stay readable in a room whose
  light has failed, without the light having to be implemented in some
  particular way to spare it.
- **Sprites.** Objects and characters placed in the world as billboards — flat
  `Image`s that always face the player. A sprite is projected against the sloped
  floor at its position and blended in wherever it stands nearer than the wall.
  It need not stand on that floor: a `base` floats its foot above it — measured
  from the floor, so on a slope it rides with the ground — which is what a mote
  of dust or a hanging lamp is. And it is as wide as its picture, not as wide as
  it is tall, so a wide, short image is drawn wide and short.

The opaque walls come first and record their distance into a **per-pixel** depth
buffer (per pixel, so a short wall only hides its own strip — the top of a
sprite or window behind it still shows). Then the translucent things — the
sprites and the see-through walls — are composited together, farthest first,
each hidden where an opaque wall is nearer. Sorting them together is what lets a
sprite cover a window in front of it and show through one behind it.

The demo world's plaza shows all three: a wall hung with a painting and a poster,
a grille and a window each with something behind them, and a barrel and a couple
of figures standing about. Seen through a doorway they are all clipped to the
opening itself rather than to a rectangle around it, because the portal mask is
kept per screen column. `Renderer` and `Image` carry the details.

Like a texture, an image can be generated or loaded: `Image.load` reads a PNG or
JPEG, colour and alpha both, and a file with no alpha of its own arrives solid.
The `loading` demo puts a loaded pattern, decal and sprite beside their
generated counterparts, which is the whole of the difference between them.

## Looking up and down

A raycaster has no true vertical rotation — every ray stays in the ground plane,
which is what keeps walls vertical. Pitch is faked by shearing the whole image
up or down: the horizon, the row the eye looks along, slides away from the
middle of the window, and the planes and every wall are measured from it, so
they all follow. The mouse drives both yaw and pitch; `Viewport` and `Player`
carry the derivation and the clamp that stops the shear from tipping too far.

## Drawing over the world

`Engine.run_state` takes an `overlay` callback, run on the finished framebuffer
after the world and before it reaches the screen — so what it draws is in front
of everything and clipped by nothing except the buffer's own edges.

`Paint` is what draws: clipped rectangles, lines, outlines and pictures.
`Framebuffer.set` and `blend` write without checking where, because the
renderer's own loops have clipped long before they call them, so every guard
against writing off the buffer lives in `Paint` instead.

`Font` is a bitmap font on a fixed grid. A glyph's place in the atlas is
arithmetic on its code point and its advance is the cell width, so a font is a
picture and four numbers — there is no metrics table to author beside the PNG or
to let drift out of step with it. Text is therefore monospaced. `measure` and
`wrap` are pure functions of a string and the two cell dimensions; `draw` puts
every glyph through `Paint`, so a line running off the edge is cut rather than
truncated by anything that measured it first. The atlas's colour is multiplied
by the colour `draw` is given, the same split `Texture` and `Material` make, so
one white typeface dresses every screen in the colour that screen wants.

The coordinates throughout are the framebuffer's, not the window's: the buffer is
a whole-number fraction of the window (`Renderer.internal_size`, capped at
`Config.max_render_height`) and the GPU stretches the result. Text is drawn once
at the atlas's own size and scaled with everything else, which is why a font for
this engine is designed small and legible rather than large and smooth. The
`text` demo shows all of it at once.

## Modules

Each module is self-contained and depends only on the ones above it.

| module              | responsibility                                                                                                             |
| ------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `Config`            | all tunable constants                                                                                                      |
| `Result_ext`        | `let*` / `let+` and a short-circuiting range loop for fallible SDL calls                                                   |
| `Vec`               | immutable 2-D vectors, with the dot and cross products the geometry needs                                                  |
| `Transform`         | rigid rotations and translations between room-local coordinate frames                                                       |
| `Plane`             | an inclined floor/ceiling plane, and the per-pixel casting equation                                                        |
| `Room`              | one independently-authored level: walls, thresholds, surfaces, ceiling or sky, sprites and collision                      |
| `World`             | named rooms, linked portals, the world's air and spawn, and the three primitives a world grows by                          |
| `Ray`               | ray-versus-segment intersection; every wall the ray crosses, farthest first                                                |
| `Player`            | camera pose: `pos` + unit `dir` + unit `right` + `pitch`; movement with wall sliding                                       |
| `Viewport`          | window size → camera geometry, projection, eye height and the pitch shear; the resize rules                                |
| `Color`             | 8-bit RGB, shading and blending                                                                                            |
| `Texture`           | colour surface patterns, generated or loaded, and the wrapping value noise they are built from                             |
| `Material`          | what a surface is made of: its pattern, and whether you see through it                                                     |
| `Door`              | a leaf hung in a doorway: open or closed, and what it is made of                                                           |
| `Atmosphere`        | the air a world is seen through: its fog, its haze, and where its light comes from                                         |
| `Sky`               | the open sky drawn where a room has no roof — a directional gradient with a sun                                            |
| `Image`             | full-colour images with alpha, for wall decals and sprites                                                                 |
| `Paint`             | clipped rectangles, lines and pictures drawn over a finished frame                                                         |
| `Font`              | a bitmap font on a fixed grid: cell lookup, measuring, wrapping and drawing                                                |
| `Surface`           | decoding a PNG or JPEG into plain bytes: the one place pixel formats appear                                                |
| `Asset`             | where a file is, searched relative to the executable rather than to a source tree                                          |
| `Input`             | SDL keyboard and mouse-look → engine intent                                                                                |
| `Framebuffer`       | a CPU pixel buffer (with alpha blending) and per-pixel depth, and the streaming texture it uploads through                 |
| `Sight`             | what the crosshair is on, traced through doorways and named by index                                                       |
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

`doc/index.mld` is the landing page; both libraries have a public name under the
`camlcast` package, which is what makes `@doc` pick their modules up (odoc skips
private libraries).

For a from-scratch walkthrough that rebuilds the whole engine feature by feature
— with the concepts, the derivations and the code — see
**[`TUTORIAL.md`](TUTORIAL.md)**.

## Tests

[Alcotest](https://github.com/mirage/alcotest), one executable per module in
`test/`, covering both libraries. They share `test/support.ml`, which holds a
hand-checkable 4x4 square room (walls two cells from the centre in every
direction), a pair of rooms joined through a doorway, and the custom testables —
a failing `Vec` check prints `(3, 2.5)` rather than a bare `false`.

```sh
dune exec test/test_player.exe -- --verbose   # one suite
dune exec test/test_ray.exe -- test hits      # one group
```

**Nothing here opens a window.** `Framebuffer.offscreen` builds a buffer with no
streaming texture behind it — the texture was the only part that needed one —
and `Renderer.draw_frame` fills a buffer with no SDL call in it. So `test_paint`
and `test_font` draw and read the pixels back, and `test_renderer` renders whole
frames: where a billboard lands, what a low wall hides of it, and what a doorway
trims it to are checked on the pixels rather than only in the arithmetic that
feeds them. Each of those tests draws the frame twice, with the sprite and
without, and takes the sprite to be the pixels that differ — which says where it
was drawn without claiming anything about the colour fog and shading left in it.

What that leaves uncovered is `Renderer.render`, which is the upload and the
present and nothing else, and `Input`, which reads SDL for the keyboard and the
mouse — though `Input.advance`, the edges and the hold timer, is a pure function
of two frames and a duration and `test_input` drives it directly.

`test_level.ml` is the closest thing to an integration test: it checks the demo
world the way a player meets it, which means an engine change that breaks
portals, sloped floors or the sky fails here rather than in a unit suite.

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
walls and thresholds: `Ray.cast` intersects the ray with every wall segment and returns the
crossings farthest-first; each wall is drawn from its foot on the sloped floor up
to its height (capped at the ceiling, if any), its texture sampled per pixel and
tinted, its decals blended on top, painted over the background and the walls
behind it. A threshold in the same sorted stream either draws a solid door or
recursively draws the linked room clipped to the opening. Its distance is kept
in a **per-pixel** depth buffer. After every
column, one more pass composites the translucent things — the billboarded
sprites and the see-through walls — together, farthest first, each hidden per
pixel where an opaque wall is nearer and restricted by the exact per-column
portal mask (so a short wall in front hides only the
lower part of what is behind, and its top still shows). The buffer is uploaded
once and the GPU scales it to the window. `Plane`, `Sky`, `Ray` and `Viewport`
carry the full derivations in their docstrings.

## License

MIT — see [LICENSE](LICENSE).
