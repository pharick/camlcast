# Building a Raycasting Engine from Scratch

A step-by-step tutorial for rebuilding this engine by hand, in OCaml on SDL2.
Each **phase** ends in something you can run and see, and each **step** adds one
idea. Read the concept, write the module, check the milestone, move on.

By the end you will have: rooms of walls at any angle, an inclined floor and
either an inclined ceiling or an open sky, per-wall heights and textures,
textured floor and ceiling, mouse look with pitch, a resizable/fullscreen
window, see-through walls, wall decals, and billboarded sprites — all drawn by a
small software renderer.

> **Prerequisites.** Basic OCaml (modules, records, variants, `let`), a little
> trigonometry, and the will to derive a few formulas. No graphics background is
> assumed — every projection is derived from similar triangles.

---

## Table of contents

- **Phase 0 — Setup**: project, window, game loop, the `Result` monad.
- **Phase 1 — Foundations**: vectors, and the core raycasting idea.
- **Phase 2 — A first 3D view**: segments, ray casting, the camera, a framebuffer, solid walls, movement.
- **Phase 3 — Making walls look real**: colour, shading, fog, textures, variable heights.
- **Phase 4 — Floor, ceiling, sky**: inclined planes, per-pixel casting, textured planes, the sky.
- **Phase 5 — Camera & window**: mouse look, pitch, resizing, fullscreen.
- **Phase 6 — Props**: a per-pixel depth buffer, transparent walls, decals, sprites.
- **Phase 7 — Assembly**: the engine loop, resource safety, tests, docs.
- **Appendix**: module map, constants, hard-won gotchas, exercises.

Guiding principles worth keeping as you go: **small self-contained modules**,
**readable maths with the derivation written down**, and the **`Result` monad**
instead of a staircase of error matches.

---

## Phase 0 — Setup

### Step 0.1 — The toolchain and project layout

Install an OCaml toolchain (opam + dune) and the SDL2 bindings, `tsdl` (which
needs the system `SDL2` library installed too, e.g. via your OS package manager
or Homebrew).

```
raycaster/
├── dune-project
├── lib/          # the engine, one module per file
│   └── dune
├── bin/          # the executable entry point
│   ├── dune
│   └── main.ml
└── test/         # unit tests (added in Phase 7)
```

`dune-project`:

```dune
(lang dune 3.21)
(package (name raycaster) (depends ocaml tsdl (alcotest :with-test)))
```

`lib/dune` — a **public name** matters later so `odoc` will document it:

```dune
(library (name raycaster) (public_name raycaster) (libraries tsdl))
```

`bin/dune`:

```dune
(executable (name main) (libraries raycaster))
```

### Step 0.2 — The `Result` monad

Every SDL call can fail (`('a, [`Msg of string]) result`). Rather than nest
`match`es, define binding operators once (`lib/result_ext.ml`):

```ocaml
let ( let* ) = Result.bind                 (* chain: stop at the first error *)
let ( let+ ) result f = Result.map f result

(* A Result-flavoured for-loop: stops and returns the first error. *)
let rec iter_range ~first ~last f =
  if first > last then Ok ()
  else let* () = f first in iter_range ~first:(first + 1) ~last f
```

Now `let* x = fallible () in ...` reads like normal code but short-circuits on
error.

### Step 0.3 — Config: constants in one place

Put every tunable number in `lib/config.ml` so the rest of the code reads as
logic, not magic numbers. We will fill it as we go; start with:

```ocaml
let window_title = "Raycaster"
let initial_width = 1024 and initial_height = 768
let fov = Float.pi /. 3.          (* 60°, the classic Wolfenstein value *)
let reference_aspect = 4. /. 3.   (* the shape fov is quoted for *)
let frame_delay = 16l             (* ms per frame ≈ 60 FPS *)
```

### Step 0.4 — A window and a game loop

Get a blank window that stays open until you press Escape. This also introduces
`with_resource`, the pattern we use so every SDL resource is freed even on error
(`Fun.protect` runs the finaliser no matter what).

```ocaml
open Tsdl
open Result_ext

let with_resource acquire release use =
  let* r = acquire () in
  Fun.protect ~finally:(fun () -> release r) (fun () -> use r)

let rec loop window event =
  (* Draining the event queue every frame is mandatory or the OS thinks the
     app hung. *)
  let quit = ref false in
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(enum (get event typ)) with
    | `Quit -> quit := true
    | `Key_down when Sdl.Event.(get event keyboard_scancode) = Sdl.Scancode.escape -> quit := true
    | _ -> ()
  done;
  if !quit then Ok ()
  else (Sdl.delay Config.frame_delay; loop window event)

let run () =
  with_resource (fun () -> Sdl.init Sdl.Init.(video + events)) (fun () -> Sdl.quit ())
  @@ fun () ->
  with_resource
    (fun () -> Sdl.create_window Config.window_title
                 ~w:Config.initial_width ~h:Config.initial_height Sdl.Window.(shown + resizable))
    Sdl.destroy_window
  @@ fun window -> loop window (Sdl.Event.create ())
```

> **Milestone 0.** `dune exec bin/main.exe` opens a black resizable window that
> closes on Escape. From here on, "run it" means this.

---

## Phase 1 — Foundations

### Step 1.1 — Vectors

The whole world is two-dimensional and flat; the 3-D look is an illusion we
produce later by choosing how tall to draw each wall. `lib/vec.ml`:

```ocaml
type t = { x : float; y : float }

let make x y = { x; y }
let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let scale v k = { x = v.x *. k; y = v.y *. k }
let length v = Float.hypot v.x v.y
let dot a b = (a.x *. b.x) +. (a.y *. b.y)

(* The 2-D cross product is a scalar (signed parallelogram area). It is zero
   exactly when the vectors are parallel — the heart of ray-vs-wall tests. *)
let cross a b = (a.x *. b.y) -. (a.y *. b.x)

let normalize v = let l = length v in if l = 0. then v else scale v (1. /. l)

(* 0 rad points along +x; angle grows clockwise on screen because y points down. *)
let of_angle a = { x = cos a; y = sin a }
let rotate v a = let c = cos a and s = sin a in
  { x = (v.x *. c) -. (v.y *. s); y = (v.x *. s) +. (v.y *. c) }

(* Perpendicular, i.e. a quarter turn — exact and cheaper than rotate by π/2. *)
let perp v = { x = -.v.y; y = v.x }
```

### Step 1.2 — The idea of raycasting

The plan: for every **vertical column of pixels** on screen, shoot **one ray**
from the player across the flat world, find the first wall it hits, and draw a
vertical strip whose **height encodes the distance** — near walls are tall, far
walls are short. Do that for all columns and the flat world looks 3-D.

Two facts make this cheap and correct:

**The camera plane.** We don't rotate a ray per column with trigonometry.
Instead we keep two unit vectors: `dir` (where the player looks) and `right` (a
quarter-turn from it). The ray for a column is

```
ray_direction = dir + right * camera_x
```

where `camera_x` runs from `-half_width` at the left edge of the screen to
`+half_width` at the right. `half_width` encodes the field of view. All columns
share `dir`; only the `right` term changes — one multiply and one add per
column.

**Why distances have no fish-eye.** We measure the hit distance `t` in *units of
that (un-normalised) `ray_direction`*. Project the hit onto the view direction:

```
(t · ray_direction) · dir = t·(dir·dir) + t·camera_x·(right·dir)
                          = t·1        + t·camera_x·0            = t
```

So `t` *is* the distance measured **perpendicular to the camera plane** — not
the straight-line distance to the player. That perpendicular distance is exactly
what a flat projection needs; using the true Euclidean distance instead makes
walls bulge toward you at the screen edges (the classic fish-eye), and this
formulation removes it with no `cos` correction anywhere.

> **Aside — grids vs segments.** Classic raycasters (Wolfenstein 3D) put walls
> on a grid and step the ray cell-to-cell with the DDA algorithm. We instead let
> walls be **arbitrary line segments**, so rooms can be any shape. The
> projection maths above is identical either way; only *how we find `t`* differs.
> We use segments from the start.

---

## Phase 2 — A first 3D view (solid walls)

### Step 2.1 — The world as segments

A level is a set of wall segments, plus (later) a floor/ceiling and sprites.
Start minimal in `lib/world.ml`:

```ocaml
type wall = {
  a : Vec.t; b : Vec.t;        (* endpoints *)
  height : float;              (* how far it rises above the floor, in cells *)
  texture : int;               (* selects colour/pattern later *)
  edge : Vec.t;                (* b - a, precomputed *)
  length : float;              (* |b - a| *)
  normal : Vec.t;              (* unit perpendicular, for shading later *)
}

type t = { walls : wall array; spawn : Vec.t }

let wall ~height ~texture a b =
  let edge = Vec.sub b a in
  { a; b; height; texture; edge; length = Vec.length edge;
    normal = Vec.normalize (Vec.perp edge) }

let make ~spawn walls = { walls = Array.of_list walls; spawn }

(* A hand-checkable 4×4 room: from the centre every wall is 2 cells away. *)
let default =
  make ~spawn:(Vec.make 2. 2.)
    [ wall ~height:3. ~texture:1 (Vec.make 0. 0.) (Vec.make 4. 0.);
      wall ~height:3. ~texture:1 (Vec.make 4. 0.) (Vec.make 4. 4.);
      wall ~height:3. ~texture:1 (Vec.make 4. 4.) (Vec.make 0. 4.);
      wall ~height:3. ~texture:1 (Vec.make 0. 4.) (Vec.make 0. 0.) ]
```

### Step 2.2 — Ray–segment intersection

Write the ray as `origin + t·direction` and a wall as `a + s·edge` with `edge =
b − a`. Setting them equal and solving with the cross product:

```
denom = direction × edge
t     = (a − origin) × edge      / denom
s     = (a − origin) × direction / denom
```

The ray meets the wall when `denom ≠ 0` (not parallel), `t > 0` (ahead), and `s
∈ [0,1]` (between the endpoints). Because we will later want to see *over* short
walls, we keep **every** wall the ray crosses, sorted **farthest-first** (the
order we will paint them). `lib/ray.ml`:

```ocaml
type hit = { distance : float; along : float; wall : World.wall }
let min_distance = 1e-4          (* so standing on a wall can't divide by zero *)

let cast (world : World.t) ~(origin : Vec.t) ~(direction : Vec.t) =
  let hits =
    Array.fold_left (fun acc (w : World.wall) ->
      let denom = Vec.cross direction w.edge in
      if Float.abs denom < 1e-12 then acc              (* parallel *)
      else
        let ao = Vec.sub w.a origin in
        let t = Vec.cross ao w.edge /. denom in
        let s = Vec.cross ao direction /. denom in
        if t > min_distance && s >= 0. && s <= 1.
        then { distance = t; along = s *. w.length; wall = w } :: acc
        else acc)
      [] world.walls
  in
  List.sort (fun h1 h2 -> Float.compare h2.distance h1.distance) hits

let nearest hits =           (* the closest wall, if any *)
  List.fold_left (fun best h -> match best with
    | Some b when b.distance <= h.distance -> best | _ -> Some h) None hits
```

`along` is the world distance from `a` to the hit — we will thread textures
across the wall with it.

### Step 2.3 — The player and the viewport

The camera pose (`lib/player.ml`). Store `dir` and `right` as unit vectors so
turning stays exact; `pitch` (look up/down) comes in Phase 5.

```ocaml
type t = { pos : Vec.t; dir : Vec.t; right : Vec.t; pitch : float }
let create ~pos ~angle =
  let dir = Vec.of_angle angle in { pos; dir; right = Vec.perp dir; pitch = 0. }
let spawn world = create ~pos:world.World.spawn ~angle:0.
let turn p ~radians =
  { p with dir = Vec.rotate p.dir radians; right = Vec.rotate p.right radians }
```

The **viewport** turns the current window size into camera geometry
(`lib/viewport.ml`). This is where the field of view and the projection live.

Two rules decide the geometry. Let `projection` be *the number of screen pixels
one world unit covers at distance 1*.

1. **Pixels stay square.** A one-cell-wide, one-cell-tall wall must cover the
   same pixels horizontally and vertically. Vertically it covers `projection/d`.
   Horizontally the camera plane spans `2·half_width` units at distance 1 across
   `width` pixels, so a unit at distance `d` covers `width/(2·half_width·d)`.
   Equate them: `half_width = width / (2·projection)`.
2. **Anchor the vertical field of view.** Set `projection` from the window
   **height**, so the vertical FOV is fixed and a wider window shows *more world*
   to the sides (Hor+), not a magnified view.

```ocaml
type t = { width : int; height : int; half_width : float; projection : float;
           eye_z : float; horizon : float }

let vertical_half_extent = Float.tan (Config.fov /. 2.) /. Config.reference_aspect

let create ~pitch ~eye_z ~width ~height =
  let width = Int.max 1 width and height = Int.max 1 height in
  let projection = float_of_int height /. 2. /. vertical_half_extent in
  { width; height; projection; eye_z;
    half_width = float_of_int width /. 2. /. projection;
    (* Level, the horizon is the middle row; pitch shears it (Phase 5). *)
    horizon = (float_of_int height /. 2.) +. (pitch *. float_of_int height) }

(* The ray for a column: camera_x runs -1 (left) .. +1 (right). *)
let ray_direction t (player : Player.t) ~column =
  let camera_x = (2. *. float_of_int column /. float_of_int t.width) -. 1. in
  Vec.add player.dir (Vec.scale player.right (t.half_width *. camera_x))
```

**Projection formula.** A point of world height `z` at perpendicular distance
`d`, with the eye at elevation `eye_z`, projects to the screen row

```
screen_y = horizon − projection · (z − eye_z) / d
```

(similar triangles: its height above the eye shrinks with distance and is
measured down from the horizon). Add it:

```ocaml
let project_height t ~z ~distance =
  t.horizon -. (t.projection *. (z -. t.eye_z) /. distance)
let row_factor t ~row = (float_of_int row -. t.horizon) /. t.projection
```

### Step 2.4 — A software framebuffer

We render into a CPU pixel buffer and upload it once per frame as a streaming
texture, then let the GPU scale it to the window. (A software buffer is what
makes per-pixel floor casting and blending possible later.) `lib/framebuffer.ml`:

```ocaml
open Tsdl
open Result_ext

type t = {
  texture : Sdl.texture;
  pixels : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  depth : float array;                 (* per-pixel depth, used in Phase 6 *)
  width : int; height : int;
}

let create sdl ~width ~height =
  let+ texture = Sdl.create_texture sdl Sdl.Pixel.format_argb8888
                   Sdl.Texture.access_streaming ~w:width ~h:height in
  { texture;
    pixels = Bigarray.(Array1.create int8_unsigned c_layout (width * height * 4));
    depth = Array.make (width * height) infinity; width; height }

(* ARGB8888 on a little-endian machine is BGRA in memory. Writing per channel
   avoids boxing an int32 in the hot loop. *)
let set t ~x ~y ~r ~g ~b =
  let i = ((y * t.width) + x) * 4 and p = t.pixels in
  Bigarray.Array1.unsafe_set p i b;
  Bigarray.Array1.unsafe_set p (i+1) g;
  Bigarray.Array1.unsafe_set p (i+2) r;
  Bigarray.Array1.unsafe_set p (i+3) 255

let clear_depth t = Array.fill t.depth 0 (Array.length t.depth) infinity

let present sdl t ~dst =
  (* update_texture's pitch is in BYTES here (4 per pixel). *)
  let* () = Sdl.update_texture t.texture None t.pixels (t.width * 4) in
  Sdl.render_copy sdl t.texture ~dst
```

> **Gotcha.** `update_texture` for a *streaming* texture wants pitch in **bytes**;
> a *static*-texture upload wants pitch in **elements**. Mixing these up squashes
> or stripes the image. Likewise `render_read_pixels` wants bytes.

### Step 2.5 — Draw solid walls

For each column: cast the ray, take the nearest hit, and fill the wall's strip.
The wall stands from `floor_z` (0 for now — flat floor) to `floor_z + height`.
Project foot and top with `project_height` (eye at `Config.eye_height = 0.5`, the
middle of a one-cell wall). Before the real floor casting of Phase 4, just fill
ceiling and floor with flat colours. `lib/renderer.ml` (first cut):

```ocaml
let draw_frame fb world (player : Player.t) =
  let width = fb.Framebuffer.width and height = fb.Framebuffer.height in
  let viewport = Viewport.create ~pitch:0. ~eye_z:Config.eye_height ~width ~height in
  for column = 0 to width - 1 do
    (* placeholder background: top half ceiling, bottom half floor *)
    for y = 0 to height - 1 do
      if y < height / 2 then Framebuffer.set fb ~x:column ~y ~r:40 ~g:42 ~b:54
      else Framebuffer.set fb ~x:column ~y ~r:68 ~g:71 ~b:90
    done;
    let dir = Viewport.ray_direction viewport player ~column in
    match Ray.(nearest (cast world ~origin:player.pos ~direction:dir)) with
    | None -> ()
    | Some hit ->
      let d = hit.Ray.distance in
      let y_foot = Viewport.project_height viewport ~z:0. ~distance:d in
      let y_top  = Viewport.project_height viewport ~z:hit.Ray.wall.World.height ~distance:d in
      let first = Int.max 0 (int_of_float y_top)
      and last  = Int.min (height - 1) (int_of_float y_foot) in
      for y = first to last do Framebuffer.set fb ~x:column ~y ~r:180 ~g:120 ~b:120 done
  done
```

Wire this into the loop: each frame ask SDL the output size, (re)size the
framebuffer to match, `draw_frame`, `present`, `render_present`. (We'll flesh
the loop out in Phase 7; for now a fixed-size framebuffer created once is fine.)

> **Milestone 2.** You are standing in a shaded room. Walls are solid strips,
> tall up close and short far away, with no fish-eye. It already looks 3-D.

### Step 2.6 — Movement with wall sliding

Move along `dir` (forward) and `right` (strafe), resolving each axis
independently so hitting a wall at an angle keeps the free component — you slide
along instead of sticking. Collision treats the player as a small disc of radius
`Config.collision_padding` and, crucially, refuses any step whose **path**
crosses a wall (so a fast step can't tunnel through a thin wall). In `World`:

```ocaml
let distance_to_wall (w : wall) (p : Vec.t) =
  if w.length = 0. then Vec.length (Vec.sub p w.a)
  else
    let s = Vec.dot (Vec.sub p w.a) w.edge /. (w.length *. w.length) in
    let s = Float.max 0. (Float.min 1. s) in
    Vec.length (Vec.sub p (Vec.add w.a (Vec.scale w.edge s)))

let blocked t p =
  Array.exists (fun w -> distance_to_wall w p < Config.collision_padding) t.walls

let segments_cross a1 a2 b1 b2 =       (* same cross-product test as Ray *)
  let d1 = Vec.sub a2 a1 and d2 = Vec.sub b2 b1 in
  let denom = Vec.cross d1 d2 in
  if Float.abs denom < 1e-12 then false
  else let off = Vec.sub b1 a1 in
    let t = Vec.cross off d2 /. denom and u = Vec.cross off d1 /. denom in
    t >= 0. && t <= 1. && u >= 0. && u <= 1.

let can_step t ~from ~dest =
  (not (blocked t dest))
  && not (Array.exists (fun w -> segments_cross from dest w.a w.b) t.walls)
```

In `Player`:

```ocaml
let slide world player (delta : Vec.t) =
  let open Vec in
  let step from moved = if World.can_step world ~from ~dest:moved then moved else from in
  let after_x = step player.pos { player.pos with x = player.pos.x +. delta.x } in
  let after_y = step after_x   { after_x   with y = after_x.y   +. delta.y } in
  { player with pos = after_y }

let walk world player ~forward ~strafe =
  slide world player (Vec.add (Vec.scale player.dir forward)
                              (Vec.scale player.right strafe))
```

Read the keyboard as a *state snapshot* (held keys move continuously) and feed
`walk`/`turn` in the loop. We build the full `Input` module in Phase 5.

> **Milestone 2.6.** You can walk around the room and slide along walls.

---

## Phase 3 — Making walls look real

### Step 3.1 — Colour, shading, fog

`lib/color.ml` — 8-bit RGB with a couple of operations:

```ocaml
type t = { r : int; g : int; b : int }
let rgb r g b = { r; g; b }
let clamp_channel v = Int.max 0 (Int.min 255 v)
let shade c f = let a v = clamp_channel (int_of_float (float_of_int v *. f)) in
  { r = a c.r; g = a c.g; b = a c.b }
let lerp a b t =                          (* used by the sky later *)
  let m x y = clamp_channel (int_of_float ((float_of_int x *. (1.-.t)) +. (float_of_int y *. t))) in
  { r = m a.r b.r; g = m a.g b.g; b = m a.b b.b }
```

`lib/palette.ml` — the look decisions in one place. Two dimmers make a flat
world read as solid:

- **Distance fog**: full colour up close, fading to `min_brightness` at
  `fog_distance`. Cheap, but it gives strong depth.
- **Orientation shading**: dim a wall by how squarely its `normal` faces a fixed
  light, in a band so nothing goes black. This is what makes corners visible.

```ocaml
let wall_color = function 1 -> Color.rgb 200 70 70 | 2 -> Color.rgb 80 190 100
  | 3 -> Color.rgb 80 120 220 | 4 -> Color.rgb 220 200 90 | _ -> Color.rgb 160 160 160

let light = Vec.normalize (Vec.make (-0.4) (-0.9))
let face_shading n = 0.6 +. (0.4 *. Float.abs (Vec.dot n light))
let fog distance = Float.max Config.min_brightness (1. -. (distance /. Config.fog_distance))
```

Tint the wall strip with `Color.shade (wall_color id) (face_shading normal *. fog d)`.

### Step 3.2 — Wall textures

A texture is a small **greyscale** pattern generated in code (no image assets)
— a texel is a *brightness*, not a colour. The colour arrives at draw time by
multiplying the greyscale by the palette colour, so one pattern dresses a wall
of any colour. `lib/texture.ml`:

```ocaml
let size = 64
type t = { texels : int array; alpha : int array; opaque : bool }  (* alpha: Phase 6 *)
let sample t ~u ~v = t.texels.((v * size) + u)

let generate f =                     (* a solid pattern from a brightness fn *)
  { texels = Array.init (size*size) (fun i ->
      Int.min 255 (Int.max 0 (f ~u:(i mod size) ~v:(i / size))));
    alpha = Array.make (size*size) 255; opaque = true }

(* Running-bond brick: 16-texel courses, every other shifted half a brick. *)
let brick = generate (fun ~u ~v ->
  let course = v / 16 in
  let u = (u + (if course land 1 = 0 then 0 else 16)) mod size in
  if v mod 16 < 2 || u mod 32 < 2 then 130 else 225)
(* ...panel, stone, checker, plain similarly... *)

(* Which texel column a hit at [offset] (0..1 across the face) falls in. *)
let column_of_offset off =
  Int.min (size-1) (Int.max 0 (int_of_float (off *. float_of_int size)))
```

**Texture mapping a strip.** Horizontally, `u` comes from the hit position along
the wall: `column_of_offset (frac hit.along)` — this tiles the pattern every
world unit. Vertically, for each pixel of the strip recover the world height it
looks at and take the fractional cell:

```ocaml
(* inside the per-pixel wall loop, distance d, wall foot at floor_z: *)
let z = viewport.eye_z -. ((float_of_int y -. viewport.horizon) *. d /. viewport.projection) in
let tile = let h = z -. floor_z in h -. Float.floor h in       (* 0 at cell bottom .. 1 at top *)
let v = int_of_float ((1. -. tile) *. float_of_int (Texture.size - 1)) in
let texel = Texture.sample pattern ~u ~v in
Framebuffer.set fb ~x:column ~y
  ~r:(tint.Color.r * texel / 255) ~g:(tint.Color.g * texel / 255) ~b:(tint.Color.b * texel / 255)
```

Tiling per cell means a tall wall repeats its pattern instead of stretching it.

> **Milestone 3.2.** Textured, coloured, fogged walls.

### Step 3.3 — Variable wall heights

Each wall already has a `height`. Because the eye sits at `eye_z = 0.5`, a wall
shorter than one cell drops below its neighbours and a taller one rises above
them. For that to *read* correctly, a ray must **not stop at the first wall** — a
low wall must not hide a tall one behind it. We already collect every hit,
farthest-first; now just paint them in that order (the **painter's algorithm**):

```ocaml
let hits = Ray.cast world ~origin:player.pos ~direction:dir in
List.iter (fun hit -> draw_wall fb viewport world player ~column ~dir hit) hits
```

A near wall paints over a far one; a tall wall still shows above a short one.

> **Milestone 3.3.** Short walls you can see over; tall pillars behind them.

---

## Phase 4 — Floor, ceiling, sky

Now replace the flat ceiling/floor fills with real, textured, possibly
**inclined** surfaces. This is the part that makes the renderer software.

### Step 4.1 — Inclined planes

Model the floor (and ceiling) as a plane `z = a·x + b·y + c`. `lib/plane.ml`:

```ocaml
type t = { a : float; b : float; c : float }
let make ~a ~b ~c = { a; b; c }
let horizontal z = { a = 0.; b = 0.; c = z }
let elevation t (p : Vec.t) = (t.a *. p.x) +. (t.b *. p.y) +. t.c
let gradient t (dir : Vec.t) = (t.a *. dir.x) +. (t.b *. dir.y)   (* rise per unit along dir *)
```

**The casting equation.** For a pixel below the horizon in a column of direction
`dir`, at what perpendicular distance `d` does the line of sight meet the plane?
The pixel sees, at distance `d`, the world point `eye_pos + d·dir` at height
`eye_z − row_factor·d`, where `row_factor = (row − horizon)/projection`. Set that
equal to the plane's own height there, `base + d·gradient` (with `base` the
plane under the eye), and solve:

```
eye_z − row_factor·d = base + d·gradient
eye_z − base         = d·(row_factor + gradient)
d                    = (eye_z − base) / (row_factor + gradient)
```

One formula serves both surfaces: the real one is whichever gives `d > 0`.

```ocaml
let view_distance t ~eye_z ~eye_pos ~dir ~row_factor =
  let denom = row_factor +. gradient t dir in
  if Float.abs denom < 1e-9 then None
  else let d = (eye_z -. elevation t eye_pos) /. denom in
       if d > 0. then Some d else None
```

Add `floor` and (optional) `ceiling : Plane.t option` fields to `World.t`. For a
gentle wedge use e.g. `Plane.make ~a:0.06 ~b:0.03 ~c:0.`.

### Step 4.2 — Casting the floor and ceiling per pixel

Replace the placeholder background. For each column, precompute the two plane
gradients; for each row, cast the relevant plane (one division), get the world
point, colour it, fade by fog. `eye_z = floor_z(player) + eye_height`.

```ocaml
let draw_planes fb viewport world (player : Player.t) ~column ~dir =
  let open Viewport in
  let px = player.pos.x and py = player.pos.y and dx = dir.x and dy = dir.y in
  let floor_base = Plane.elevation world.floor player.pos in
  let gf = Plane.gradient world.floor dir in
  for y = 0 to fb.Framebuffer.height - 1 do
    let r = row_factor viewport ~row:y in
    let dn = r +. gf in
    if dn > 1e-9 then begin                        (* below the horizon: floor *)
      let d = (viewport.eye_z -. floor_base) /. dn in
      let wx = px +. (d *. dx) and wy = py +. (d *. dy) in
      let base = Palette.floor_color and f = Palette.fog d in
      Framebuffer.set fb ~x:column ~y
        ~r:(shade base.Color.r f) ~g:(shade base.g f) ~b:(shade base.b f)
    end else (* above the horizon: ceiling (or sky, Step 4.4) *) ...
  done
```

The ceiling is symmetric with its own plane and `denom < 0`. When both are in
view (only near the tilted horizon) pick the nearer positive `d`.

Because walls now stand on the *sloped* floor, compute each wall's `floor_z` and
`top_z` at its **hit point** and cap `top_z` to the ceiling (if any) so a wall
never pokes through:

```ocaml
let hit_point = Vec.make (px +. d*.dx) (py +. d*.dy) in
let floor_z = Plane.elevation world.floor hit_point in
let top_z = match world.ceiling with
  | Some c -> Float.min (floor_z +. w.height) (Plane.elevation c hit_point)
  | None -> floor_z +. w.height in
```

### Step 4.3 — Texturing the planes

Sample a greyscale `Texture` at the **world point** the pixel casts to, tiling
every world unit, tinted by a base colour and fogged — exactly the walls' recipe.
Anchoring the pattern in world space is what makes the tilt read: its features
foreshorten and their rows tilt with the surface.

```ocaml
let plane_texel pattern ~x ~y =
  let frac v = v -. Float.floor v in
  Texture.sample pattern ~u:(Texture.column_of_offset (frac x))
                         ~v:(Texture.column_of_offset (frac y))
```

Use `Texture.checker` for the floor and a bevelled `panel` for the ceiling, say.

### Step 4.4 — The sky

Make the ceiling optional (`None` = open sky). The sky is an **infinitely far
backdrop**: its colour depends only on the *direction looked in* — the column's
azimuth and how high up the pixel sits — never on where you stand, so it doesn't
slide as you walk, only wheels as you turn. `lib/sky.ml`:

```ocaml
let horizon_color = Color.rgb 176 196 222 and zenith_color = Color.rgb 40 62 126
let sun_color = Color.rgb 255 246 216
let sun_azimuth = -0.9 and sun_height = 0.5 and sun_radius = 0.55

let color ~azimuth ~up =
  let t = Float.max 0. (Float.min 1. (up *. 2.2)) in
  let sky = Color.lerp horizon_color zenith_color t in
  let daz = (* wrap azimuth - sun_azimuth to [-π,π] *) ... in
  let d = Float.hypot daz (up -. sun_height) in
  let glow = Float.max 0. (1. -. (d /. sun_radius)) in
  Color.lerp sky sun_color (glow *. glow)
```

In the background pass, when there is no ceiling, an above-horizon pixel gets
`Sky.color ~azimuth:(atan2 dir.y dir.x) ~up:(-. row_factor)`.

> **Milestone 4.** A textured, tilted floor; overhead either a textured ceiling
> or a gradient sky with a sun. Build a bigger level here — an arena of pillars,
> a room with a doorway, low walls you see over — with `World.path` and
> `World.regular_polygon` helpers (a polyline / closed polygon of walls).

---

## Phase 5 — Camera & window

### Step 5.1 — Mouse look and looking up/down

**Yaw** already rotates `dir`/`right`. **Pitch** is faked by *shearing* the whole
image vertically — a raycaster has no true vertical rotation (that would tilt the
walls). We already put pitch into the `horizon`: `horizon = height/2 +
pitch·height`. Everything (the plane casts, every wall) is measured from
`horizon`, so moving it looks up or down for free. Clamp pitch to
`Config.max_pitch`.

```ocaml
(* Player *)
let pitch_by p ~delta =
  let l = Config.max_pitch in
  { p with pitch = Float.max (-.l) (Float.min l (p.pitch +. delta)) }
```

Input (`lib/input.ml`) reads held keys as a snapshot and blends in **relative
mouse** motion. Each field comes out as a finished per-frame delta:

```ocaml
let mouse_delta () = let _, (dx, dy) = Sdl.get_relative_mouse_state () in
  (float_of_int dx, float_of_int dy)

type motion = { forward : float; strafe : float; turn : float; pitch : float }

let motion () =
  let keys = Sdl.get_keyboard_state () in
  let down sc = Bigarray.Array1.get keys sc = 1 in
  let axis ~pos ~neg = (if List.exists down pos then 1. else 0.)
                    -. (if List.exists down neg then 1. else 0.) in
  let mdx, mdy = mouse_delta () in
  { forward = axis ~pos:Sdl.Scancode.[w] ~neg:Sdl.Scancode.[s] *. Config.move_speed;
    strafe  = axis ~pos:Sdl.Scancode.[d] ~neg:Sdl.Scancode.[a] *. Config.move_speed;
    turn = (axis ~pos:Sdl.Scancode.[right] ~neg:Sdl.Scancode.[left] *. Config.rot_speed)
           +. (mdx *. Config.look_sensitivity);
    (* mouse up is negative dy but should look up, hence the minus *)
    pitch = (axis ~pos:Sdl.Scancode.[up] ~neg:Sdl.Scancode.[down] *. Config.pitch_speed)
            -. (mdy *. Config.pitch_sensitivity) }
```

Enable relative mouse mode once at startup (`Sdl.set_relative_mouse_mode true`)
so the cursor is hidden, pinned, and never hits a screen edge. The engine `step`
applies the deltas in order — turn, pitch, then walk:

```ocaml
let step world player (m : Input.motion) =
  player |> Player.turn ~radians:m.turn |> Player.pitch_by ~delta:m.pitch
         |> Player.walk world ~forward:m.forward ~strafe:m.strafe
```

### Step 5.2 — Resizing and fullscreen

Cache **no** size. Each frame, ask SDL for the current drawable size and build a
fresh `Viewport` from it — that alone handles resizing, HiDPI and the fullscreen
transition, with no resize events to handle. Because per-pixel casting costs one
division per pixel, render into an **internal buffer capped** to
`Config.max_render_height` and let the GPU scale it up, keeping the frame rate
steady at any window size:

```ocaml
let internal_size ~width ~height =
  let scale = Int.max 1 ((height + Config.max_render_height - 1) / Config.max_render_height) in
  (Int.max 1 (width / scale), Int.max 1 (height / scale))
```

Fullscreen is a toggle read from the event queue (F11); use
`fullscreen_desktop`, which stretches over the desktop instantly and needs
nothing restored. The renderer notices the new size next frame on its own.

> **Milestone 5.** Mouse look with pitch; the window resizes and goes fullscreen
> smoothly.

---

## Phase 6 — Props: transparency, decals, sprites

These three sit *on top of* the opaque world and need careful ordering. The key
enabling idea is a **depth buffer**.

### Step 6.1 — A per-pixel depth buffer

To know whether a sprite or a see-through wall is hidden by an opaque wall, we
need the distance of the nearest opaque wall **per pixel** — not per column,
because a *short* wall only covers the lower part of its column; anything behind
it should still show its top.

We already added `depth : float array` to the framebuffer and `clear_depth`. When
drawing an **opaque** wall, record its distance at each pixel it covers:

```ocaml
(* in the wall loop, occlude = true for opaque walls *)
let index = (y * width) + column in
Framebuffer.set fb ~x:column ~y ~r:cr ~g:cg ~b:cb;
if occlude then depth.(index) <- d
```

Opaque walls are painted far-to-near, so a nearer one overwrites both colour and
depth; the final `depth.(index)` is the nearest opaque wall covering that pixel
(or `infinity` where only background shows).

### Step 6.2 — Transparent walls

Give `Texture` a per-texel **alpha** and an `opaque` flag; add
`generate_masked f` where `f` returns *(brightness, alpha)*, and patterns like a
grille (solid bars, clear gaps) and glass (translucent panes):

```ocaml
let bars = generate_masked (fun ~u ~v ->
  if u mod 16 < 5 || v mod 16 < 5 then (80, 255) else (0, 0))     (* holes *)
let glass = generate_masked (fun ~u ~v ->
  let mullion = u<2||u>=size-2||v<2||v>=size-2||abs(u-size/2)<2||abs(v-size/2)<2 in
  if mullion then (150, 255) else (235, 80))                       (* translucent panes *)
```

Framebuffer gains an alpha blend (`src·a + dst·(255−a)` per channel):

```ocaml
let blend t ~x ~y ~r ~g ~b ~a =
  let i = ((y*t.width)+x)*4 and p = t.pixels in
  let inv = 255 - a in
  let mix dst src = ((src*a) + (dst*inv)) / 255 in
  Bigarray.Array1.(unsafe_set p i (mix (unsafe_get p i) b));
  Bigarray.Array1.(unsafe_set p (i+1) (mix (unsafe_get p (i+1)) g));
  Bigarray.Array1.(unsafe_set p (i+2) (mix (unsafe_get p (i+2)) r))
```

In the wall loop, a texel with `a = 255` is written (and records depth), `a = 0`
is skipped (a hole), otherwise it is blended. See-through walls **don't** occlude
(don't write depth); they are collected and drawn *after* the opaque pass so
whatever is behind them is already on screen to show through.

### Step 6.3 — Decals (pictures on walls)

Decals are colourful, so they use a new **RGBA `Image`** type (full colour +
alpha, unlike greyscale `Texture`), generated in code — a framed painting, a
poster. `lib/image.ml`:

```ocaml
type t = { size : int; pixels : Color.t array; alpha : int array }
let make size f =
  let n = size*size in
  let pixels = Array.make n (Color.rgb 0 0 0) and alpha = Array.make n 0 in
  for v = 0 to size-1 do for u = 0 to size-1 do
    let c, a = f ~u ~v in let i = v*size+u in pixels.(i) <- c; alpha.(i) <- a
  done done; { size; pixels; alpha }
let index t ~u ~v = (v * t.size) + u
```

A wall carries a `decals` list — each decal a picture at a position `along` the
wall and height `z`, with half-extents. Since `hit.along` is constant down a
column, decide once which decals this column falls in (and their texture column),
then per pixel test the height range and **blend** the image texel over the wall,
in the same light as the wall.

### Step 6.4 — Sprites (billboards)

A sprite is an `Image` standing in the world that always faces the player. Add
`sprites : sprite array` to the world (`{ pos; size; image }`). To draw one:

1. Camera-space coordinates: `depth_s = (pos − player.pos) · dir` (perpendicular
   distance) and `lateral = (pos − player.pos) · right`. Skip if `depth_s ≤ 0`
   (behind you).
2. Screen column of the centre — invert the camera-plane mapping:
   `camera_x = lateral / (depth_s · half_width)`, then
   `center = (camera_x + 1) · width / 2`.
3. Vertical placement: project the foot at the sprite's `floor_z` and the top at
   `floor_z + size` with `project_height`. The image is square, so its on-screen
   width equals its height; the horizontal extent is `center ± (y_base − y_top)/2`.
4. For each pixel, sample the image; draw it (blended) **only where `depth_s <
   depth.(pixel)`** — nearer than the opaque wall there. Per-pixel is what lets a
   short wall hide only the sprite's legs, not its head.

```ocaml
let camera_x = lateral /. (depth_s *. viewport.half_width) in
let center = (camera_x +. 1.) *. float_of_int width /. 2. in
let y_base = project_height viewport ~z:floor_z ~distance:depth_s in
let y_top  = project_height viewport ~z:(floor_z +. s.size) ~distance:depth_s in
(* loop cols in [center-half, center+half], rows in [y_top, y_base]; blend where
   depth_s < depth.((y*width)+col) and the image alpha > 0, faded by fog depth_s *)
```

### Step 6.5 — Compositing order

Sprites and see-through walls are *both* translucent, so they can't each own a
pass: a sprite in front of a window must cover it, one behind must show through
it. Put them in **one list, sorted by depth, drawn farthest-first**, each still
tested per pixel against the opaque depth buffer:

```ocaml
type fragment = Sprite of World.sprite | See_through of int * Vec.t * Ray.hit

(* build (depth, fragment) list from sprites and deferred see-through hits,
   sort by depth descending, then: *)
List.iter (fun (d, f) -> match f with
  | Sprite s               -> draw_sprite fb viewport world player s ~depth_s:d
  | See_through (col,dir,h) -> draw_wall fb viewport world player ~column:col ~dir ~occlude:false h)
  ordered
```

So a whole frame is: **backgrounds + opaque walls per column** (recording
per-pixel depth), then **one combined translucent pass** of sprites and windows.

> **Milestone 6.** A wall hung with a painting and a poster; a grille and a
> window you see the room through; barrels and figures standing about, correctly
> occluded — a short wall hides only their lower halves.

---

## Phase 7 — Assembly

### Step 7.1 — The engine loop and resource safety

Nest `with_resource` so SDL init, the window, the renderer, relative mouse mode
and the framebuffer are all released in reverse order even on error. The loop
carries the immutable `player` and the `fullscreen` flag between frames:

```ocaml
let rec loop ctx ~player ~fullscreen =
  let request = Input.poll ctx.event in                 (* drains the event queue *)
  if request.Input.quit then Ok ()
  else
    let* fullscreen =
      if request.toggle_fullscreen then set_fullscreen ctx.window (not fullscreen)
      else Ok fullscreen in
    let player = step ctx.world player (Input.motion ()) in
    let* () = Renderer.render ctx.renderer ctx.framebuffer ctx.world player in
    Sdl.delay Config.frame_delay;
    loop ctx ~player ~fullscreen
```

`Renderer.render` reads the output size, resizes the framebuffer to the internal
size if needed, `clear_depth`, `draw_frame`, `present`, `render_present`.

> **Milestone 7 — the whole engine.** Everything runs together.

### Step 7.2 — Tests and docs

Keep the pure logic testable and test it with **Alcotest** — one executable per
module, sharing fixtures (a hand-checkable 4×4 room). Good things to pin:

- `Vec`: cross is zero for parallel vectors; `normalize` is unit length.
- `Ray`: from the room centre every wall is distance 2; a diagonal is `2` in
  *units of direction* (the no-fish-eye property); hits come back farthest-first.
- `Plane`: on a flat floor one below the eye, `view_distance` reduces to
  `eye_z / row_factor`; the floor is only in view below the horizon.
- `Viewport`: a point at eye height projects onto the horizon at every distance;
  `half_width = tan(fov/2)` at the reference shape; widening grows `half_width`
  but not `projection` (Hor+).
- `Sky`: the zenith is bluer and darker than the horizon; the sun is a bright
  spot; azimuth wraps at ±π.

`Input`, `Framebuffer` and `Renderer` need a live SDL surface, so test their
*pure* pieces (in `Plane`, `Viewport`, `Palette`, `Sky`, `Image`) instead.

Document modules with `(** ... *)` and build HTML with `odoc` (`dune build @doc`;
the library needs a `public_name`). Put derivations in the docstrings.

**How to verify rendering without a screen.** SDL can render offscreen: create a
hidden window + renderer, fill a `Framebuffer` with `draw_frame`, wrap its bytes
in a surface (`Sdl.create_rgb_surface_from`, masks for ARGB8888), and
`save_bmp`. Eyeballing four viewpoints catches sign errors no unit test will.

---

## Appendix

### Module dependency map

Each depends only on those above it (dune figures the order out; this is the
mental model):

```
Config
Result_ext
Vec
Plane        (inclined floor/ceiling + the casting equation)
World        (wall segments + decals, floor, optional ceiling, sprites, collision)
Ray          (ray-vs-segment; every wall crossed, farthest first)
Player       (pose pos/dir/right/pitch; movement + sliding)
Viewport     (window → projection, eye height, pitch shear, resize rules)
Texture      (greyscale wall/plane patterns + per-texel alpha)
Color/Palette(colour, shading, fog, the tables)
Sky          (directional gradient + sun)
Image        (RGBA pictures for decals and sprites)
Input        (keyboard + relative mouse → intent)
Framebuffer  (CPU pixels + alpha blend + per-pixel depth + streaming texture)
Renderer     (floor/ceiling/sky, opaque walls + decals, then sprites + windows by depth)
Engine       (window lifetime, fullscreen, the loop)
```

### Constants worth exposing (`Config`)

`fov`, `reference_aspect`, `eye_height` (0.5), `move_speed` (~0.06),
`rot_speed` (~0.035), `collision_padding` (~0.15), `fog_distance` (~12),
`min_brightness` (~0.25), `look_sensitivity`/`pitch_sensitivity` (~0.0025),
`pitch_speed`, `max_pitch` (~0.75), `max_render_height` (~480), `frame_delay`
(16 ms).

### Hard-won gotchas

- **`update_texture` pitch is bytes for a streaming texture, elements for a
  static one.** `render_read_pixels` is bytes. A wrong pitch squashes/stripes.
- **Never read the back buffer after `render_present`** — it's undefined. Render
  offscreen for screenshots.
- **Opaque surfaces:** set the alpha byte to 255 or your captures come out blank.
- **`World.cell` uses `floor`, not `int_of_float`** if you ever add a grid —
  truncation folds cells −1 and 0 together. (We use segments and sidestep this.)
- **Collision must test the *path*, not just the endpoint**, or a fast step
  tunnels through a thin wall.
- **`horizon` is a float**; only convert to an int pixel at the last moment.
- **Sprites vs transparent walls need one depth-sorted pass**, and occlusion must
  be **per pixel**, or short walls cull whole sprites behind them.

### Exercises to go further

- **Wall tops.** Floor-cast the horizontal top faces of short walls so you see
  them when looking down.
- **Textured sky** by sampling a panoramic image by azimuth/elevation.
- **Animated sprites** by swapping the `Image` per frame; **directional sprites**
  by choosing the image from the angle between the sprite's facing and the view.
- **Coloured lighting** by tinting fog per region, or point lights that dim with
  distance from a source.
- **A minimap** drawing the wall segments and the player from above.
- **Per-pixel depth for translucent interpenetration** (splitting a window where
  a sprite passes through it) if you want fully correct transparency.

---

Built in the same spirit as the engine it describes: small modules, derivations
written down, and a picture on screen at the end of every phase. Happy casting.
