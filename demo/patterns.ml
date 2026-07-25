(** The showcase level's wall patterns: masonry, joinery, tile and glazing.

    Each is a pure function of [u] and [v] passed to {!Raycaster.Texture.generate}, so
    the project keeps no binary assets and every pattern is testable. Two of
    them — {!bars} and {!glass} — are built with
    {!Raycaster.Texture.generate_masked} and so carry an alpha; a
    {!Raycaster.Material} wearing either is see-through, which is the whole
    mechanism behind the renderer's translucent pass. *)

open Raycaster

let hash = Texture.hash

(** Running bond masonry: courses 16 texels high, every other one shifted by
    half a brick so the vertical joints never line up between courses. *)
let brick =
  Texture.generate (fun ~u ~v ->
      let course = v / 16 in
      let u = (u + if course land 1 = 0 then 0 else 16) mod Texture.size in
      let in_mortar = v mod 16 < 2 || u mod 32 < 2 in
      if in_mortar then 130 else 225 + (hash (u / 32) course mod 30))

(** Bevelled panels: a lit edge along the top and left, a shadow along the
    bottom and right. The eye reads the pair as depth. *)
let panel =
  Texture.generate (fun ~u ~v ->
      let x = u mod 32 and y = v mod 32 in
      if x < 2 || y < 2 then 255 else if x >= 30 || y >= 30 then 140 else 215)

(** Vertical planks crossed by a sturdy horizontal rail. *)
let door =
  Texture.generate (fun ~u ~v ->
      if v >= 28 && v < 36 then 150
      else if u mod 16 < 2 then 115
      else 205 + (hash (u / 16) 19 mod 30))

(** Irregular blocks: each course is shifted by a hashed amount, so unlike
    {!brick} the courses do not repeat in step with each other. *)
let stone =
  Texture.generate (fun ~u ~v ->
      let course = v / 16 in
      let u = (u + (hash course 7 mod Texture.size)) mod Texture.size in
      let in_joint = v mod 16 < 2 || u mod 21 < 2 in
      if in_joint then 120 else 200 + (hash (u / 21) course mod 45))

(** A plain check, for surfaces that should read as tiled rather than built. *)
let checker =
  Texture.generate (fun ~u ~v ->
      if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

(** No pattern at all, for a surface that should show its colour flat. *)
let plain = Texture.generate (fun ~u:_ ~v:_ -> 235)

(** A metal grille: a lattice of solid bars with clear holes between them, so
    the room behind shows through the gaps. *)
let bars =
  Texture.generate_masked (fun ~u ~v ->
      if u mod 16 < 5 || v mod 16 < 5 then
        (70 + (hash (u / 16) (v / 16) mod 30), 255)
      else (0, 0))

(** A leaded window: solid mullions around and across, translucent panes
    between, so the room behind is dimly visible through the glass. *)
let glass =
  Texture.generate_masked (fun ~u ~v ->
      let size = Texture.size in
      let mullion =
        u < 2
        || u >= size - 2
        || v < 2
        || v >= size - 2
        || abs (u - (size / 2)) < 2
        || abs (v - (size / 2)) < 2
      in
      if mullion then (150, 255) else (235, 80))
