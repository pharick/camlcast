(** The showcase level's wall patterns: masonry, joinery, tile and glazing.

    Each is a pure function of [u] and [v] — which is what makes every one of
    them testable — handed to {!Camlcast.Texture.generate}. That is one of the
    two ways in: the {!Loading} demo reads its patterns from files with
    {!Camlcast.Texture.load} instead. Two of them, {!bars} and {!glass}, go
    through {!Camlcast.Texture.generate_masked} and so carry an alpha; a
    {!Camlcast.Material} wearing either is see-through, which is the whole
    mechanism behind the renderer's translucent pass.

    {1 Colour is an argument}

    Every one of these takes its colours {e before} [u] and [v], so a partial
    application is a pattern and the same function serves any number of them.
    {!Surfaces} is where that is collected: {!checker} is applied twice, once
    for a yellow tiled floor and once for a grey-brown one, and the two are one
    function apart.

    Two of them take {e two} colours, because two is what the thing has:
    {!brick}'s mortar is not tinted brick, and the lead of a leaded window is
    not tinted glass. Those are the patterns that a texture carrying only a
    brightness could not have drawn, however it was dressed afterwards — the
    mortar would have been pale red between red bricks. Everywhere else a single
    colour through {!Camlcast.Color.level} is the honest answer, because a bevel
    and a plank shadow really are the same material with less light on them. *)

open Camlcast

let hash = Texture.hash

(** Running bond masonry: courses 16 texels high, every other one shifted by
    half a brick so the vertical joints never line up between courses. The
    mortar is its own colour, and a paler, flatter one — it is the thing you can
    see the wall was built out of. *)
let brick ~color ~mortar ~u ~v =
  let course = v / 16 in
  let u = (u + if course land 1 = 0 then 0 else 16) mod Texture.default_size in
  let in_mortar = v mod 16 < 2 || u mod 32 < 2 in
  if in_mortar then Color.level mortar 210
  else Color.level color (225 + (hash (u / 32) course mod 30))

(** Bevelled panels: a lit edge along the top and left, a shadow along the
    bottom and right. The eye reads the pair as depth. One colour, because a
    bevel is the same wood catching more or less light. *)
let panel ~color ~u ~v =
  let x = u mod 32 and y = v mod 32 in
  Color.level color
    (if x < 2 || y < 2 then 255 else if x >= 30 || y >= 30 then 140 else 215)

(** Vertical planks crossed by a sturdy horizontal rail. *)
let door ~color ~u ~v =
  Color.level color
    (if v >= 28 && v < 36 then 150
     else if u mod 16 < 2 then 115
     else 205 + (hash (u / 16) 19 mod 30))

(** Irregular blocks: each course is shifted by a hashed amount, so unlike
    {!brick} the courses do not repeat in step with each other. *)
let stone ~color ~u ~v =
  let course = v / 16 in
  let u =
    (u + (hash course 7 mod Texture.default_size)) mod Texture.default_size
  in
  let in_joint = v mod 16 < 2 || u mod 21 < 2 in
  Color.level color
    (if in_joint then 120 else 200 + (hash (u / 21) course mod 45))

(** A plain check, for surfaces that should read as tiled rather than built. *)
let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

(** No pattern at all, for a surface that should show its colour flat. *)
let plain ~color ~u:_ ~v:_ = Color.level color 235

(** A metal grille: a lattice of solid bars with clear holes between them, so
    the room behind shows through the gaps. *)
let bars ~color ~u ~v =
  if u mod 16 < 5 || v mod 16 < 5 then
    (Color.level color (70 + (hash (u / 16) (v / 16) mod 30)), 255)
  else (Color.rgb 0 0 0, 0)

(** A leaded window: solid mullions around and across, translucent panes
    between, so the room behind is dimly visible through the glass. The lead is
    a dark grey and the glass a pale blue-green, which is two materials and so
    two colours. *)
let glass ~lead ~pane ~u ~v =
  let size = Texture.default_size in
  let mullion =
    u < 2
    || u >= size - 2
    || v < 2
    || v >= size - 2
    || abs (u - (size / 2)) < 2
    || abs (v - (size / 2)) < 2
  in
  if mullion then (Color.level lead 150, 255) else (Color.level pane 235, 80)
