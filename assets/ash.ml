(** The ashen surfaces of the House.

    The hallway of the book is described over and over by what it lacks. It is
    ash grey. It is cold. It has no windows, no fixtures, no marks, no seams. It
    is not stone and it is not plaster, and no one can say what it is. Light
    does not so much fall on it as get absorbed by it.

    So these patterns are the opposite of masonry. There are no courses, no
    joints and no grout lines, because a course would tell you how the wall was
    built and there is no answer to that question here. What is left is a very
    narrow band of value noise: enough grain that a wall reads as a surface
    rather than as a flat fill, and not one feature large enough to be
    recognised again. A wall you can find your way by is not this wall.

    Everything is built from {!Raycaster.Texture.noise}, whose lattice wraps at
    the texture's size. That matters more here than anywhere: with no pattern to
    hide behind, a seam every world unit would be the only thing in the whole
    House with a shape, and the eye would go straight to it.

    {1 What repeats anyway}

    A wall's pattern tiles once per world unit horizontally, so a three-unit
    wall shows the same 64 texels three times. Wrapping removes the {e seam}
    between the copies but not the repeat itself. Keeping the largest octave at
    a 32-texel lattice — half the whole texture — means the biggest feature is
    also the one that varies least across a tile, so what repeats is close to
    nothing. It is still there if you look for it. *)

open Raycaster

(** Three octaves, from a fine grain up to a slow mottling half the texture
    across, summed about a mid grey and kept inside a band of about forty
    levels out of two hundred and fifty-six. Sixteen would read as flat and
    eighty would read as concrete; this is the width at which the eye keeps
    deciding there is something there and keeps being wrong. *)
let mottle ~seed ~base ~fine ~broad ~sweep ~u ~v =
  let octave s cell weight =
    (Texture.noise ~seed:(seed + s) ~cell ~u ~v - 128) * weight / 128
  in
  base + octave 0 2 fine + octave 1 8 broad + octave 2 32 sweep

(** The walls. Nothing but noise — no direction, no scale, nothing to measure
    the corridor against. *)
let plaster =
  Texture.generate (fun ~u ~v -> mottle ~seed:11 ~base:226 ~fine:6 ~broad:10 ~sweep:14 ~u ~v)

(** The same surface with the faintest vertical striation in it. A long
    corridor wall of pure noise loses all its perspective once the fog takes
    hold — it stops receding and becomes a grey fill — and a suggestion of
    verticals gives the eye just enough to follow the wall away from itself.

    The striation is drawn from a very wide noise lattice rather than a fixed
    period, so it never resolves into stripes you could count. *)
let scored =
  Texture.generate (fun ~u ~v ->
      let ground = mottle ~seed:23 ~base:224 ~fine:5 ~broad:9 ~sweep:12 ~u ~v in
      (* Depends on [u] alone, so it runs the full height of the wall. *)
      let stripe = (Texture.noise ~seed:29 ~cell:4 ~u ~v:0 - 128) * 7 / 128 in
      ground + stripe)

(** The floor. Coarser than the walls, because a plane's pattern is sampled in
    world space and foreshortens as it recedes: features that read clearly on a
    wall a few cells away vanish entirely underfoot. Darker, too — nothing here
    reflects anything. *)
let ground =
  Texture.generate (fun ~u ~v -> mottle ~seed:37 ~base:212 ~fine:4 ~broad:16 ~sweep:22 ~u ~v)

(** The ceiling. The same again, flatter still. It is almost always in shadow
    and almost never looked at, and both of those are the point. *)
let soffit =
  Texture.generate (fun ~u ~v -> mottle ~seed:41 ~base:202 ~fine:3 ~broad:8 ~sweep:16 ~u ~v)

(** A door. The one thing in the House that was made by somebody: a flat panel
    inset a little way from its frame, and a rail across it. It is the only
    pattern here with a straight line in it, which is exactly why it reads as a
    door and everything else reads as wall. *)
let leaf =
  Texture.generate (fun ~u ~v ->
      let grain = mottle ~seed:53 ~base:214 ~fine:5 ~broad:7 ~sweep:9 ~u ~v in
      let border = u < 6 || u > 57 || v < 4 || v > 59 in
      let rail = v >= 30 && v < 34 in
      if border then grain - 26 else if rail then grain - 18 else grain)
