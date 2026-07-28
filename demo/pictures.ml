(** The showcase level's pictures: two decals to hang on walls, two sprites to
    stand in the world, and a strip of frames for one that drifts.

    Unlike a {!Camlcast.Texture}, which is square and tiles a world cell because
    it is part of a surface, an {!Camlcast.Image} is drawn once at whatever
    shape it was authored in. The decals are opaque within their frame; the
    sprites are cut out against {!Camlcast.Image.clear} so only the object
    itself is drawn. *)

open Camlcast

(** {1 Decals} *)

(** A small framed landscape: a wooden border around a sky, a sun and grass. *)
let painting =
  Image.make 32 (fun ~u ~v ->
      if u < 2 || u > 29 || v < 2 || v > 29 then (Color.rgb 38 26 14, 255)
      else if u < 4 || u > 27 || v < 4 || v > 27 then (Color.rgb 150 105 55, 255)
      else if Image.disc ~cx:22. ~cy:11. ~r:3.5 u v then
        (Color.rgb 245 225 120, 255)
      else if v < 18 then (Color.rgb 120 165 210, 255)
      else (Color.rgb 85 140 70, 255))

(** A bold poster: a yellow ring on a red field, in a dark border. *)
let poster =
  Image.make 32 (fun ~u ~v ->
      if u < 2 || u > 29 || v < 2 || v > 29 then (Color.rgb 22 22 26, 255)
      else
        let du = float_of_int u -. 16. and dv = float_of_int v -. 16. in
        let d = Float.hypot du dv in
        if d > 8. && d < 12. then (Color.rgb 232 200 60, 255)
        else (Color.rgb 155 42 42, 255))

(** {1 Sprites} *)

(** A wooden barrel: a shaded cylinder with darker hoops, clear to either side.
*)
let barrel =
  Image.make 32 (fun ~u ~v ->
      if u < 7 || u > 24 || v < 3 || v > 30 then Image.clear
      else
        let edge = (float_of_int u -. 16.) /. 9. in
        let round = 1. -. (0.55 *. edge *. edge) in
        let hoop = v mod 9 < 2 || v < 5 || v > 28 in
        let base = if hoop then 70. else 135. in
        let r = int_of_float (base *. round) in
        (Color.rgb r (r * 3 / 5) (r / 3), 255))

(** How many frames {!motes} has, and how many specks are in each. *)
let mote_frames = 12

let mote_count = 40
let mote_width = 144
let mote_height = 48

(** One frame of a drifting cloud of dust: {!mote_count} specks of a pixel or
    two each, scattered across a picture three times as wide as it is tall and
    swinging up and down out of step with one another.

    The only picture here that is not square, and deliberately. A sprite is as
    wide as its own image says — {!Camlcast.Room.sprite_half_width} — so this
    one is drawn as a wide, thin drift rather than stretched across a box.

    Where each speck sits comes from its index and nothing else, so the same
    cloud comes back every run: there is no RNG to seed and no state to carry,
    and the whole picture is a function of [frame].

    The five steps below are all irrational and none is a rational multiple of
    another, which is the part that has to be got right. Take two that add to
    one — the golden ratio's 0.618 and 0.382, say — and the fractional parts run
    exactly opposite, so every speck lands on the same diagonal and the cloud
    comes out as a set of ruled lines. These are the two-dimensional golden
    ratio for the position and three unrelated surds for the rest. *)
let mote ~frame =
  let turn = float_of_int frame /. float_of_int mote_frames *. 2. *. Float.pi in
  let fraction step k = Float.rem (float_of_int k *. step) 1. in
  (* Worked out once per frame and not once per pixel: [Image.make] runs its
     function three thousand times over, and this list is the same every time. *)
  let specks =
    List.init mote_count (fun k ->
        let swing = 2.2 +. (6. *. fraction 0.2360679775 k)
        and lag = fraction 0.4142135624 k *. 2. *. Float.pi in
        ( fraction 0.7548776662 k *. float_of_int mote_width,
          (0.15 +. (0.7 *. fraction 0.5698402910 k))
          *. float_of_int mote_height
          +. (swing *. sin (lag +. turn)),
          1.2 +. (1.7 *. fraction 0.7320508076 k),
          0.4 +. (0.6 *. fraction 0.4142135624 k) ))
  in
  Image.make ~height:mote_height mote_width (fun ~u ~v ->
      let x = float_of_int u +. 0.5 and y = float_of_int v +. 0.5 in
      (* The brightest speck covering this pixel, faded across most of its own
         radius rather than only at the rim. Sampling is nearest-neighbour, so a
         speck two texels across is several screen pixels across when you walk
         up to it; a gradient that wide is what keeps it a speck rather than a
         square. *)
      let lit =
        List.fold_left
          (fun best (cx, cy, radius, bright) ->
            let d = Float.hypot (x -. cx) (y -. cy) in
            if d >= radius then best
            else
              Float.max best
                (bright *. Float.min 1. (1.5 *. (1. -. (d /. radius)))))
          0. specks
      in
      if lit <= 0.02 then Image.clear
      else
        let grey = int_of_float (150. +. (85. *. lit)) in
        (Color.rgb grey (grey - 6) (grey - 24), int_of_float (235. *. lit)))

(** Every frame of it, built once when this module is loaded.

    This is what {!Camlcast.Room.with_sprites} means by precomputed: all twelve
    pictures exist before the first frame is drawn, and animating the cloud is
    choosing one of them by index. Nothing generates an image while the world is
    being rendered — see {!Floating}, which does the choosing. *)
let motes = Array.init mote_frames (fun frame -> mote ~frame)

(** A standing figure: a head, a shirted torso with arms, and legs. *)
let figure =
  Image.make 32 (fun ~u ~v ->
      if Image.disc ~cx:16. ~cy:7. ~r:4.5 u v then (Color.rgb 226 182 142, 255)
      else if u >= 7 && u <= 25 && v >= 12 && v <= 15 then
        (Color.rgb 58 88 158, 255)
      else if u >= 10 && u <= 22 && v >= 11 && v <= 21 then
        (Color.rgb 72 104 182, 255)
      else if u >= 12 && u <= 20 && v >= 21 && v <= 31 then
        (Color.rgb 44 46 62, 255)
      else Image.clear)
