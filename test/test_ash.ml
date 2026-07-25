open Raycaster
open Assets
open Support

let surfaces =
  [
    ("plaster", Ash.plaster);
    ("scored", Ash.scored);
    ("ground", Ash.ground);
    ("soffit", Ash.soffit);
  ]

let extremes (t : Texture.t) =
  let lo = ref 255 and hi = ref 0 in
  for v = 0 to Texture.size - 1 do
    for u = 0 to Texture.size - 1 do
      let s = Texture.sample t ~u ~v in
      if s < !lo then lo := s;
      if s > !hi then hi := s
    done
  done;
  (!lo, !hi)

(* The whole look rests on this. Too narrow and the wall is a flat fill with no
   surface to it; too wide and it starts to read as concrete, which is a
   material, which is the one thing the House is not allowed to be. *)
let the_band_is_narrow () =
  List.iter
    (fun (name, t) ->
      let lo, hi = extremes t in
      Alcotest.(check bool)
        (Printf.sprintf "%s spreads %d levels, wanted 16..64" name (hi - lo))
        true
        (hi - lo >= 16 && hi - lo <= 64))
    surfaces

(* No courses, no joints, no grout lines. A masonry pattern has a hard step from
   brick to mortar somewhere in it; ash has nothing anywhere with an edge. *)
let nothing_has_an_edge () =
  List.iter
    (fun (name, t) ->
      let worst = ref 0 in
      for v = 0 to Texture.size - 1 do
        for u = 0 to Texture.size - 1 do
          let here = Texture.sample t ~u ~v in
          let right = Texture.sample t ~u:((u + 1) mod Texture.size) ~v
          and down = Texture.sample t ~u ~v:((v + 1) mod Texture.size) in
          worst :=
            Int.max !worst (Int.max (abs (here - right)) (abs (here - down)))
        done
      done;
      Alcotest.(check bool)
        (Printf.sprintf "%s has no step worse than %d" name !worst)
        true
        (!worst <= 14))
    surfaces

(* A wall's pattern repeats once per world unit, so an edge at the wrap would be
   the only thing in the whole House with a shape to it, one per cell, all the
   way down every corridor. The check above already sampled across the wrap, so
   this pins the reason: the seam is no worse than anywhere else. *)
let the_wrap_is_invisible () =
  List.iter
    (fun (name, t) ->
      let last = Texture.size - 1 in
      let seam = ref 0 and interior = ref 0 in
      for k = 0 to last do
        seam :=
          Int.max !seam
            (Int.max
               (abs (Texture.sample t ~u:last ~v:k - Texture.sample t ~u:0 ~v:k))
               (abs (Texture.sample t ~u:k ~v:last - Texture.sample t ~u:k ~v:0)))
      done;
      for v = 0 to last do
        for u = 0 to last - 1 do
          interior :=
            Int.max !interior
              (abs (Texture.sample t ~u ~v - Texture.sample t ~u:(u + 1) ~v))
        done
      done;
      Alcotest.(check bool)
        (Printf.sprintf "%s: seam %d is no worse than interior %d" name !seam
           !interior)
        true
        (!seam <= !interior))
    surfaces

(* Every surface has to be its own; four fields from the same noise with the
   same seed would make floor, wall and ceiling one continuous substance and
   lose which way is up. *)
let the_surfaces_are_distinct () =
  let fingerprint (_, t) =
    List.map
      (fun (u, v) -> Texture.sample t ~u ~v)
      [ (0, 0); (9, 17); (32, 32); (55, 3); (63, 63) ]
  in
  Alcotest.(check int)
    "four surfaces, four fields" (List.length surfaces)
    (List.length (List.sort_uniq compare (List.map fingerprint surfaces)))

(* [scored] exists to stop a long corridor wall dissolving into flat fog, by
   suggesting verticals the eye can follow away from itself. So its variation
   has to be more horizontal than vertical: a column of it stays put while a row
   of it moves. *)
let scored_runs_vertically () =
  let spread values =
    List.fold_left Int.max 0 values - List.fold_left Int.min 255 values
  in
  let down = List.init 64 (fun v -> Texture.sample Ash.scored ~u:20 ~v)
  and across = List.init 64 (fun u -> Texture.sample Ash.scored ~u ~v:20) in
  Alcotest.(check bool)
    (Printf.sprintf "across (%d) varies more than down (%d)" (spread across)
       (spread down))
    true
    (spread across > spread down)

(* The door is the one thing in the House that somebody made, and the only
   pattern with a straight line in it. That is what makes it read as a door
   while everything around it reads as wall. *)
let the_door_is_the_one_made_thing () =
  let step (t : Texture.t) =
    let worst = ref 0 in
    for v = 0 to Texture.size - 1 do
      for u = 0 to Texture.size - 2 do
        worst :=
          Int.max !worst
            (abs (Texture.sample t ~u ~v - Texture.sample t ~u:(u + 1) ~v))
      done
    done;
    !worst
  in
  Alcotest.(check bool)
    (Printf.sprintf "the leaf has an edge (%d) the wall has not (%d)"
       (step Ash.leaf) (step Ash.plaster))
    true
    (step Ash.leaf > step Ash.plaster * 2);
  Alcotest.(check bool)
    "its frame is darker than its face" true
    (Texture.sample Ash.leaf ~u:1 ~v:32 < Texture.sample Ash.leaf ~u:32 ~v:20)

(* Ash grey is not one colour. Close enough to be the same substance, far enough
   apart that a corridor does not read as a featureless tube. *)
let the_greys_are_close_but_ordered () =
  let value (m : Material.t) =
    let c = m.Material.color in
    c.Color.r + c.Color.g + c.Color.b
  in
  Alcotest.(check bool)
    "the wall is lighter than the floor" true
    (value Surfaces.wall > value Surfaces.floor);
  Alcotest.(check bool)
    "and the floor lighter than the ceiling" true
    (value Surfaces.floor > value Surfaces.ceiling);
  (* Perfectly neutral grey reads as digital; these are all a shade warm. *)
  List.iter
    (fun (name, (m : Material.t)) ->
      let c = m.Material.color in
      Alcotest.(check bool)
        (name ^ " is desaturated but not neutral")
        true
        (c.Color.r > c.Color.b && c.Color.r - c.Color.b <= 12))
    [
      ("wall", Surfaces.wall);
      ("floor", Surfaces.floor);
      ("ceiling", Surfaces.ceiling);
    ]

(* Three numbers make the House the House, and all three are the opposite of a
   daylit level's. *)
let the_air_closes_in () =
  let air = Surfaces.air in
  Alcotest.(check bool)
    "the far end of a corridor is gone before you have walked it" true
    (air.Atmosphere.fog_distance <= 10.);
  Alcotest.(check bool)
    "and what it fades to is effectively black" true
    (air.Atmosphere.min_brightness <= 0.1
    && air.Atmosphere.haze.Color.r <= 16
    && air.Atmosphere.haze.Color.g <= 16
    && air.Atmosphere.haze.Color.b <= 16);
  (* No shadows and no discernible source: a wall is very nearly the same
     brightness whichever way it faces, so distance is the only thing left that
     tells one surface from another. *)
  let brightest = ref 0. and dimmest = ref 1. in
  List.iter
    (fun angle ->
      let f = Atmosphere.face_shading air (Vec.of_angle angle) in
      if f > !brightest then brightest := f;
      if f < !dimmest then dimmest := f)
    (List.init 24 (fun i -> float_of_int i *. Float.pi /. 12.));
  Alcotest.(check bool)
    (Printf.sprintf "orientation spans only %.2f" (!brightest -. !dimmest))
    true
    (!brightest -. !dimmest <= 0.2)

let () =
  Alcotest.run "Ash"
    [
      ( "the surface",
        [
          case "the band is narrow" the_band_is_narrow;
          case "nothing has an edge" nothing_has_an_edge;
          case "the wrap is invisible" the_wrap_is_invisible;
          case "the surfaces are distinct" the_surfaces_are_distinct;
          case "scored runs vertically" scored_runs_vertically;
          case "the door is the one made thing" the_door_is_the_one_made_thing;
        ] );
      ( "the House",
        [
          case "the greys are close but ordered" the_greys_are_close_but_ordered;
          case "the air closes in" the_air_closes_in;
        ] );
    ]
