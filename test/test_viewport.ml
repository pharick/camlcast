open Camlcast
open Support

let at ?(pitch = 0.) ?(eye_z = 0.5) ~width ~height () =
  Viewport.create ~pitch ~eye_z ~width ~height

let reference = at ~width:800 ~height:600 ()

let clamps_a_minimised_window () =
  let v = at ~width:0 ~height:0 () in
  Alcotest.(check int) "width is usable" 1 v.Viewport.width;
  Alcotest.(check int) "height is usable" 1 v.Viewport.height;
  Alcotest.(check bool)
    "and the maths stays finite" true
    (Float.is_finite v.Viewport.half_width
    && Float.is_finite v.Viewport.projection)

(* The rule the whole module is built on: a wall one cell wide and one cell
   tall must cover the same number of pixels in both directions. *)
let pixels_stay_square () =
  List.iter
    (fun (width, height) ->
      let v = at ~width ~height () in
      Alcotest.check close
        (Printf.sprintf "%dx%d" width height)
        (float_of_int width /. 2. /. v.Viewport.projection)
        v.Viewport.half_width)
    [ (800, 600); (1920, 1080); (600, 800); (1, 1); (2560, 1440) ]

let the_reference_shape_gets_the_configured_fov () =
  Alcotest.check close "half width = tan (fov / 2)"
    (Float.tan (Config.fov /. 2.))
    reference.Viewport.half_width

(* Hor+: a wider window shows more of the world to the sides, and does not
   magnify what was already on screen. *)
let widening_reveals_more_world () =
  let wide = at ~width:1200 ~height:600 () in
  Alcotest.(check bool)
    "the horizontal field of view grows" true
    (wide.Viewport.half_width > reference.Viewport.half_width);
  Alcotest.check close "the vertical field of view is untouched"
    reference.Viewport.projection wide.Viewport.projection

let the_vertical_field_of_view_is_fixed () =
  let doubled = at ~width:1600 ~height:1200 () in
  Alcotest.check close "half width is shape-only, not size"
    reference.Viewport.half_width doubled.Viewport.half_width;
  Alcotest.check close "projection scales with the pixels"
    (2. *. reference.Viewport.projection)
    doubled.Viewport.projection

(* A point at exactly eye height projects onto the horizon, however far away it
   is — that is what "eye height" means on screen. *)
let eye_height_lands_on_the_horizon () =
  List.iter
    (fun distance ->
      Alcotest.check close
        (Printf.sprintf "%g away" distance)
        reference.Viewport.horizon
        (Viewport.project_height reference ~z:0.5 ~distance))
    [ 0.5; 2.; 10. ]

let higher_points_project_higher () =
  let row z = Viewport.project_height reference ~z ~distance:3. in
  Alcotest.(check bool)
    "a point above the eye is above the horizon (smaller row)" true
    (row 1.5 < reference.Viewport.horizon);
  Alcotest.(check bool)
    "a point below the eye is below it" true
    (row (-0.5) > reference.Viewport.horizon)

(* The projected offset from the horizon is inversely proportional to distance:
   twice as far, half as tall. *)
let projection_is_inversely_proportional () =
  let offset distance =
    Viewport.project_height reference ~z:1.5 ~distance
    -. reference.Viewport.horizon
  in
  Alcotest.check close "twice as far is half the offset"
    (offset 4. /. 2.)
    (offset 8.)

let row_factor_is_zero_at_the_horizon () =
  let row = int_of_float reference.Viewport.horizon in
  Alcotest.check close "the horizon row sits at factor zero" 0.
    (Viewport.row_factor reference ~row)

(* Pitch shears the horizon away from the middle of the window: looking up
   drops it (more ceiling), looking down raises it (more floor). *)
let pitch_shears_the_horizon () =
  let middle = float_of_int reference.Viewport.height /. 2. in
  Alcotest.check close "level, the horizon is the middle row" middle
    reference.Viewport.horizon;
  Alcotest.(check bool)
    "looking up drops the horizon" true
    ((at ~pitch:0.2 ~width:800 ~height:600 ()).Viewport.horizon > middle);
  Alcotest.(check bool)
    "looking down raises it" true
    ((at ~pitch:(-0.2) ~width:800 ~height:600 ()).Viewport.horizon < middle)

let the_centre_column_looks_straight_ahead () =
  let player = Player.create ~room:0 ~pos:centre ~angle:0.7 in
  let direction =
    Viewport.ray_direction reference player
      ~column:(reference.Viewport.width / 2)
  in
  Alcotest.check vec "the middle of the screen is dir" player.Player.dir
    direction

(* The payoff of the camera plane construction: a flat wall seen head-on reports
   one distance across the whole window, at any shape, so it is drawn flat
   rather than bulging towards the viewer. *)
let a_flat_wall_stays_flat () =
  List.iter
    (fun (width, height) ->
      let v = at ~width ~height () in
      let player = Player.create ~room:0 ~pos:centre ~angle:0. in
      List.iter
        (fun column ->
          let direction = Viewport.ray_direction v player ~column in
          let hit = nearest_hit room ~origin:player.Player.pos ~direction in
          Alcotest.check close
            (Printf.sprintf "%dx%d column %d" width height column)
            2. hit.Ray.distance)
        [ 0; width / 4; width / 2; width - 1 ])
    [ (800, 600); (1920, 1080); (400, 900) ]

(* {1 Billboards}

   [sprite_box] is what the renderer draws a sprite in and what anything wanting
   to ring one has to land on, so the two features a sprite gained — a base
   above the floor and a width from its own picture — are asserted here, on the
   rectangle, rather than only on the pixels in test_renderer. *)

let facing_east = Player.create ~room:0 ~pos:(Vec.make 0. 0.) ~angle:0.

(* A square picture is what every sprite used to be, and it comes out square. *)
let a_square_picture_is_as_wide_as_it_is_tall () =
  let s =
    Room.sprite ~size:1.5
      ~image:(Image.make 16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)))
      (Vec.make 4. 0.)
  in
  List.iter
    (fun distance ->
      let l, t, r, b =
        Viewport.sprite_box reference facing_east ~floor_z:0. ~distance s
      in
      Alcotest.check close
        (Printf.sprintf "at %.0f cells" distance)
        (b -. t) (r -. l);
      Alcotest.check close "and centred, dead ahead" 400. ((l +. r) /. 2.))
    [ 2.; 4.; 10. ]

(* And a picture twice as wide as it is tall is drawn twice as wide as it is
   tall, at every distance, because the aspect is a ratio and the projection
   scales both edges together. *)
let a_wide_picture_is_as_wide_as_its_picture () =
  let s =
    Room.sprite ~size:1.5
      ~image:(Image.make ~height:8 16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)))
      (Vec.make 4. 0.)
  in
  List.iter
    (fun distance ->
      let l, t, r, b =
        Viewport.sprite_box reference facing_east ~floor_z:0. ~distance s
      in
      Alcotest.check close
        (Printf.sprintf "twice as wide at %.0f cells" distance)
        (2. *. (b -. t))
        (r -. l);
      Alcotest.check close "and still centred" 400. ((l +. r) /. 2.))
    [ 2.; 4.; 10. ]

(* A base lifts the whole billboard and changes nothing else: both edges rise by
   the rows the projection gives that height, and the columns do not move. *)
let a_base_raises_both_edges_together () =
  let at base =
    Room.sprite ~base ~size:1.5
      ~image:(Image.make 16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)))
      (Vec.make 4. 0.)
  in
  let ground =
    Viewport.sprite_box reference facing_east ~floor_z:0. ~distance:4. (at 0.)
  and lifted =
    Viewport.sprite_box reference facing_east ~floor_z:0. ~distance:4. (at 1.25)
  in
  let gl, gt, gr, gb = ground and ll, lt, lr, lb = lifted in
  Alcotest.check close "the same left edge" gl ll;
  Alcotest.check close "the same right edge" gr lr;
  let rise =
    Viewport.project_height reference ~z:0. ~distance:4.
    -. Viewport.project_height reference ~z:1.25 ~distance:4.
  in
  Alcotest.check close "the top rose" (gt -. rise) lt;
  Alcotest.check close "and the foot by the same" (gb -. rise) lb

(* The lift is measured from the floor under it and not from an absolute height,
   which is what makes a sprite ride a slope instead of the ground climbing
   through it. Raising the floor by a cell and raising the base by a cell are
   the same picture. *)
let a_base_is_measured_from_the_floor () =
  let image = Image.make 16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)) in
  let on_a_high_floor =
    Viewport.sprite_box reference facing_east ~floor_z:1. ~distance:4.
      (Room.sprite ~size:1.5 ~image (Vec.make 4. 0.))
  and lifted_off_a_low_one =
    Viewport.sprite_box reference facing_east ~floor_z:0. ~distance:4.
      (Room.sprite ~base:1. ~size:1.5 ~image (Vec.make 4. 0.))
  in
  let _, at, _, ab = on_a_high_floor and _, bt, _, bb = lifted_off_a_low_one in
  Alcotest.check close "the same top" at bt;
  Alcotest.check close "the same foot" ab bb

let () =
  Alcotest.run "Viewport"
    [
      ( "camera",
        [
          case "clamps a minimised window" clamps_a_minimised_window;
          case "pixels stay square" pixels_stay_square;
          case "the reference shape gets the configured fov"
            the_reference_shape_gets_the_configured_fov;
        ] );
      ( "resizing",
        [
          case "widening reveals more world" widening_reveals_more_world;
          case "the vertical field of view is fixed"
            the_vertical_field_of_view_is_fixed;
        ] );
      ( "projection",
        [
          case "eye height lands on the horizon" eye_height_lands_on_the_horizon;
          case "higher points project higher" higher_points_project_higher;
          case "projection is inversely proportional"
            projection_is_inversely_proportional;
          case "row factor is zero at the horizon"
            row_factor_is_zero_at_the_horizon;
          case "pitch shears the horizon" pitch_shears_the_horizon;
        ] );
      ( "rays",
        [
          case "the centre column looks straight ahead"
            the_centre_column_looks_straight_ahead;
          case "a flat wall stays flat at any window shape"
            a_flat_wall_stays_flat;
        ] );
      ( "billboards",
        [
          case "a square picture is as wide as it is tall"
            a_square_picture_is_as_wide_as_it_is_tall;
          case "a wide picture is as wide as its picture"
            a_wide_picture_is_as_wide_as_its_picture;
          case "a base raises both edges together"
            a_base_raises_both_edges_together;
          case "a base is measured from the floor"
            a_base_is_measured_from_the_floor;
        ] );
    ]
