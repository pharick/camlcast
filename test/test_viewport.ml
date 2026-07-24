open Raycaster
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
  let player = Player.create ~pos:centre ~angle:0.7 in
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
      let player = Player.create ~pos:centre ~angle:0. in
      List.iter
        (fun column ->
          let direction = Viewport.ray_direction v player ~column in
          let hit = nearest_hit room ~origin:player.Player.pos ~direction in
          Alcotest.check close
            (Printf.sprintf "%dx%d column %d" width height column)
            2. hit.Ray.distance)
        [ 0; width / 4; width / 2; width - 1 ])
    [ (800, 600); (1920, 1080); (400, 900) ]

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
    ]
