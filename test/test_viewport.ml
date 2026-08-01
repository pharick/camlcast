open Camlcast_core
open Support

let at ?(pitch = 0.) ?(eye_z = 0.5) ~width ~height () =
  Viewport.make ~pitch ~eye_z ~width ~height

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

(* The horizon is a place on the screen and not a row, and a row is its centre,
   so whether any row sits exactly on it is a question of parity. At an odd
   height the middle of the buffer falls on a pixel's centre and that row reads
   exactly zero; at an even one it falls on the boundary between two, which
   straddle zero by half a pixel each and neither of which is the horizon. *)
let row_factor_is_zero_at_the_horizon () =
  let odd = at ~width:800 ~height:601 () in
  Alcotest.check close "an odd height puts a row on the horizon" 0.
    (Viewport.row_factor odd ~row:(odd.Viewport.height / 2));
  let half = 0.5 /. reference.Viewport.projection in
  Alcotest.check close "an even one has the row above it half a pixel high"
    (-.half)
    (Viewport.row_factor reference ~row:299);
  Alcotest.check close "and the row below it half a pixel low" half
    (Viewport.row_factor reference ~row:300);
  (* A minimised window is one pixel, and that pixel is the horizon. Under the
     old convention it read half a screen's worth of factor instead. *)
  let tiny = at ~width:1 ~height:1 () in
  Alcotest.check close "and the one pixel of a 1x1 window is the horizon" 0.
    (Viewport.row_factor tiny ~row:0)

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

(* The column [Paint.crosshair] draws on is [width / 2], and [Sight] answers
   about the ray straight ahead, so the two agree exactly when that column's
   centre is the middle of the buffer — which it is at every odd width, the
   sizes [Renderer.internal_size] actually produces from a 1366- or 2560-wide
   window. An even width has no middle column at all: the middle falls between
   two, and the most that can be asked is that neither is favoured.

   Half a pixel buys more than it used to. [Sight] reads the texel under the
   crosshair now, so at an even width the ray it traces and the ray through the
   pixel drawn can fall either side of a texel edge, and the two disagree over a
   grille's bar by one texel of the pattern. That is what the bound below is
   worth in the world, and why it is asserted and not waived. *)
let the_centre_column_looks_straight_ahead () =
  let player = Player.make ~room:0 ~pos:centre ~angle:0.7 in
  List.iter
    (fun width ->
      let v = at ~width ~height:600 () in
      Alcotest.check vec
        (Printf.sprintf "%d columns: the middle one is dir" width)
        player.Player.dir
        (Viewport.ray_direction v player ~column:(width / 2)))
    [ 1; 161; 683 ];
  let off column =
    Vec.sub (Viewport.ray_direction reference player ~column) player.Player.dir
  in
  Alcotest.check vec
    "an even width straddles it, the two middle columns equal and opposite"
    (Vec.make 0. 0.)
    (Vec.add (off 399) (off 400))

(* The payoff of the camera plane construction: a flat wall seen head-on reports
   one distance across the whole window, at any shape, so it is drawn flat
   rather than bulging towards the viewer. *)
let a_flat_wall_stays_flat () =
  List.iter
    (fun (width, height) ->
      let v = at ~width ~height () in
      let player = Player.make ~room:0 ~pos:centre ~angle:0. in
      List.iter
        (fun column ->
          let direction = Viewport.ray_direction v player ~column in
          let hit = nearest_hit room ~origin:player.Player.pos ~direction in
          Alcotest.check close
            (Printf.sprintf "%dx%d column %d" width height column)
            2. hit.Ray.distance)
        [ 0; width / 4; width / 2; width - 1 ])
    [ (800, 600); (1920, 1080); (400, 900) ]

(* The rule the module is built on, stated as a round trip: [ray_direction]
   takes a column and answers for its centre, [project_point] answers in the
   continuous coordinates that centre lives on, so a point placed along a
   column's own ray projects back to [column + 0.5]. Everything else about the
   convention follows from this — it is what makes [Float.round] of a projected
   extent name the pixels whose centres it covers, which is how the renderer
   turns a wall or a sprite into rows and columns. Sample the edges as well as
   the middle: an error in the half would show at column 0 and width - 1 first. *)
let a_column_projects_back_to_its_own_centre () =
  List.iter
    (fun (width, height) ->
      let v = at ~width ~height () in
      let pose = Player.make ~room:0 ~pos:centre ~angle:0.7 in
      List.iter
        (fun column ->
          let direction = Viewport.ray_direction v pose ~column in
          let point = Vec.add pose.Player.pos (Vec.scale direction 3.) in
          let x, _ =
            Option.get
              (Viewport.project_point v pose ~point ~z:pose.Player.pitch)
          in
          Alcotest.check close
            (Printf.sprintf "%dx%d column %d" width height column)
            (float_of_int column +. 0.5)
            x)
        (List.sort_uniq compare [ 0; width / 2; width - 1 ]))
    [ (800, 600); (161, 101); (1, 1) ]

(* The one place three modules have to agree, and the disagreement this change
   exists to end: the pixel [Paint] draws the crosshair on, the ray [Viewport]
   casts through that pixel, and the ray [Sight] picks along — which is
   [player.dir] with [Viewport.centre_rise], zero at a level pitch.

   The crosshair is found by looking at what was actually drawn rather than by
   recomputing [width / 2], so this fails if either module moves and the other
   does not. *)
let the_crosshair_sits_on_the_centre_ray () =
  let player = Player.make ~room:0 ~pos:centre ~angle:0.7 in
  List.iter
    (fun (width, height) ->
      let fb = Framebuffer.offscreen ~width ~height in
      Paint.crosshair fb ~color:(Color.rgb 255 255 255);
      let lit ~x ~y = (Framebuffer.pixel fb ~x ~y).Color.r > 0 in
      (* The arms cross on one pixel, so the row carrying the horizontal arm has
         more lit pixels in it than any other and likewise the column carrying
         the vertical one. Whichever line has strictly the most is the middle,
         and asking for it that way holds down to a buffer of one pixel. *)
      let busiest what n count =
        let tally = List.map (fun i -> (i, count i)) (List.init n Fun.id) in
        let most = List.fold_left (fun m (_, c) -> Int.max m c) 0 tally in
        match List.filter (fun (_, c) -> c = most) tally with
        | [ (i, _) ] -> i
        | found -> Alcotest.failf "%s: %d lines tie" what (List.length found)
      in
      let count_if n at = List.length (List.filter at (List.init n Fun.id)) in
      let v = at ~width ~height () in
      let name = Printf.sprintf "%dx%d" width height in
      let cy =
        busiest (name ^ " row") height (fun y ->
            count_if width (fun x -> lit ~x ~y))
      in
      let cx =
        busiest (name ^ " column") width (fun x ->
            count_if height (fun y -> lit ~x ~y))
      in
      (* An odd size puts a pixel's centre on the middle of the buffer, so the
         agreement is exact. An even one puts the middle on a boundary, where
         the most that can be true of any pixel is that its centre is half a
         pixel from it — and that is asserted rather than waived, because a
         crosshair drawn anywhere else would still pass the odd cases. *)
      if width mod 2 = 1 then
        Alcotest.check vec
          (name ^ ": the crosshair column is dir")
          player.Player.dir
          (Viewport.ray_direction v player ~column:cx)
      else
        Alcotest.(check bool)
          (name ^ ": the crosshair column is half a pixel from the middle")
          true
          (Float.abs (float_of_int cx +. 0.5 -. (float_of_int width /. 2.))
          <= 0.5);
      if height mod 2 = 1 then
        Alcotest.check close
          (name ^ ": and its row is the horizon")
          0.
          (Viewport.row_factor v ~row:cy)
      else
        Alcotest.(check bool)
          (name ^ ": and its row half a pixel from the horizon")
          true
          (Float.abs (float_of_int cy +. 0.5 -. v.Viewport.horizon) <= 0.5);
      (* Which is the ray Sight traces: level, it rises nowhere. *)
      Alcotest.check close
        (name ^ ": and Sight looks flat along it")
        0.
        (Viewport.centre_rise ~pitch:0.))
    [ (161, 101); (683, 384); (1, 1); (160, 100) ]

(* {1 Billboards}

   [sprite_box] is what the renderer draws a sprite in and what anything wanting
   to ring one has to land on, so the two features a sprite gained — a base
   above the floor and a width from its own picture — are asserted here, on the
   rectangle, rather than only on the pixels in test_renderer. *)

let facing_east = Player.make ~room:0 ~pos:(Vec.make 0. 0.) ~angle:0.

(* A square picture is what every sprite used to be, and it comes out square. *)
let a_square_picture_is_as_wide_as_it_is_tall () =
  let s =
    Room.sprite ~size:1.5
      ~image:(Image.make ~width:16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)))
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
      ~image:
        (Image.make ~height:8 ~width:16 (fun ~u:_ ~v:_ ->
             (Color.rgb 1 1 1, 255)))
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
      ~image:(Image.make ~width:16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)))
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
  let image = Image.make ~width:16 (fun ~u:_ ~v:_ -> (Color.rgb 1 1 1, 255)) in
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
          case "a column projects back to its own centre"
            a_column_projects_back_to_its_own_centre;
        ] );
      ( "the crosshair",
        [ case "sits on the centre ray" the_crosshair_sits_on_the_centre_ray ]
      );
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
