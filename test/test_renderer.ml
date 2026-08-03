(** What actually arrives in the framebuffer, one pixel at a time.

    {!Renderer.draw_frame} is documented as pure array writes with no SDL calls
    in it, and {!Framebuffer.offscreen} builds a buffer with no window behind
    it, so a whole frame renders here headlessly. Everything below is about
    sprites: where a billboard lands, what hides it, and what trims it. The rest
    of the renderer is covered through {!Plane}, {!Viewport}, {!Material} and
    {!Atmosphere}, whose arithmetic it is.

    {1 How a sprite is found on the screen}

    Not by looking for its colour. What reaches the buffer has been through fog
    and, behind it, through wall shading, so the number in the pixel is nobody's
    idea of the colour that went in — and asserting on it would be asserting on
    {!Atmosphere}, which has its own suite. Instead every test draws the frame
    {e twice}, once with the sprite and once without, and the sprite is exactly
    the pixels that differ. That answers "where was it drawn" without any claim
    about what it was drawn in. *)

open Camlcast_core
open Support

let width = 160
let height = 100

(* A sprite is a picture, and these are the two that matter here: one square and
   one twice as wide as it is tall, both solid to the edges so that the pixels
   that change are the whole billboard and not a cut-out inside it. The colour
   is only ever compared against itself. *)
let solid ?height w =
  Image.make ?height ~width:w (fun ~u:_ ~v:_ -> (Color.rgb 255 0 255, 255))

let square = solid 16
let wide = solid ~height:8 16

(** The box a sprite covers on screen: the smallest rectangle holding every
    pixel that differs between the two worlds, as [(left, top, right, bottom)],
    or [None] where the sprite reached nothing at all. *)
let drawn ?(width = width) ?(height = height) ~with_it ~without player =
  let a = Framebuffer.offscreen ~width ~height
  and b = Framebuffer.offscreen ~width ~height in
  Renderer.draw_frame a with_it player;
  Renderer.draw_frame b without player;
  let box = ref None in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if Framebuffer.pixel a ~x ~y <> Framebuffer.pixel b ~x ~y then
        box :=
          Some
            (match !box with
            | None -> (x, y, x, y)
            | Some (l, t, r, b) ->
                (Int.min l x, Int.min t y, Int.max r x, Int.max b y))
    done
  done;
  !box

let box ?width ?height ~with_it ~without player =
  match drawn ?width ?height ~with_it ~without player with
  | Some b -> b
  | None -> Alcotest.fail "the sprite was not drawn at all"

(* One room, one sprite, and the same room without it. Both are built from the
   same parts so that the only difference between the two frames is the
   billboard. [floor] is a plane so that the sloped case is the same fixture. *)
let hall ?(floor = Plane.horizontal 0.) ?ceiling ?(extra = []) sprites =
  Room.make
    ~floor:{ Room.plane = floor; material = pale }
    ~ceiling:
      (Option.value ceiling
         ~default:
           (Room.Roof { Room.plane = Plane.above floor 3.; material = dim }))
    ~sprites
    ([
       Room.wall ~height:3. ~material:pale (Vec.make (-4.) (-4.))
         (Vec.make 12. (-4.));
       Room.wall ~height:3. ~material:pale (Vec.make 12. (-4.))
         (Vec.make 12. 4.);
       Room.wall ~height:3. ~material:pale (Vec.make 12. 4.) (Vec.make (-4.) 4.);
       Room.wall ~height:3. ~material:pale (Vec.make (-4.) 4.)
         (Vec.make (-4.) (-4.));
     ]
    @ extra)

let alone ?floor ?extra sprites =
  let one room =
    World.make
      ~rooms:[ ("hall", room) ]
      ~links:[] ~atmosphere:air
      ~spawn:("hall", Vec.make 0. 0.)
  in
  (one (hall ?floor ?extra sprites), one (hall ?floor ?extra []))

(** At the origin, looking east down the hall, level. *)
let looking_east ?(pos = Vec.make 0. 0.) () = Player.make ~room:0 ~pos ~angle:0.

(** The viewport a frame of this size is drawn through, over a floor at
    [floor_z] — what {!Viewport.sprite_box} has to be asked with if its answer
    is to be comparable with what landed on the buffer. *)
let viewport ~floor_z =
  Viewport.make ~pitch:0. ~eye_z:(floor_z +. Config.eye_height) ~width ~height

(* Everything else in the family above — a grille, a room through a doorway, the
   corner of one, a mark on a wall — is picked where it is drawn because both
   sides read one function. A sprite is the exception, and it is the exception on
   purpose: {!Sight} asks {!Room.sprite_column} and {!Room.sprite_row} in world
   coordinates, while {!Renderer} inverts them once into a screen rectangle and
   interpolates across it, one divide per column instead of a dot product per
   pixel. The two are the same map read from opposite ends, and nothing but care
   was holding them equal.

   Care was not enough. {!Sight} passed the sprite's offset from the eye where
   {!Room.sprite_column} wanted the crosshair's offset from the sprite, so the
   picking was the mirror image of the drawing — a sprite pickable exactly where
   it is transparent. Nothing caught it: the width test is on an absolute value,
   the row is a separate calculation that was always right, and every sprite in
   the suite was symmetric or solid to its edges.

   So this is the case that could not have been written as a claim about
   coordinates. It sweeps a lopsided sprite across the view and asks the two
   questions that must have one answer — did the frame put this sprite under the
   crosshair, and does the crosshair say it is on it — reading the first from the
   picture, by rendering the same world with and without it and looking at the
   one pixel in the middle.

   The picture is cut at column 5 of 16 rather than at its middle, which is not
   fussiness. Dead ahead the crosshair falls on the box's exact centre, and for
   an even-width picture that is exactly a texel boundary: the renderer reaches
   that real number through the projection and {!Sight} through
   {!Room.sprite_half_width}, they agree to about [5e-16], and a [floor] turns
   that into two different columns. Off the middle, the ulp changes nothing. *)
let a_sprite_is_picked_where_it_is_drawn () =
  let lopsided =
    Image.make ~width:16 ~height:16 (fun ~u ~v:_ ->
        if u < 5 then Image.clear else (Color.rgb 255 0 255, 255))
  in
  (* Odd extents, so there is an exact middle pixel and the crosshair has one
     pixel to be under. *)
  let width = 201 and height = 101 in
  let world sprites =
    World.make
      ~rooms:[ ("hall", hall ~extra:[] sprites) ]
      ~links:[] ~atmosphere:air
      ~spawn:("hall", Vec.make 0. 0.)
  in
  let seen = ref 0 and unseen = ref 0 in
  List.iter
    (fun offset ->
      let s = Room.sprite ~size:1.5 ~image:lopsided (Vec.make 6. offset) in
      let player = looking_east () in
      let with_it = world [ s ] and without = world [] in
      let a = Framebuffer.offscreen ~width ~height
      and b = Framebuffer.offscreen ~width ~height in
      Renderer.draw_frame a with_it player;
      Renderer.draw_frame b without player;
      let x = width / 2 and y = height / 2 in
      let drawn = Framebuffer.pixel a ~x ~y <> Framebuffer.pixel b ~x ~y in
      let picked =
        match Sight.look with_it player with
        | Some { Sight.kind = Sight.Sprite _; _ } -> true
        | _ -> false
      in
      if drawn then incr seen else incr unseen;
      Alcotest.(check bool)
        (Printf.sprintf
           "at %+.2f across, the crosshair agrees with the frame (drawn %b)"
           offset drawn)
        drawn picked)
    [ -0.6; -0.5; -0.4; -0.3; -0.2; -0.1; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6 ];
  (* Both answers have to occur, or a sweep that missed the sprite entirely
     would agree about nothing and pass. *)
  Alcotest.(check bool)
    (Printf.sprintf "the sweep crossed the cut (%d on, %d off)" !seen !unseen)
    true
    (!seen > 0 && !unseen > 0)

(* A material whose colour changes from texel to texel. Everything else in this
   file is drawn on flat [pale], and against a flat material a cast that lands
   at the wrong world point shows the very same colour — the only thing left to
   notice it by is the fog, which is gentle. Graded, a wrong distance is a wrong
   colour. *)
let graded blue =
  Material.make
    ~pattern:(Texture.generate (fun ~u ~v -> Color.rgb (u * 3) (v * 3) blue))

(* The hall again, with its floor and roof graded, and given a sloped plane so
   the gradient term of the cast is doing work rather than falling out of a
   level one. *)
let graded_hall floor =
  Room.make
    ~floor:{ Room.plane = floor; material = graded 40 }
    ~ceiling:
      (Room.Roof { Room.plane = Plane.above floor 3.; material = graded 200 })
    [
      Room.wall ~height:3. ~material:pale (Vec.make (-4.) (-4.))
        (Vec.make 12. (-4.));
      Room.wall ~height:3. ~material:pale (Vec.make 12. (-4.)) (Vec.make 12. 4.);
      Room.wall ~height:3. ~material:pale (Vec.make 12. 4.) (Vec.make (-4.) 4.);
      Room.wall ~height:3. ~material:pale (Vec.make (-4.) 4.)
        (Vec.make (-4.) (-4.));
    ]

(* {!Plane.view_distance} is the engine's written statement of the plane cast,
   and a background pixel is {!Plane.cast} — the same arithmetic with the
   hoisting left to the caller. They were two copies for a long time, with
   renderer.mli claiming the renderer called [view_distance] while it quietly
   ran its own; this is what makes the claim a thing that can fail rather than a
   thing that is merely written down.

   For every background pixel it reaches, the colour in the buffer must be what
   the material shows at the world point [view_distance] puts under that pixel,
   faded by the fog of that same distance — the blend {!Atmosphere.fog}
   documents. That is sensitive to the whole formula rather than to its guard: a
   cast that drifts moves the sample and the fade together, and the grading
   above is there so the sample moving is visible.

   Only pixels nearer than the first wall along their own ray are asked about,
   since past that the background is painted over; the count at the end is so
   that a fixture which quietly stopped reaching any of them would fail here
   rather than pass silently. *)
let the_background_is_the_cast_the_engine_exports () =
  let floor_plane = Plane.make ~a:0.06 ~b:(-0.04) ~c:0. in
  let room = graded_hall floor_plane in
  let world =
    World.make
      ~rooms:[ ("hall", room) ]
      ~links:[] ~atmosphere:air
      ~spawn:("hall", Vec.make 0. 0.)
  in
  let player = looking_east () in
  let fb = Framebuffer.offscreen ~width ~height in
  Renderer.draw_frame fb world player;
  let floor = Room.floor_plane room in
  let roof = Option.get (Room.ceiling_surface room) in
  let eye_z = Plane.elevation floor player.Player.pos +. Config.eye_height in
  let view = Viewport.make ~pitch:0. ~eye_z ~width ~height in
  let haze = air.Atmosphere.haze in
  let checked = ref 0 in
  for column = 0 to width - 1 do
    let dir = Viewport.ray_direction view player ~column in
    let wall =
      match
        Ray.nearest (Ray.cast room ~origin:player.Player.pos ~direction:dir)
      with
      | Some (h : Ray.hit) -> h.Ray.distance
      | None -> infinity
    in
    for row = 0 to height - 1 do
      let row_factor = Viewport.row_factor view ~row in
      (* Which plane this row could be showing is the renderer's own question,
         asked the renderer's own way — below the horizon a floor, above it a
         roof. [view_distance] does not ask it, answering for whichever plane it
         is handed. *)
      let plane, material =
        if row_factor > 0. then (floor, Room.floor_material room)
        else (roof.Room.plane, roof.Room.material)
      in
      match
        Plane.view_distance plane ~eye_z ~eye_pos:player.Player.pos ~dir
          ~row_factor
      with
      | Some d when d < wall -. 0.05 ->
          let p = Vec.add player.Player.pos (Vec.scale dir d) in
          let c = Material.plane_texel material ~x:p.Vec.x ~y:p.Vec.y in
          let f = Atmosphere.fog air d in
          let veil = 1. -. f in
          let mix v h =
            int_of_float ((float_of_int v *. f) +. (float_of_int h *. veil))
          in
          incr checked;
          Alcotest.check color
            (Printf.sprintf "column %d, row %d" column row)
            (Color.rgb
               (mix c.Color.r haze.Color.r)
               (mix c.Color.g haze.Color.g)
               (mix c.Color.b haze.Color.b))
            (Framebuffer.pixel fb ~x:column ~y:row)
      | Some _ | None -> ()
    done
  done;
  Alcotest.(check bool)
    (Printf.sprintf "most of the frame was background (%d pixels)" !checked)
    true
    (!checked > width * height / 4)

(* The drawing half of the cutoff {!Sight} reads. A billboard is scaled by one
   over its distance, so near enough there is nothing left worth placing and the
   renderer stops; nearer than {!Config.sprite_near_clip} it reaches no pixel at
   all. Neither module has a near cutoff of its own, and this is the pair of
   cases on this side that says so — a sprite the crosshair could pick but the
   frame does not show is a target the player cannot see. *)
let a_sprite_nearer_than_the_clip_is_not_drawn () =
  let at d = alone [ Room.sprite ~size:1.6 ~image:square (Vec.make d 0.) ] in
  let clip = Config.sprite_near_clip in
  (* Just beyond it: [box] is what fails if nothing was drawn. *)
  let with_it, without = at (clip +. 0.01) in
  ignore (box ~with_it ~without (looking_east ()));
  (* Just inside it, and the two frames are the same frame. *)
  let with_it, without = at (clip -. 0.01) in
  Alcotest.(check bool)
    "one nearer than the clip changes no pixel" true
    (drawn ~with_it ~without (looking_east ()) = None)

(* A sprite standing on the floor is where it always was: on the rectangle
   Viewport.sprite_box gives, and — its picture being square — as wide as it is
   tall. This is the case every other demo and every other suite already
   depends on, so it is the one that says the new field changed nothing. *)
let a_sprite_on_the_floor_is_where_the_viewport_says () =
  let s = Room.sprite ~size:1.6 ~image:square (Vec.make 5. 0.) in
  let with_it, without = alone [ s ] in
  let l, t, r, b = box ~with_it ~without (looking_east ()) in
  let { Viewport.left = el; top = et; right = er; bottom = eb } =
    Viewport.sprite_box (viewport ~floor_z:0.) (looking_east ()) ~floor_z:0.
      ~distance:5. s
  in
  let near name got expected =
    Alcotest.(check bool)
      (Printf.sprintf "%s: drawn at %d, predicted %.2f" name got expected)
      true
      (Float.abs (float_of_int got -. expected) <= 1.5)
  in
  near "left" l el;
  near "top" t et;
  near "right" r er;
  near "bottom" b eb;
  Alcotest.(check bool)
    (Printf.sprintf "as wide as it is tall (%d x %d)" (r - l) (b - t))
    true
    (abs (r - l - (b - t)) <= 2)

(* Lifting a sprite moves it up the screen and nowhere else: the same columns,
   and both edges raised by the rows the projection predicts. Nothing about the
   billboard changes but where its foot is. *)
let a_base_lifts_it_and_does_nothing_else () =
  (* Far enough down the hall that the lifted one still fits on the buffer:
     raised at five cells it would run off the top, and a box cut by the edge
     is not a box this can compare. *)
  let at base = Room.sprite ~base ~size:1.6 ~image:square (Vec.make 8. 0.) in
  let ground = at 0. and lifted = at 1.2 in
  let with_ground, without = alone [ ground ] in
  let with_lifted, _ = alone [ lifted ] in
  let gl, gt, gr, gb = box ~with_it:with_ground ~without (looking_east ()) in
  let ll, lt, lr, lb = box ~with_it:with_lifted ~without (looking_east ()) in
  Alcotest.(check (pair int int)) "the same columns" (gl, gr) (ll, lr);
  Alcotest.(check bool) "the same height" true (abs (gb - gt - (lb - lt)) <= 1);
  (* And by how much: 1.2 cells at distance 8, in pixels. *)
  let v = viewport ~floor_z:0. in
  let rise =
    Viewport.project_height v ~z:0. ~distance:8.
    -. Viewport.project_height v ~z:1.2 ~distance:8.
  in
  Alcotest.(check bool)
    (Printf.sprintf "raised by %d rows, expected %.1f" (gb - lb) rise)
    true
    (Float.abs (float_of_int (gb - lb) -. rise) <= 1.5)

(* The vertical rule, from the other end: an elevated sprite's foot sits at
   floor + base, so a sprite lifted clear of the eye is drawn entirely above the
   horizon — which is where the level view looks. *)
let a_sprite_high_enough_is_all_above_the_horizon () =
  let s = Room.sprite ~base:2. ~size:0.6 ~image:square (Vec.make 5. 0.) in
  let with_it, without = alone [ s ] in
  let _, _, _, bottom = box ~with_it ~without (looking_east ()) in
  Alcotest.(check bool)
    (Printf.sprintf "its lowest row is %d, the horizon is %d" bottom (height / 2))
    true
    (bottom < height / 2)

(* A billboard is as wide as its picture says. Two sprites of the same size,
   one drawn from a square image and one from an image twice as wide as it is
   tall, come out the same height and one twice the width of the other. *)
let a_wide_picture_makes_a_wide_billboard () =
  let pos = Vec.make 5. 0. in
  let with_square, without = alone [ Room.sprite ~size:1.6 ~image:square pos ]
  and with_wide, _ = alone [ Room.sprite ~size:1.6 ~image:wide pos ] in
  let sl, st, sr, sb = box ~with_it:with_square ~without (looking_east ()) in
  let wl, wt, wr, wb = box ~with_it:with_wide ~without (looking_east ()) in
  Alcotest.(check bool) "the same height" true (abs (sb - st - (wb - wt)) <= 1);
  Alcotest.(check bool)
    (Printf.sprintf "twice as wide: %d against %d" (wr - wl) (sr - sl))
    true
    (abs (wr - wl - (2 * (sr - sl))) <= 3);
  Alcotest.(check bool)
    "and still centred where the square one was" true
    (abs (wl + wr - (sl + sr)) <= 2)

(* A wall standing between the eye and a sprite hides the part of it behind,
   per pixel and not per sprite. The wall here is shorter than the sprite is
   high, so the top survives and the bottom does not. *)
let a_low_wall_cuts_the_bottom_off () =
  let s = Room.sprite ~size:2. ~image:square (Vec.make 8. 0.) in
  let clear_with, clear_without = alone [ s ] in
  let wall =
    [ Room.wall ~height:0.9 ~material:dim (Vec.make 4. (-2.)) (Vec.make 4. 2.) ]
  in
  let hidden_with, hidden_without = alone ~extra:wall [ s ] in
  let _, ct, _, cb =
    box ~with_it:clear_with ~without:clear_without (looking_east ())
  in
  let _, ht, _, hb =
    box ~with_it:hidden_with ~without:hidden_without (looking_east ())
  in
  Alcotest.(check int) "its top is untouched" ct ht;
  Alcotest.(check bool)
    (Printf.sprintf "its bottom rose from %d to %d" cb hb)
    true (hb < cb);
  Alcotest.(check bool) "but some of it is still there" true (hb > ht)

(* Raise the same sprite over the same wall and more of it survives: the wall
   is where it was, so the higher the sprite the less of it is behind. *)
let lifting_it_over_the_wall_reveals_more () =
  let at base = Room.sprite ~base ~size:2. ~image:square (Vec.make 8. 0.) in
  let wall =
    [ Room.wall ~height:0.9 ~material:dim (Vec.make 4. (-2.)) (Vec.make 4. 2.) ]
  in
  let low_with, without = alone ~extra:wall [ at 0. ]
  and high_with, _ = alone ~extra:wall [ at 1.5 ] in
  let _, lt, _, lb = box ~with_it:low_with ~without (looking_east ()) in
  let _, ht, _, hb = box ~with_it:high_with ~without (looking_east ()) in
  Alcotest.(check bool)
    (Printf.sprintf "more rows survive: %d against %d" (hb - ht) (lb - lt))
    true
    (hb - ht > lb - lt)

(* A sprite in the room beyond a doorway is trimmed to the opening, column by
   column, and not to a rectangle around it: nothing of it lands outside the
   doorway's own columns. *)
let a_sprite_through_a_doorway_is_trimmed_to_the_opening () =
  (* {!Support.two_rooms} is two 4 x 4 rooms joined by a doorway one cell wide,
     with the transform between them a translation by (-4, 0). So a sprite at
     (6, 2) of the second room's frame stands four cells straight ahead of a
     player at (2, 2) of the first — and, two and a half cells across, is wider
     than the opening it is seen through. *)
  let looking = Player.make ~room:0 ~pos:centre ~angle:0. in
  let sprite = Room.sprite ~size:2.5 ~image:square (Vec.make 2. 2.) in
  let second = World.room two_rooms 1 in
  let with_it =
    World.replace_room two_rooms ~room:1
      ~replacement:(Room.with_sprites second [ sprite ])
  in
  let l, _, r, _ = box ~with_it ~without:two_rooms looking in
  (* Where the opening is: the columns in which shutting the door changes the
     picture. Everything else in the frame is this room's own walls, which a
     leaf two cells away cannot touch. *)
  let a = Framebuffer.offscreen ~width ~height
  and b = Framebuffer.offscreen ~width ~height in
  Renderer.draw_frame a two_rooms looking;
  Renderer.draw_frame b two_rooms_closed looking;
  let first = ref max_int and last = ref min_int in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      if Framebuffer.pixel a ~x ~y <> Framebuffer.pixel b ~x ~y then begin
        first := Int.min !first x;
        last := Int.max !last x
      end
    done
  done;
  Alcotest.(check bool)
    (Printf.sprintf "the doorway is columns %d..%d" !first !last)
    true (!first < !last);
  Alcotest.(check bool)
    (Printf.sprintf "the sprite is drawn in %d..%d, inside it" l r)
    true
    (l >= !first && r <= !last);
  (* And it is genuinely trimmed rather than merely small: unclipped it would be
     wider than the opening it came through. *)
  let unclipped =
    let { Viewport.left = el; right = er; _ } =
      Viewport.sprite_box (viewport ~floor_z:0.) looking ~floor_z:0.
        ~distance:4. sprite
    in
    er -. el
  in
  Alcotest.(check bool)
    (Printf.sprintf "unclipped it would be %.0f columns wide, not %d" unclipped
       (r - l + 1))
    true
    (unclipped > float_of_int (!last - !first + 1))

(* {1 Doorways you can see through}

   A leaf or a lintel of a material that carries an alpha is a wall you can see
   through, and the room beyond has to be drawn behind it or its clear texels
   show this room's own floor. Both cases below are the same claim from two
   directions: something that is only in the far room reaches the screen when
   what stands between is see-through, and reaches nothing when it is solid. *)

(* {!Support.joined_rooms}' second room with a roof of a given material over it
   instead of the sky it is authored with. The roof is the one thing varied
   between the two frames of every case below that asks about the far room's
   ceiling, so a pixel that differs is a pixel that came from inside it. *)
let roofed world material =
  let before = World.room world 1 in
  World.replace_room world ~room:1
    ~replacement:
      (Room.make
         ~thresholds:
           (List.init (Room.threshold_count before) (Room.threshold_at before))
         ~floor:(Room.floor_surface before)
         ~ceiling:(Room.Roof { Room.plane = Plane.horizontal 3.; material })
         (List.init (Room.wall_count before) (Room.wall_at before)))

(* Through the bars of a shut grille door, a sprite standing in the next room.
   Behind a solid leaf the same sprite is not drawn at all — the recursion never
   happens, which is the whole of why a closed door is cheap. *)
let a_see_through_leaf_shows_the_room_behind () =
  let looking = Player.make ~room:0 ~pos:centre ~angle:0. in
  let sprite = Room.sprite ~size:2.5 ~image:square (Vec.make 2. 2.) in
  let peopled world =
    World.replace_room world ~room:1
      ~replacement:(Room.with_sprites (World.room world 1) [ sprite ])
  in
  let reaches world = drawn ~with_it:(peopled world) ~without:world looking in
  Alcotest.(check bool)
    "through the bars, the sprite is drawn" true
    (reaches two_rooms_barred <> None);
  Alcotest.(check bool)
    "behind a solid leaf, none of it is" true
    (reaches two_rooms_closed = None)

(* The strip of wall over an opening, the same way. The door below is solid and
   shut, so the opening itself shows nothing of the far room; the only way in is
   the transom. What is varied is that room's roof and nothing else, so a pixel
   that differs between the two frames is a pixel that came from inside it. *)
let a_see_through_lintel_shows_the_room_behind () =
  (* Pitched up, because the eye is half a cell off the floor and the opening is
     two cells tall: from a level view at this range the strip above it is off
     the top of the window entirely, and there is nothing to look through. *)
  let looking =
    Player.pitch_by
      (Player.make ~room:0 ~pos:centre ~angle:0.)
      ~fraction:Config.max_pitch
  in
  let reaches world =
    drawn ~with_it:(roofed world pale) ~without:(roofed world dim) looking
  in
  Alcotest.(check bool)
    "through the glass over the door, the far room's roof is drawn" true
    (reaches (joined_rooms ~door:(Door.make dim) ~lintel:mesh ()) <> None);
  Alcotest.(check bool)
    "under a solid lintel, none of it is" true
    (reaches (joined_rooms ~door:(Door.make dim) ()) = None)

(* And with no lintel at all, still none of it. Omitting one says the opening
   already reaches the top of the wall it was cut into, not that the gap runs on
   above it: the rows over the head of a two-cell opening in a three-cell room
   are that room's own ceiling, exactly as they are over a wall that stops short
   of it — {!Renderer} caps a wall at the ceiling and paints the ceiling above.
   Recursing through them instead would put the neighbour's roof, or its sky,
   overhead in this room. *)
let a_bare_opening_does_not_show_the_room_above_it () =
  let looking =
    Player.pitch_by
      (Player.make ~room:0 ~pos:centre ~angle:0.)
      ~fraction:Config.max_pitch
  in
  let reaches world =
    drawn ~with_it:(roofed world pale) ~without:(roofed world dim) looking
  in
  Alcotest.(check bool)
    "over a lintel-less opening, the far room's roof is not drawn" true
    (reaches (joined_rooms ~door:(Door.make dim) ~bare:true ()) = None)

(* The two cases above are the same claim about the strip over an opening, and
   the roof makes it about the opening itself. Drop the ceiling of the room the
   player is standing in below the doorway's head and those rows are this room's
   own roof, exactly as the rows over a lintel-less opening are: neither the far
   room's ceiling nor its sky may be painted through the gap, however tall the
   gap was cut. The seam is what makes it matter — two rooms whose floors meet
   need not have their roofs meet, and the neighbour's clip in [draw_planes]
   only catches the case where they do.

   Both ways in are checked, because they are two call sites: a transom you can
   see through, and a gap with nothing across it at all. *)
let the_roof_caps_what_an_opening_shows () =
  let looking =
    Player.pitch_by
      (Player.make ~room:0 ~pos:centre ~angle:0.)
      ~fraction:Config.max_pitch
  in
  (* The near room under a roof that hangs below the head of its own two-cell
     opening, and clear over the eye. Only room 0 is touched; [roofed] varies
     room 1 as before, so a pixel that differs still came from inside it. *)
  let low world =
    let before = World.room world 0 in
    World.replace_room world ~room:0
      ~replacement:
        (Room.make
           ~thresholds:
             (List.init
                (Room.threshold_count before)
                (Room.threshold_at before))
           ~floor:(Room.floor_surface before)
           ~ceiling:
             (Room.Roof { Room.plane = Plane.horizontal 1.2; material = dim })
           (List.init (Room.wall_count before) (Room.wall_at before)))
  in
  Alcotest.(check bool)
    "the roof is under the opening's head and over the eye" true
    (Config.eye_height < 1.2 && 1.2 < 2.);
  let reaches world =
    let world = low world in
    drawn ~with_it:(roofed world pale) ~without:(roofed world dim) looking
  in
  Alcotest.(check bool)
    "through a transom under a low roof, the far room's roof is not drawn" true
    (reaches (joined_rooms ~door:(Door.make dim) ~lintel:mesh ()) = None);
  Alcotest.(check bool)
    "and through an open doorway under one, none of it either" true
    (reaches (joined_rooms ()) = None)

(* And where it {e is} drawn, it does not begin at the top of the window. The
   camera carried into the far room sits behind that room's copy of the opening,
   so its roof runs back from there towards the eye and stands, in the rows near
   the top of the strip, nearer than the doorway itself. Those rows are this
   room's own ceiling: the ray meets it long before it gets to the wall the
   transom is set in.

   Standing further back than the case above, which is the reason that one does
   not already catch this: from the middle of the room every row of the strip
   that is on the screen at all already looks past the doorway, and there is
   nothing left for the clip to take. *)
let the_far_rooms_ceiling_begins_where_the_doorway_does () =
  let looking =
    Player.pitch_by
      (Player.make ~room:0 ~pos:(Vec.make 0.3 2.) ~angle:0.)
      ~fraction:Config.max_pitch
  in
  let world = joined_rooms ~door:(Door.make dim) ~lintel:mesh () in
  let _, top, _, _ =
    box ~with_it:(roofed world pale) ~without:(roofed world dim) looking
  in
  (* The doorway is at [x = 4] and the eye at [x = 0.3], so every column meets
     it at the same perpendicular distance; the roof is three cells up. Read off
     {!Viewport} rather than written down, so that a change to the field of view
     moves the expectation with the picture. *)
  let edge =
    Viewport.project_height
      (Viewport.make ~pitch:Config.max_pitch ~eye_z:Config.eye_height ~width
         ~height)
      ~z:3. ~distance:3.7
  in
  Alcotest.(check bool)
    (Printf.sprintf "the far roof reaches row %d, and not above row %.0f" top
       edge)
    true
    (float_of_int top >= edge -. 1.)

(* {1 What a doorway does not show}

   The recursion is entered with the camera carried into the neighbour's frame,
   which puts it behind that room's own copy of the opening — so a ray cast there
   passes through whatever the neighbour has standing on {e this} side of its own
   doorway before it reaches the doorway at all. In a convex room there is never
   anything there. In one that folds back on itself there is, and it is space the
   player is standing in rather than anything the doorway can show. *)

(* {!Support.recessed} sets the second room's doorway in the back of a blind
   slot, and [blind:false] takes that slot's back wall away and changes nothing
   else. Along every ray through the opening that wall stands a cell and a half
   off, half a cell nearer than the doorway — so the two worlds have to be drawn
   identically, pixel for pixel.

   Which is a stronger claim than any box: not that the wall is hidden, or
   trimmed, or drawn dim, but that it is not in this picture at all. Untrimmed it
   fills the whole of the doorway's columns, being the first thing every one of
   those rays meets. *)
let what_stands_in_front_of_a_doorway_is_not_drawn_through_it () =
  let looking = Player.make ~room:0 ~pos:centre ~angle:0. in
  Alcotest.(check bool)
    "the wall in front of the doorway reaches no pixel" true
    (drawn ~with_it:(recessed ()) ~without:(recessed ~blind:false ()) looking
    = None)

(* A sprite stands on the floor wherever the floor has got to. Over a plane that
   climbs east, the same sprite at the same place is drawn higher than it is
   over a level one, by what the plane says the ground has risen. *)
let a_sloped_floor_carries_it () =
  let s = Room.sprite ~size:1.6 ~image:square (Vec.make 5. 0.) in
  let slope = Plane.make ~a:0.2 ~b:0. ~c:0. in
  let level_with, level_without = alone [ s ] in
  let slope_with, slope_without = alone ~floor:slope [ s ] in
  let _, _, _, level =
    box ~with_it:level_with ~without:level_without (looking_east ())
  in
  let _, _, _, sloped =
    box ~with_it:slope_with ~without:slope_without (looking_east ())
  in
  (* The eye rises with the floor under the player, at x = 0, where the plane is
     still 0; the ground under the sprite, at x = 5, has risen a whole cell. *)
  let v = viewport ~floor_z:0. in
  let rise =
    Viewport.project_height v ~z:0. ~distance:5.
    -. Viewport.project_height v
         ~z:(Plane.elevation slope (Vec.make 5. 0.))
         ~distance:5.
  in
  Alcotest.(check bool)
    (Printf.sprintf "its foot rose %d rows, expected %.1f" (level - sloped) rise)
    true
    (Float.abs (float_of_int (level - sloped) -. rise) <= 1.5)

(* Sprites are composited farthest first, so a near one covers a far one rather
   than showing through it. Both are solid, so where they overlap the near one
   is the only thing that can be seen — and the frame with both in it is the
   frame with just the near one, over the far one's remaining pixels. *)
let a_near_sprite_covers_a_far_one () =
  let near = Room.sprite ~size:1.6 ~image:square (Vec.make 3. 0.)
  and far = Room.sprite ~size:1.6 ~image:square (Vec.make 6. 0.) in
  let both, _ = alone [ far; near ] in
  let just_near, without = alone [ near ] in
  let a = Framebuffer.offscreen ~width ~height
  and b = Framebuffer.offscreen ~width ~height in
  Renderer.draw_frame a both (looking_east ());
  Renderer.draw_frame b just_near (looking_east ());
  ignore (box ~with_it:just_near ~without (looking_east ()));
  let differs = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if Framebuffer.pixel a ~x ~y <> Framebuffer.pixel b ~x ~y then
        incr differs
    done
  done;
  Alcotest.(check int)
    "nothing of the far sprite shows anywhere in the frame" 0 !differs

(* Behind the player is not on the screen. Nothing is drawn, and nothing is
   read out of range in the attempt. *)
let a_sprite_behind_the_player_is_not_drawn () =
  let s = Room.sprite ~size:1.6 ~image:square (Vec.make (-3.) 0.) in
  let with_it, without = alone [ s ] in
  Alcotest.(check bool)
    "no pixel differs" true
    (drawn ~with_it ~without (looking_east ()) = None)

(* {1 What distance fades into}

   Fog is a blend towards the air's own {!Atmosphere.haze} and not a multiply
   towards black, and in every fixture above the two are indistinguishable
   because the haze is nearly black already. The air below is deliberately not:
   its haze is brighter than anything in the room and a different hue, so the
   direction of the fade is unambiguous. Under a multiply a receding surface can
   only lose on every channel; under the blend it moves towards the haze, which
   here means {e gaining} on two of them. The first three below fail against a
   multiply, one for each of the three fog sites.

   The fourth is here for the opposite mistake, and is the only test that
   catches it: fog blends and orientation multiplies, and folding the
   orientation into the blend as well passes all three of the others. *)

let vivid =
  Atmosphere.make ~haze:(Color.rgb 40 220 255) ~fog_distance:12.
    ~min_brightness:0.05 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6
    ~directional:0.4 ()

(** The hall again, under air you can see the colour of. *)
let hazy ?floor ?ceiling ?extra sprites =
  World.make
    ~rooms:[ ("hall", hall ?floor ?ceiling ?extra sprites) ]
    ~links:[] ~atmosphere:vivid
    ~spawn:("hall", Vec.make 0. 0.)

let shot world player =
  let fb = Framebuffer.offscreen ~width ~height in
  Renderer.draw_frame fb world player;
  fb

(** How far a colour is from the haze, summed over the three channels: [0] is
    the haze itself, and 765 is as far from it as an 8-bit colour gets. One
    number, because the claim being made is about a direction and not about any
    one channel. *)
let from_the_haze (c : Color.t) =
  let h = vivid.Atmosphere.haze in
  abs (c.Color.r - h.Color.r)
  + abs (c.Color.g - h.Color.g)
  + abs (c.Color.b - h.Color.b)

(* A wall far away is not a dark wall. It is a wall with a lot of air in front
   of it, and what it moves towards as it recedes is the colour of that air — so
   at the reach of the fade there is almost nothing of the wall left in the
   pixel. Multiplying towards black gets the {e brightness} of this right in a
   world whose haze is nearly black, and the colour wrong in every other one. *)
let a_distant_wall_fades_into_the_haze_and_not_into_the_dark () =
  let world = hazy [] in
  let at away =
    Framebuffer.pixel
      (shot world (looking_east ~pos:(Vec.make (12. -. away) 0.) ()))
      ~x:(width / 2) ~y:(height / 2)
  in
  let near = at 2. and far = at 12. in
  Alcotest.(check bool)
    (Printf.sprintf "the far wall is nearer the haze: %d against %d"
       (from_the_haze far) (from_the_haze near))
    true
    (from_the_haze far < from_the_haze near);
  Alcotest.(check bool)
    (Printf.sprintf "and at the reach of the fade it is nearly the haze: %d"
       (from_the_haze far))
    true
    (from_the_haze far < 30);
  (* And the direction of it, on one channel: this air is bluer than the wall,
     so the far wall's blue is {e higher} than the near one's. No factor
     multiplying a channel can raise it. *)
  Alcotest.(check bool)
    (Printf.sprintf "its blue rose with distance: %d against %d" far.Color.b
       near.Color.b)
    true
    (far.Color.b > near.Color.b)

(* The same claim over a distance that changes with every row. A column of floor
   is the whole fade in one strip — a cell away at the bottom of the screen and
   most of the room away near the horizon — so the fade has to arrive at the
   haze there too. It is the same haze that fills the band {e at} the horizon,
   where the eye looks past both planes, and a floor fading to black instead
   would meet that band at a seam. *)
let the_floor_fades_into_the_haze_towards_the_horizon () =
  let fb = shot (hazy []) (looking_east ()) in
  let at y = Framebuffer.pixel fb ~x:(width / 2) ~y in
  (* Both rows are below the east wall's foot, which at twelve cells lands a few
     rows under the horizon: row 60 is floor several cells out, row 95 is floor
     at your feet. *)
  let far = at 60 and near = at 95 in
  Alcotest.(check bool)
    (Printf.sprintf "floor near the horizon is nearer the haze: %d against %d"
       (from_the_haze far) (from_the_haze near))
    true
    (from_the_haze far < from_the_haze near);
  Alcotest.(check bool)
    (Printf.sprintf "and its blue rose with distance: %d against %d" far.Color.b
       near.Color.b)
    true
    (far.Color.b > near.Color.b)

(* A sprite is a billboard at one distance, so the air in front of it is one
   colour and the whole picture moves towards the haze together. The same grey
   picture twice, near and far, read through its middle. *)
let a_distant_sprite_fades_into_the_haze () =
  let grey =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 180 180 180, 255))
  in
  let at away =
    let s = Room.sprite ~size:1.6 ~image:grey (Vec.make away 0.) in
    Framebuffer.pixel
      (shot (hazy [ s ]) (looking_east ()))
      ~x:(width / 2) ~y:(height / 2)
  in
  let near = at 2. and far = at 11. in
  Alcotest.(check bool)
    (Printf.sprintf "the far sprite is nearer the haze: %d against %d"
       (from_the_haze far) (from_the_haze near))
    true
    (from_the_haze far < from_the_haze near);
  Alcotest.(check bool)
    (Printf.sprintf "and its blue rose with distance: %d against %d" far.Color.b
       near.Color.b)
    true
    (far.Color.b > near.Color.b)

(* And the other thing that darkens a surface, which for a long time reached
   every wall and no sprite: the room's light. A billboard has no normal, so
   what lights it is {!Atmosphere.t.ambient} — the model's own name for what a
   surface facing away from the light gets — and nothing about which way it is
   turned.

   Two rooms differing in nothing but their ambient, with a wall read in the
   same frame to compare the sprite against. The wall is what makes this about
   the room rather than about the picture: both have to come down, and by the
   same factor, or a sprite is once again the one thing in the world the light
   does not reach. [directional] is nothing, so every wall reads [ambient] too
   and the two are directly comparable; the air barely fades over this distance,
   so what moves is the light and not the fog. *)
let a_sprite_is_lit_by_the_room_it_stands_in () =
  let grey =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 180 180 180, 255))
  in
  let read ~ambient =
    let world =
      World.with_atmosphere
        (fst (alone [ Room.sprite ~size:1.6 ~image:grey (Vec.make 4. 0.) ]))
        (Atmosphere.make ~haze:(Color.rgb 20 20 28) ~fog_distance:400.
           ~min_brightness:0.05 ~light:(Vec.make (-0.4) (-0.9)) ~ambient
           ~directional:0. ())
    in
    let fb = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame fb world (looking_east ());
    (* The sprite through the middle, and the east wall down a column the
       billboard is nowhere near. *)
    ( (Framebuffer.pixel fb ~x:(width / 2) ~y:(height / 2)).Color.r,
      (Framebuffer.pixel fb ~x:4 ~y:(height / 2)).Color.r )
  in
  let bright_sprite, bright_wall = read ~ambient:1. in
  let dim_sprite, dim_wall = read ~ambient:0.2 in
  Alcotest.(check bool)
    (Printf.sprintf "the wall went dark with the room: %d to %d" bright_wall
       dim_wall)
    true
    (dim_wall < bright_wall / 2);
  Alcotest.(check bool)
    (Printf.sprintf "and so did the sprite: %d to %d" bright_sprite dim_sprite)
    true
    (dim_sprite < bright_sprite / 2);
  (* By the same factor, which is the whole claim: one light over the room and
     not a dimming of the billboard's own. A ratio, the two surfaces being
     different colours, and a twentieth of slack, the readings being bytes. *)
  let ratio a b = float_of_int a /. float_of_int b in
  Alcotest.(check bool)
    (Printf.sprintf "by the room's factor and not one of its own: %.3f to %.3f"
       (ratio dim_sprite bright_sprite)
       (ratio dim_wall bright_wall))
    true
    (Float.abs (ratio dim_sprite bright_sprite -. ratio dim_wall bright_wall)
    < 0.05)

(* {!Room.sprite_light} is {!Room.decal_light} for billboards, so a sprite has
   the way out of the dark a mark on a wall has, and these two are the sprite
   spelling of the pair under "marks on walls". A lamp is what wants it:
   something the room's light does not account for, drawn in the colours it
   holds however dark the room has become. *)
let a_glowing_sprite_keeps_its_own_light () =
  let white =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  let lit ~glow ~ambient =
    let world =
      World.with_atmosphere
        (fst
           (alone [ Room.sprite ~glow ~size:1.6 ~image:white (Vec.make 4. 0.) ]))
        (Atmosphere.make ~haze:(Color.rgb 20 20 28) ~fog_distance:400.
           ~min_brightness:0.05 ~light:(Vec.make (-0.4) (-0.9)) ~ambient
           ~directional:0. ())
    in
    let fb = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame fb world (looking_east ());
    (Framebuffer.pixel fb ~x:(width / 2) ~y:(height / 2)).Color.r
  in
  let plain_lit = lit ~glow:0. ~ambient:1.
  and plain_dark = lit ~glow:0. ~ambient:0.2 in
  Alcotest.(check bool)
    (Printf.sprintf "an ordinary one goes down with the room: %d to %d"
       plain_lit plain_dark)
    true
    (plain_dark < plain_lit / 2);
  let glowing_lit = lit ~glow:1. ~ambient:1.
  and glowing_dark = lit ~glow:1. ~ambient:0.2 in
  Alcotest.(check int)
    "a fully glowing one does not move at all" glowing_lit glowing_dark;
  Alcotest.(check bool)
    (Printf.sprintf "and is the picture's own white: %d" glowing_lit)
    true (glowing_lit > 250);
  let half = lit ~glow:0.5 ~ambient:0.2 in
  Alcotest.(check bool)
    (Printf.sprintf "half glow sits between: %d < %d < %d" plain_dark half
       glowing_dark)
    true
    (plain_dark < half && half < glowing_dark)

(* And out of the haze with it, for the reason a decal is: a sprite making all
   of its own light and still wearing the air's colour would not be its own
   colour at all, and at the far end of a long room it would be the haze. *)
let a_glowing_sprite_takes_none_of_the_haze () =
  let white =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  let at ~glow away =
    let s = Room.sprite ~glow ~size:1.6 ~image:white (Vec.make away 0.) in
    Framebuffer.pixel
      (shot (hazy [ s ]) (looking_east ()))
      ~x:(width / 2) ~y:(height / 2)
  in
  (* [vivid]'s haze is a bright cyan, so paint drifting into it moves a long way
     and in a direction no amount of dimming could produce. *)
  let paint_near = at ~glow:0. 2. and paint_far = at ~glow:0. 11. in
  Alcotest.(check bool)
    (Printf.sprintf "paint takes the air: %d then %d" (from_the_haze paint_near)
       (from_the_haze paint_far))
    true
    (from_the_haze paint_far < from_the_haze paint_near);
  let glowing_near = at ~glow:1. 2. and glowing_far = at ~glow:1. 11. in
  Alcotest.(check bool)
    (Printf.sprintf "a glowing one does not: %d then %d"
       (from_the_haze glowing_near)
       (from_the_haze glowing_far))
    true
    (glowing_near = glowing_far);
  Alcotest.(check bool)
    (Printf.sprintf "and is still the white it was painted: #%02x%02x%02x"
       glowing_far.Color.r glowing_far.Color.g glowing_far.Color.b)
    true
    (glowing_far.Color.r > 250 && glowing_far.Color.b > 250)

(* The other half of the rule, and the half a fade towards the haze makes it
   easy to get wrong: orientation is a multiply and distance is a blend, and the
   two have to stay apart. A wall turned away from the light has to go
   {e dark}. Fold the shading into the blend instead and it goes {e hazy} — and
   with air brighter than the wall, that means the face turned away from the
   light coming out brighter than the face turned into it, which is the shading
   inverted.

   {!Support.room} is a square with the eye at its centre, so all four walls are
   the same material at the same distance and differ only in which way they
   face. Two of them, on every channel. *)
let orientation_dims_a_wall_rather_than_fogging_it () =
  let square_room =
    World.make
      ~rooms:[ ("room", room) ]
      ~links:[] ~atmosphere:vivid ~spawn:("room", centre)
  in
  let facing angle =
    Framebuffer.pixel
      (shot square_room (Player.make ~room:0 ~pos:centre ~angle))
      ~x:(width / 2) ~y:(height / 2)
  in
  (* South is nearly square-on to the light, east nearly edge-on to it. *)
  let into = facing (-.Float.pi /. 2.) and away = facing 0. in
  Alcotest.(check bool)
    (Printf.sprintf
       "square-on to the light is brighter on every channel: #%02x%02x%02x \
        against #%02x%02x%02x"
       into.Color.r into.Color.g into.Color.b away.Color.r away.Color.g
       away.Color.b)
    true
    (into.Color.r > away.Color.r
    && into.Color.g > away.Color.g
    && into.Color.b > away.Color.b);
  (* And brighter by the same amount on all three, which is the sharp form of
     it: the two walls stand at one distance, so the air's share of them is one
     colour and cancels, and the whole of the difference is the multiply. *)
  let dr = into.Color.r - away.Color.r
  and dg = into.Color.g - away.Color.g
  and db = into.Color.b - away.Color.b in
  Alcotest.(check (pair int int))
    (Printf.sprintf "and by the same amount on each: %d, %d, %d" dr dg db)
    (dr, dr) (dg, db)

(* {1 Decals}

   A mark is on one face of a wall. Everything below is that claim, on the
   pixels: a see-through partition standing in the hall, so that both of its
   faces can be looked at without walking through anything, with a mark on one
   of them. *)

let mark = Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 0 255 255, 255))

(* Standing at (5, -2) to (5, 2): the edge runs +y, so the normal — a quarter
   turn to its left — points at -x, and the origin is at its Front. *)
let partition decals =
  [
    Room.wall ~height:2.5 ~material:mesh ~decals (Vec.make 5. (-2.))
      (Vec.make 5. 2.);
  ]

let marked facing =
  fst
    (alone
       ~extra:
         (partition
            [
              Room.decal ~facing ~along:2. ~z:0.6 ~half_width:0.5
                ~half_height:0.5 mark;
            ])
       [])

let bare = fst (alone ~extra:(partition []) [])
let in_front () = looking_east ()
let behind () = Player.make ~room:0 ~pos:(Vec.make 10. 0.) ~angle:Float.pi

let a_decal_is_drawn_on_the_face_it_is_on () =
  Alcotest.(check bool)
    "a Front mark is there from the front" true
    (drawn ~with_it:(marked Room.Front) ~without:bare (in_front ()) <> None);
  Alcotest.(check bool)
    "and nowhere at all from behind" true
    (drawn ~with_it:(marked Room.Front) ~without:bare (behind ()) = None)

(* The other way round, so the test cannot pass by never drawing anything. The
   partition is see-through, so from behind there is a wall to draw the mark on
   and the only thing stopping it is the face rule. *)
let a_decal_on_the_far_face_is_drawn_from_behind () =
  Alcotest.(check bool)
    "a Back mark is there from behind" true
    (drawn ~with_it:(marked Room.Back) ~without:bare (behind ()) <> None);
  Alcotest.(check bool)
    "and nowhere at all from the front" true
    (drawn ~with_it:(marked Room.Back) ~without:bare (in_front ()) = None)

(* Which way round it is drawn, which the two above say nothing about — they ask
   whether a mark is there and not what it looks like, and a mirrored picture is
   as present as an unmirrored one.

   [along] runs from the wall's [a] to its [b], and by {!Room}'s winding rule
   that walk goes left to right on screen for someone at the Front and right to
   left for someone at the Back. So the offset alone names a column of the
   picture only from one side. Left unmirrored, every mark on a far face is
   drawn reversed — invisible on the demos' poster, which is a ring in a border,
   and unmissable on anything with writing in it.

   The mark here has a hand: its left half red and its right half green. Which
   colour lands on the left of the screen is the whole of the test, and it is
   asked of both faces, because the claim is not that the two agree with each
   other but that both agree with the picture as it was authored. *)
let handed =
  Image.make ~width:8 (fun ~u ~v:_ ->
      if u < 4 then (Color.rgb 255 0 0, 255) else (Color.rgb 0 255 0, 255))

let a_mark_is_drawn_the_way_it_was_authored_on_either_face () =
  let hung facing =
    fst
      (alone
         ~extra:
           (partition
              [
                Room.decal ~facing ~along:2. ~z:0.6 ~half_width:0.5
                  ~half_height:0.5 handed;
              ])
         [])
  in
  (* Every column the mark changed, split by which half of its palette arrived
     there. Read off the buffer and not off [Room], so this is what a player
     sees rather than what the placement says. *)
  let halves ~with_it player =
    let a = Framebuffer.offscreen ~width ~height
    and b = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame a with_it player;
    Renderer.draw_frame b bare player;
    let left = ref [] and right = ref [] in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let p = Framebuffer.pixel a ~x ~y in
        if p <> Framebuffer.pixel b ~x ~y then
          if p.Color.r > p.Color.g then left := x :: !left
          else if p.Color.g > p.Color.r then right := x :: !right
      done
    done;
    let span l =
      match List.sort compare l with
      | [] -> Alcotest.fail "half of the mark reached no pixel at all"
      | l -> (List.hd l, List.nth l (List.length l - 1))
    in
    (span !left, span !right)
  in
  List.iter
    (fun (name, facing, player) ->
      let (l0, l1), (r0, r1) = halves ~with_it:(hung facing) player in
      Alcotest.(check bool)
        (Printf.sprintf
           "%s: the picture's left half is on the left of the screen — red at \
            %d..%d, green at %d..%d"
           name l0 l1 r0 r1)
        true (l1 < r0))
    [
      ("a Front mark from the front", Room.Front, in_front ());
      ("a Back mark from behind", Room.Back, behind ());
    ]

(* And the round-trip, on the pixels this time: aim at a wall, put a mark where
   Sight says the crosshair is, and it lands on the crosshair — the middle of
   the screen, which is where {!Viewport.ray_direction} sends the centre column
   and where {!Paint.crosshair} draws.

   The mark is small — a fifth of a cell either way — so a box around it that
   still holds the centre pixel is a tight claim about the numbers, not a
   bounding box that would hold anything.

   Run at an odd size as well as an even one, and that is the point of the
   second: {!Renderer.internal_size} divides the window down by a whole number,
   so a 1366- or 2560-wide screen produces an odd buffer, and it is there that a
   convention disagreeing with itself about where a pixel is shows up. *)
let a_mark_lands_under_the_crosshair () =
  let world = fst (alone []) in
  let aim = looking_east () in
  match Sight.look world aim with
  | Some { Sight.kind = Sight.Wall w; room; _ } ->
      let with_it =
        World.replace_room world ~room
          ~replacement:
            (Room.add_decal (World.room world room) ~wall:w.index
               (Room.decal ~facing:w.facing ~along:w.along ~z:w.z
                  ~half_width:0.2 ~half_height:0.2 mark))
      in
      List.iter
        (fun (width, height) ->
          let l, t, r, b = box ~width ~height ~with_it ~without:world aim in
          let cx = width / 2 and cy = height / 2 in
          Alcotest.(check bool)
            (Printf.sprintf
               "%dx%d: the crosshair (%d, %d) is inside %d..%d by %d..%d" width
               height cx cy l r t b)
            true
            (l <= cx && cx <= r && t <= cy && cy <= b);
          (* And it is a small mark on a far wall, not half the screen. *)
          Alcotest.(check bool)
            (Printf.sprintf "%dx%d: and it is small: %d by %d" width height
               (r - l + 1)
               (b - t + 1))
            true
            (r - l < width / 4 && b - t < height / 4))
        [ (160, 100); (161, 101) ]
  | _ -> Alcotest.fail "expected the wall ahead"

(* The other half of that round trip, and the one a see-through wall can fail:
   what {!Sight} says the crosshair is on has to be what was drawn there, texel
   by texel and not material by material. A grille is where the two can part
   company, because its alpha changes from one texel to the next — the renderer
   samples the one the column lands on, so a picker judging the whole material
   names whatever is behind a bar the picture shows no way through.

   Whether the wall covered the crosshair is read off the frame rather than
   asserted about. Draw the hall with a sprite standing behind the screen and
   again without it, and look at the middle pixel: unchanged means the screen
   covered it, changed means the sprite showed through. That measurement goes
   through none of Sight's arithmetic, which is what makes this a comparison
   between the two modules rather than a restatement of one of them.

   The screen is placed twice, a tenth of a cell apart, so that one crossing
   falls on a bar and the other in a hole — and neither module is told which is
   which. An odd buffer, because there the middle pixel's own ray is exactly
   [player.dir], the ray Sight traces; at an even size the two straddle the
   middle by half a pixel and this would be a test about that instead. *)
let a_grille_is_picked_where_it_is_drawn () =
  let odd = 161 and tall = 101 in
  (* Four cells of grille across the hall at x = 4, and a sprite four cells
     further on for it to hide or not. Sliding the near end moves where along
     the screen the level ray crosses it, which is the only difference between
     the two placements. *)
  let screen y0 =
    [
      Room.wall ~height:3. ~material:mesh (Vec.make 4. y0)
        (Vec.make 4. (y0 +. 4.));
    ]
  in
  let behind = [ Room.sprite ~size:1.5 ~image:square (Vec.make 8. 0.) ] in
  let showed_through y0 =
    let with_it, without = alone ~extra:(screen y0) behind in
    let a = Framebuffer.offscreen ~width:odd ~height:tall
    and b = Framebuffer.offscreen ~width:odd ~height:tall in
    Renderer.draw_frame a with_it (looking_east ());
    Renderer.draw_frame b without (looking_east ());
    let at fb = Framebuffer.pixel fb ~x:(odd / 2) ~y:(tall / 2) in
    at a <> at b
  in
  let picked y0 =
    match
      Sight.look (fst (alone ~extra:(screen y0) behind)) (looking_east ())
    with
    | Some { Sight.kind = Sight.Wall _; _ } -> "wall"
    | Some { Sight.kind = Sight.Sprite _; _ } -> "sprite"
    | other ->
        Alcotest.failf "expected the screen or the sprite, got %s"
          (match other with None -> "nothing" | Some _ -> "a doorway")
  in
  (* The two placements answer differently, or the fixture is not aimed at what
     this is about and everything below would pass by agreeing on one case. *)
  Alcotest.(check bool)
    "the two placements land on different texels" true
    (showed_through (-2.) <> showed_through (-2.1));
  List.iter
    (fun y0 ->
      let seen = showed_through y0 in
      Alcotest.(check string)
        (Printf.sprintf
           "a screen from y = %g: the sprite %s through it, so the crosshair \
            is on the"
           y0
           (if seen then "shows" else "does not show"))
        (if seen then "sprite" else "wall")
        (picked y0))
    [ -2.; -2.1 ]

(* What the buffer's size promises, over every window shape worth having and a
   good many that are not.

   The shape is the part worth pinning, because it is the part the interface
   used to overstate. Both axes divide by one whole number, so the buffer is the
   window at a clean pixel multiple — and then each floors, so the ratio comes
   out a little off wherever that number does not go into both. What is asserted
   is the bound those two truncations imply rather than a figure somebody
   measured: dividing [w] by [s] loses less than one whole pixel, so the ratio
   moves by less than one part in each of the buffer's own extents. A test
   written against a constant would be a test that a particular window is no
   worse than it happens to be. *)
let the_buffer_is_the_window_at_a_whole_number_of_pixels () =
  let shapes =
    List.concat_map
      (fun w ->
        List.map (fun h -> (w, h)) [ 240; 400; 480; 481; 769; 1080; 1441; 2160 ])
      [ 320; 401; 640; 1001; 1366; 1367; 1920; 2560; 3840 ]
  in
  List.iter
    (fun (w, h) ->
      let bw, bh = Renderer.internal_size ~width:w ~height:h in
      let said = Printf.sprintf "%dx%d -> %dx%d" w h bw bh in
      Alcotest.(check bool)
        (said ^ ": both extents are real")
        true
        (bw >= 1 && bh >= 1);
      Alcotest.(check bool)
        (said ^ ": no bigger than the window it is shown in")
        true
        (bw <= w && bh <= h);
      (* Within the cap, or the window was already inside it and is used whole.
         The second half is what says the cap is a cap and not a target. *)
      Alcotest.(check bool)
        (Printf.sprintf "%s: inside the cap of %d" said Config.max_render_height)
        true
        (bh <= Config.max_render_height || (bw, bh) = (w, h));
      if h <= Config.max_render_height then
        Alcotest.(check (pair int int))
          (said ^ ": a window already small enough is left alone")
          (w, h) (bw, bh);
      let want = float_of_int w /. float_of_int h
      and got = float_of_int bw /. float_of_int bh in
      let bound = (1. /. float_of_int bw) +. (1. /. float_of_int bh) in
      Alcotest.(check bool)
        (Printf.sprintf
           "%s: shape out by %.4f%%, and the two floors allow %.4f%%" said
           (Float.abs (got -. want) /. want *. 100.)
           (bound *. 100.))
        true
        (Float.abs (got -. want) /. want < bound))
    shapes

(* The corner a jamb shares with the threshold beside it, aimed at squarely.
   {!Ray.segment} takes [s] in a closed interval at both ends and has to: a
   room's corners are shared between two walls, and the pair does not come out
   as an exact one and zero but as one and a hair below zero, so a half-open
   test lets a ray out through the corner of a closed room. What the overlap
   costs is that a ray through such a corner meets both segments at one
   distance, and the tie then has to be settled the same way by both readers of
   that list — the renderer, which paints along it and shows the last, and
   {!Sight}, which scans it and reports one.

   Constructible rather than a matter of luck: the wall runs from (4, -3) to
   (4, 4), so {!Room.doorway} centres a one-cell opening over [0, 1] and the
   lower jamb ends at exactly (4, 0). At an odd width the middle column's ray is
   exactly [player.dir], so a player at the origin looking east aims down the
   axis and straight through that corner.

   The picture is the oracle, as it is for the grille above: what the frame
   shows in that column is read off the frame, by drawing it with the sprite and
   without and comparing the one pixel, and {!Sight} has to name what was
   shown. *)
let the_corner_of_a_doorway_is_picked_where_it_is_drawn () =
  let odd = 161 and tall = 101 in
  let looking = Player.make ~room:0 ~pos:(Vec.make 0. 0.) ~angle:0. in
  let world ~beyond =
    let jambs, threshold =
      Room.doorway ~name:"east" ~width:1. ~opening:2. ~height:3. ~material:pale
        (Vec.make 4. (-3.)) (Vec.make 4. 4.)
    in
    let here =
      Room.make ~thresholds:[ threshold ] ~floor:flat_floor
        ~ceiling:flat_ceiling
        (jambs
        @ [
            Room.wall ~height:3. ~material:pale (Vec.make (-4.) (-3.))
              (Vec.make 4. (-3.));
            Room.wall ~height:3. ~material:pale (Vec.make 4. 4.)
              (Vec.make (-4.) 4.);
            Room.wall ~height:3. ~material:pale (Vec.make (-4.) 4.)
              (Vec.make (-4.) (-3.));
          ])
    in
    let next =
      let jambs, threshold =
        Room.doorway ~name:"west" ~width:1. ~opening:2. ~height:3. ~material:dim
          (Vec.make 0. 4.) (Vec.make 0. (-3.))
      in
      Room.make ~thresholds:[ threshold ] ~floor:flat_floor
        ~ceiling:flat_ceiling ~sprites:beyond
        (jambs
        @ [
            Room.wall ~height:3. ~material:dim (Vec.make 0. (-3.))
              (Vec.make 8. (-3.));
            Room.wall ~height:3. ~material:dim (Vec.make 8. (-3.))
              (Vec.make 8. 4.);
            Room.wall ~height:3. ~material:dim (Vec.make 8. 4.) (Vec.make 0. 4.);
          ])
    in
    World.make
      ~rooms:[ ("here", here); ("beyond", next) ]
      ~links:[ (("here", "east"), ("beyond", "west")) ]
      ~atmosphere:air
      ~spawn:("here", Vec.make 0. 0.)
  in
  let sprite = [ Room.sprite ~size:1.6 ~image:square (Vec.make 4. 0.5) ] in
  let with_it = world ~beyond:sprite and without = world ~beyond:[] in
  let at w =
    let fb = Framebuffer.offscreen ~width:odd ~height:tall in
    Renderer.draw_frame fb w looking;
    Framebuffer.pixel fb ~x:(odd / 2) ~y:(tall / 2)
  in
  (* The fixture is aimed at what this is about, or everything below agrees by
     the column showing the jamb and there being no tie to settle. *)
  Alcotest.(check bool)
    "the middle column shows the room through the opening" true
    (at with_it <> at without);
  match Sight.look with_it looking with
  | Some { Sight.kind = Sight.Sprite _; room; _ } ->
      Alcotest.(check int)
        "and the crosshair is on it, in the room beyond" 1 room
  | other ->
      Alcotest.failf
        "the frame shows the sprite through the corner and the crosshair says \
         %s"
        (match other with
        | None -> "nothing"
        | Some { Sight.kind = Sight.Wall w; room; _ } ->
            Printf.sprintf "wall %d of room %d" w.index room
        | Some { Sight.kind = Sight.Doorway d; room; _ } ->
            Printf.sprintf "doorway %d of room %d" d.index room
        | Some _ -> "a sprite somewhere else")

(* A line of [n] rooms, each the same 4 x 4 square in its own coordinates,
   joined by bare openings a cell wide in the middle of the wall they share, and
   [sprites] standing in the last of them. Standing in the first at (2, 2)
   looking due east puts every opening — and everything beyond them — on one
   line.

   Not {!Support.joined_rooms}, which is a pair: the whole of what this is for is
   a chain longer than the renderer will follow to the end of. *)
let chain n sprites =
  let room i =
    let east_jambs, east =
      Room.doorway ~name:"east" ~width:1. ~opening:2. ~height:3. ~material:pale
        (Vec.make 4. 0.) (Vec.make 4. 4.)
    and west_jambs, west =
      Room.doorway ~name:"west" ~width:1. ~opening:2. ~height:3. ~material:dim
        (Vec.make 0. 4.) (Vec.make 0. 0.)
    in
    let first = i = 0 and last = i = n - 1 in
    Room.make
      ~thresholds:
        ((if last then [] else [ east ]) @ if first then [] else [ west ])
      ~floor:flat_floor ~ceiling:flat_ceiling
      ~sprites:(if last then sprites else [])
      ((if last then
          [
            Room.wall ~height:3. ~material:pale (Vec.make 4. 0.)
              (Vec.make 4. 4.);
          ]
        else east_jambs)
      @ (if first then
           [
             Room.wall ~height:3. ~material:dim (Vec.make 0. 4.)
               (Vec.make 0. 0.);
           ]
         else west_jambs)
      @ [
          Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
        ])
  in
  World.make
    ~rooms:(List.init n (fun i -> (string_of_int i, room i)))
    ~links:
      (List.init (n - 1) (fun i ->
           ((string_of_int i, "east"), (string_of_int (i + 1), "west"))))
    ~atmosphere:air
    ~spawn:("0", Vec.make 2. 2.)

(* The third of these round trips, and the one a line of doorways can fail:
   {!Sight} may not stop looking before the picture does. The renderer follows
   {!Config.max_portal_depth} doorways in a row, so a ray given any fewer names
   the doorway the player is looking straight {e through} while a sprite two
   rooms on fills the middle of their screen.

   Neither side is told that number. The picture is asked whether the sprite
   reached the crosshair — the with-it-and-without measurement the rest of this
   file is built on, taken at the one pixel — and {!Sight} is asked what it found
   there, and the two have to give the same answer at every length of chain. The
   sweep runs past the budget on purpose: out there the opening is filled with
   haze, and the answer they have to agree on is that neither can see that far.

   An odd buffer, for the reason the grille above wants one: there the middle
   pixel's own ray is exactly [player.dir], the ray Sight traces. *)
let a_room_is_picked_as_far_in_as_it_is_drawn () =
  let odd = 161 and tall = 101 in
  let looking = Player.make ~room:0 ~pos:(Vec.make 2. 2.) ~angle:0. in
  let sprite = [ Room.sprite ~size:1.6 ~image:square (Vec.make 2. 2.) ] in
  let answers =
    List.map
      (fun n ->
        let with_it = chain n sprite and without = chain n [] in
        let at world =
          let fb = Framebuffer.offscreen ~width:odd ~height:tall in
          Renderer.draw_frame fb world looking;
          Framebuffer.pixel fb ~x:(odd / 2) ~y:(tall / 2)
        in
        let drawn = at with_it <> at without
        and picked =
          match Sight.look with_it looking with
          | Some { Sight.kind = Sight.Sprite _; room; _ } -> room = n - 1
          | _ -> false
        in
        (n, drawn, picked))
      (List.init 5 (fun i -> i + 2))
  in
  (* The sweep has to cross the budget, or every case below agrees by never
     reaching the far room at all. *)
  Alcotest.(check bool)
    "the far room is reached at some lengths of chain and not others" true
    (List.exists (fun (_, drawn, _) -> drawn) answers
    && List.exists (fun (_, drawn, _) -> not drawn) answers);
  List.iter
    (fun (n, drawn, picked) ->
      Alcotest.(check bool)
        (Printf.sprintf
           "%d rooms: the sprite %s under the crosshair, so Sight %s name it" n
           (if drawn then "is drawn" else "is not drawn")
           (if drawn then "has to" else "cannot"))
        drawn picked)
    answers

(* The wall's own share of the light, which nothing else here pins. A pattern's
   texel is what the surface {e is}, and the air is the only thing between that
   and the screen — so if the light stopped arriving, every wall in the game
   would come out at full pattern colour with no depth in it whatever, and the
   two tests below would go on passing, because they are about decals.

   The same wall at two distances, head on both times, so the face shading is
   identical and cancels. What is left is the ratio of the two fog factors —
   once the air's own colour is taken back out of both readings, because fog
   fades a surface {e towards the haze} and not towards black, so a far reading
   is part surface and part air. *)
let a_wall_is_lit_by_the_air_it_is_seen_through () =
  let world = fst (alone []) in
  let lit away =
    let fb = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame fb world
      (looking_east ~pos:(Vec.make (12. -. away) 0.) ());
    (Framebuffer.pixel fb ~x:(width / 2) ~y:(height / 2)).Color.r
  in
  (* Near enough that both readings are well clear of the bottom of the byte:
     the whole assertion is a ratio, and a ratio of two small integers is mostly
     rounding. *)
  let near = lit 2. and far = lit 6. in
  Alcotest.(check bool)
    (Printf.sprintf "a wall near is brighter than far: %d against %d" near far)
    true (near > far);
  Alcotest.(check bool) "and neither is black" true (far > 0);
  (* Not a plain ratio any more: each reading is part surface and part air. Take
     the air back out — it is a known colour — and the ratio is there exactly. A
     reading is [texel * face_shading * fog + haze * (1 - fog)], so subtracting
     the haze leaves [fog * (texel * face_shading - haze)], and that bracket is
     the same wall at both distances. *)
  let haze = air.Atmosphere.haze.Color.r in
  let expected = Atmosphere.fog air 2. /. Atmosphere.fog air 6. in
  let got = float_of_int (near - haze) /. float_of_int (far - haze) in
  Alcotest.(check bool)
    (Printf.sprintf
       "by the fog factor, with the air taken out: %.3f against %.3f" got
       expected)
    true
    (Float.abs (got -. expected) < 0.05);
  (* And the scale of it, which no ratio can reach: a light that was uniformly
     wrong would keep every ratio in the game intact and darken all of it. The
     wall is a flat pattern, so the one texel it has — shaded by how squarely
     this face meets the light, then carried towards the haze by how far away it
     is — is the whole prediction.

     Note where the fog is and is not. It is the {e amount} of the blend and not
     part of the shade: [Color.shade texel (face_shading *. fog)] followed by a
     blend would apply it twice. *)
  let wall = Room.wall_at (World.room world 0) 1 in
  let texel = Texture.sample wall.Room.material.Material.pattern ~u:0 ~v:0 in
  let predicted =
    (Color.lerp
       (Color.shade texel (Atmosphere.face_shading air wall.Room.normal))
       air.Atmosphere.haze
       (1. -. Atmosphere.fog air 2.))
      .Color.r
  in
  Alcotest.(check bool)
    (Printf.sprintf
       "and the near reading is that texel under that light, through that much \
        air: %d against %d"
       near predicted)
    true
    (abs (near - predicted) <= 2)

(* A decal is lit by the same one factor the wall under it is: orientation and
   fog. It is what the wall is {e made of} that does not reach it — a poster
   on a red wall is not red — and the two are easy to confuse, so this pins the
   half that does.

   A white picture at two distances, in air that fades over twelve cells. The
   face shading is the same for both (same wall, same normal), so it cancels,
   and what is left is the ratio of the two fog factors — once the air's own
   colour is taken back out of both readings, exactly as for the wall above. *)
let a_decal_is_fogged_like_the_wall_it_is_on () =
  let white =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  let world = fst (alone []) in
  (* The hall's east wall is index 1, running from (12, -4); looking east from
     the axis hits it four along, at eye height. *)
  let marked =
    World.replace_room world ~room:0
      ~replacement:
        (Room.add_decal (World.room world 0) ~wall:1
           (Room.decal ~along:4. ~z:Config.eye_height ~half_width:1.
              ~half_height:1. white))
  in
  let lit away =
    let fb = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame fb marked
      (looking_east ~pos:(Vec.make (12. -. away) 0.) ());
    (Framebuffer.pixel fb ~x:(width / 2) ~y:(height / 2)).Color.r
  in
  let near = lit 4. and far = lit 12. in
  Alcotest.(check bool)
    (Printf.sprintf "a decal near is brighter than far: %d against %d" near far)
    true (near > far);
  Alcotest.(check bool) "and neither is black" true (far > 0);
  let haze = air.Atmosphere.haze.Color.r in
  let expected = Atmosphere.fog air 4. /. Atmosphere.fog air 12. in
  let got = float_of_int (near - haze) /. float_of_int (far - haze) in
  Alcotest.(check bool)
    (Printf.sprintf
       "by the fog factor, with the air taken out: %.3f against %.3f" got
       expected)
    true
    (Float.abs (got -. expected) < 0.05)

(* The other half of the same fact, and the one it is easy to mistake for the
   first: what the wall itself is made of does not reach the decal. Two rooms
   differing only in how dark that surface is draw the same picture on the wall
   and a different wall behind it.

   This is why a poster on a red wall is not red. It is also why a game that
   dims a room by repainting its surfaces will find its chalk marks standing
   out more as it does: the wall goes down twice — its own colour and the fog —
   where the mark goes down once. *)
let a_decal_ignores_what_the_wall_is_made_of () =
  let white =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  let hung =
    Room.decal ~along:4. ~z:Config.eye_height ~half_width:1. ~half_height:1.
      white
  in
  let world_of shade =
    let coat =
      Material.make
        ~pattern:
          (Texture.generate (fun ~u:_ ~v:_ -> Color.rgb shade shade shade))
    in
    let room =
      Room.make
        ~floor:{ Room.plane = Plane.horizontal 0.; material = pale }
        ~ceiling:
          (Room.Roof { Room.plane = Plane.horizontal 3.; material = dim })
        [
          Room.wall ~height:3. ~material:pale (Vec.make (-4.) (-4.))
            (Vec.make 12. (-4.));
          Room.wall ~height:3. ~material:coat ~decals:[ hung ]
            (Vec.make 12. (-4.)) (Vec.make 12. 4.);
          Room.wall ~height:3. ~material:pale (Vec.make 12. 4.)
            (Vec.make (-4.) 4.);
          Room.wall ~height:3. ~material:pale (Vec.make (-4.) 4.)
            (Vec.make (-4.) (-4.));
        ]
    in
    World.make
      ~rooms:[ ("hall", room) ]
      ~links:[] ~atmosphere:air
      ~spawn:("hall", Vec.make 0. 0.)
  in
  let frame world =
    let fb = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame fb world (looking_east ());
    fb
  in
  let bright = frame (world_of 240) and dark = frame (world_of 60) in
  Alcotest.check color "the mark is the same on a dark wall as on a pale one"
    (Framebuffer.pixel bright ~x:(width / 2) ~y:(height / 2))
    (Framebuffer.pixel dark ~x:(width / 2) ~y:(height / 2));
  (* And the wall above it, which the same colour does reach, is not. *)
  Alcotest.(check bool)
    "though the wall it is on is darker" true
    (Framebuffer.pixel bright ~x:(width / 2) ~y:30
    <> Framebuffer.pixel dark ~x:(width / 2) ~y:30)

(* [glow] is how a mark stays readable in a room whose light has gone. The two
   tests above are the default: a decal takes the room's light, and a lamp made
   out of the atmosphere therefore takes the mark down with everything else.
   This is the way out of that, and it is per decal.

   The same white picture at the same place, in air that fades over forty
   cells and in air that fades over six, once as paint and once glowing. *)
let close_air ~fog_distance =
  Atmosphere.make ~haze:(Color.rgb 20 20 28) ~fog_distance ~min_brightness:0.05
    ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6 ~directional:0.4 ()

let a_glowing_decal_keeps_its_own_light () =
  let white =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  let lit ~glow ~fog_distance =
    let world = fst (alone []) in
    let marked =
      World.replace_room world ~room:0
        ~replacement:
          (Room.add_decal (World.room world 0) ~wall:1
             (Room.decal ~glow ~along:4. ~z:Config.eye_height ~half_width:1.
                ~half_height:1. white))
    in
    let marked = World.with_atmosphere marked (close_air ~fog_distance) in
    let fb = Framebuffer.offscreen ~width ~height in
    Renderer.draw_frame fb marked (looking_east ());
    (Framebuffer.pixel fb ~x:(width / 2) ~y:(height / 2)).Color.r
  in
  (* Paint: the failing light takes it with everything else. *)
  let paint_lit = lit ~glow:0. ~fog_distance:40.
  and paint_dark = lit ~glow:0. ~fog_distance:6. in
  Alcotest.(check bool)
    (Printf.sprintf "paint goes down with the room: %d to %d" paint_lit
       paint_dark)
    true
    (paint_dark < paint_lit / 2);
  (* Glowing: it is drawn at its own colours whatever the room has done. *)
  let glowing_lit = lit ~glow:1. ~fog_distance:40.
  and glowing_dark = lit ~glow:1. ~fog_distance:6. in
  Alcotest.(check int)
    "a fully glowing one does not move at all" glowing_lit glowing_dark;
  Alcotest.(check bool)
    (Printf.sprintf "and is the picture's own white: %d" glowing_lit)
    true (glowing_lit > 250);
  (* And it is an interpolation, so halfway is between the two and glow can
     never darken anything. *)
  let half = lit ~glow:0.5 ~fog_distance:6. in
  Alcotest.(check bool)
    (Printf.sprintf "half glow sits between: %d < %d < %d" paint_dark half
       glowing_dark)
    true
    (paint_dark < half && half < glowing_dark);
  Alcotest.(check bool)
    "and glow never darkens" true
    (lit ~glow:0.3 ~fog_distance:40. >= paint_lit)

(* Glow lifts a mark out of the haze as well as out of the dark, and it has to
   be both: a mark that made all of its own light and still had the air's colour
   laid over it would not be its own colour at all — at the far end of a long
   room it would be the haze. {!Room.decal_light} is a fraction raised towards
   [1.] by [glow], and the renderer asks it for the fog factor as well as for
   the light, so a fully glowing mark takes none of the air.

   The picture is a saturated red, and deliberately not the white the test above
   uses: white plus haze saturates back to white, so a white mark cannot tell a
   mark that took no haze from one that took some and clipped. *)
let a_glowing_decal_takes_none_of_the_haze () =
  let red = Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 200 0 0, 255)) in
  let lit ~glow =
    let world = hazy [] in
    let marked =
      World.replace_room world ~room:0
        ~replacement:
          (Room.add_decal (World.room world 0) ~wall:1
             (Room.decal ~glow ~along:4. ~z:Config.eye_height ~half_width:1.
                ~half_height:1. red))
    in
    Framebuffer.pixel
      (shot marked (looking_east ()))
      ~x:(width / 2) ~y:(height / 2)
  in
  let glowing = lit ~glow:1. in
  (* On the blue, because the picture has none: any of this air that reached it
     would show there, and the red only says it was not dimmed on the way. *)
  Alcotest.(check int)
    "a fully glowing mark takes none of the air's colour" 0 glowing.Color.b;
  Alcotest.(check bool)
    (Printf.sprintf "and is the red it was painted: %d" glowing.Color.r)
    true (glowing.Color.r >= 199);
  (* And paint at the same place is most of the way into that air, so the claim
     above is not that the air is doing nothing here. *)
  let paint = lit ~glow:0. in
  Alcotest.(check bool)
    (Printf.sprintf "where paint at the same place is %d from the haze"
       (from_the_haze paint))
    true
    (from_the_haze paint < 40)

(* {1 Every pixel of a frame}

   Nothing clears the colour buffer. [Framebuffer.clear_depth] resets the depth
   and there is deliberately no companion for the colour, because the background
   pass is meant to cover every pixel of every column before anything reads one —
   so a pixel it skips does not go black, it keeps whatever the {e previous}
   frame put there.

   No test above could have caught one. They all compare two freshly allocated
   buffers, a fresh buffer is zeroed, and a pixel neither frame writes reads back
   as black in both and diffs away to nothing.

   So draw the same frame twice: once into a buffer seeded with a colour, once
   into a fresh one. Every pixel the frame writes lands the same in both; a pixel
   it skips keeps the seed in one and the black in the other. No colour can hide
   a gap or invent one, because what is being compared is two runs of the same
   drawing and not a colour anybody predicted. *)
let unwritten world player =
  let seeded = Framebuffer.offscreen ~width ~height
  and fresh = Framebuffer.offscreen ~width ~height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Framebuffer.set seeded ~x ~y ~r:255 ~g:0 ~b:255
    done
  done;
  Renderer.draw_frame seeded world player;
  Renderer.draw_frame fresh world player;
  let count = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if Framebuffer.pixel seeded ~x ~y <> Framebuffer.pixel fresh ~x ~y then
        incr count
    done
  done;
  !count

(* A roof lower than the eye is authorable: [Plane.above] takes any height and
   neither [Room.roof] nor [Room.make] has an opinion about it. Such a ceiling
   casts to a {e negative} distance — behind the eye, and so no surface at all —
   and above the horizon the floor is not in view either, which makes the band
   the haze's. Asking whether the raw cast was finite rather than whether it
   pointed forwards called that a plane the doorway had clipped, left the pixels
   alone, and what they showed was the frame before. *)
let a_roof_below_the_eye_leaves_nothing_behind () =
  let low = Room.Roof { Room.plane = Plane.horizontal 0.4; material = dim } in
  Alcotest.(check bool)
    "the roof really is under the eye" true (0.4 < Config.eye_height);
  let world = hazy ~ceiling:low [] in
  List.iter
    (fun (name, pitch) ->
      Alcotest.(check int)
        (name ^ ": nothing is left of the frame before")
        0
        (unwritten world (Player.pitch_by (looking_east ()) ~fraction:pitch)))
    [
      ("level", 0.);
      ("looking up", Config.max_pitch);
      ("looking down", -.Config.max_pitch);
    ]

(* And the contract itself, over the ordinary fixtures: a roofed room, a room
   open to the sky, and a frame drawn through a doorway into both. *)
let every_pixel_of_a_frame_is_written () =
  let tipped pitch world =
    (world, Player.pitch_by (Player.spawn world) ~fraction:pitch)
  in
  List.iter
    (fun (name, (world, player)) ->
      Alcotest.(check int)
        (name ^ ": every pixel is written")
        0 (unwritten world player))
    [
      ("the hall", (hazy [], looking_east ()));
      ("looking up at the roof", tipped Config.max_pitch (hazy []));
      ("looking down at the floor", tipped (-.Config.max_pitch) (hazy []));
      ("through a doorway", tipped 0. two_rooms);
      ("through a doorway, looking up", tipped Config.max_pitch two_rooms);
      ( "a sloped floor",
        tipped 0. (hazy ~floor:(Plane.make ~a:0.1 ~b:0. ~c:0.) []) );
    ]

(* {1 Where a surface stops}

   One claim, in the three places the renderer turns a projected extent into
   pixels. A pixel belongs to a surface when its {e centre} falls on it, and an
   extent is half-open at the far end: a wall runs down to its foot, where the
   floor takes over, so the pixel the foot lands in is the floor's.

   For a wall this is not a rounding error. The per-pixel sampler works from the
   row's centre rather than from the loop's bounds, so an extra row asks
   [Texture.row_of_height] for a height {e below} the wall's own foot — and that
   tiles rather than clamps, so what comes back is the top row of the pattern. A
   one-pixel line of the tile's top band along the foot of every wall, leaf and
   lintel strip on screen, over a row of floor, with a row of the wall's depth
   written into it. *)

(* The pixels an extent covers, worked out from the definition rather than
   asked of {!Viewport}: a pixel is covered when its own centre falls in
   [\[a, b)], and one whose centre does not is not, however much of it the
   extent overlaps. The range is clipped to the [n] pixels there are, which is
   what the renderer's own bounds do and what makes this comparable with them.

   Written out here on purpose. Asking [Viewport.first_pixel] and
   [Viewport.last_pixel] what they thought would agree with the renderer however
   the pair of them moved, which is a test of nothing. *)
let covered ~n a b =
  let inside i =
    let centre = float_of_int i +. 0.5 in
    centre >= a && centre < b
  in
  let first = ref (-1) and last = ref (-1) in
  for i = n - 1 downto 0 do
    if inside i then first := i
  done;
  for i = 0 to n - 1 do
    if inside i then last := i
  done;
  (!first, !last)

(* A pattern whose rows say which end of the tile they came from: its top band
   red and everything below blue. Nothing else in these fixtures is red, and the
   two survive fog and shading as an inequality — the haze is neither. *)
let banded =
  Material.make
    ~pattern:
      (Texture.generate (fun ~u:_ ~v ->
           if v < 8 then Color.rgb 255 0 0 else Color.rgb 0 0 255))

(* The hall with its far wall banded. The other three are behind the player or
   edge-on to the centre column, so what that column meets is this wall, the
   floor under it, and nothing else. *)
let banded_hall =
  let floor = Plane.horizontal 0. in
  Room.make
    ~floor:{ Room.plane = floor; material = pale }
    ~ceiling:(Room.Roof { Room.plane = Plane.above floor 3.; material = dim })
    [
      Room.wall ~height:3. ~material:pale (Vec.make (-4.) (-4.))
        (Vec.make 12. (-4.));
      Room.wall ~height:3. ~material:banded (Vec.make 12. (-4.))
        (Vec.make 12. 4.);
      Room.wall ~height:3. ~material:pale (Vec.make 12. 4.) (Vec.make (-4.) 4.);
      Room.wall ~height:3. ~material:pale (Vec.make (-4.) 4.)
        (Vec.make (-4.) (-4.));
    ]

let banded_world =
  World.make
    ~rooms:[ ("hall", banded_hall) ]
    ~links:[] ~atmosphere:air
    ~spawn:("hall", Vec.make 0. 0.)

(* The depth buffer is the oracle for "a wall painted this pixel":
   [Renderer.draw_frame] clears it to infinity and only an opaque wall writes to
   it, so the rows it holds a distance in are exactly the rows of the strip.

   Swept along the hall so the foot lands at a spread of fractional rows — the
   bug is there at every one of them, but only a sweep says so. *)
let a_wall_stops_where_the_floor_starts () =
  let fb = Framebuffer.offscreen ~width ~height in
  let v = viewport ~floor_z:0. in
  let column = width / 2 in
  List.iter
    (fun x ->
      let player = looking_east ~pos:(Vec.make x 0.) () in
      Renderer.draw_frame fb banded_world player;
      let distance = 12. -. x in
      let y_foot = Viewport.project_height v ~z:0. ~distance
      and y_top = Viewport.project_height v ~z:3. ~distance in
      let painted row =
        fb.Framebuffer.depth.((row * width) + column) < infinity
      in
      let highest = ref (-1) and lowest = ref (-1) in
      for row = height - 1 downto 0 do
        if painted row then highest := row
      done;
      for row = 0 to height - 1 do
        if painted row then lowest := row
      done;
      let name =
        Printf.sprintf "%.2f cells away, the strip is [%.3f, %.3f)" distance
          y_top y_foot
      in
      Alcotest.(check (pair int int))
        (name ^ ": the rows whose centres are on the wall")
        (covered ~n:height y_top y_foot)
        (!highest, !lowest);
      let c = Framebuffer.pixel fb ~x:column ~y:!lowest in
      Alcotest.(check bool)
        (Printf.sprintf "%s: sampled at the bottom of the tile, not the top"
           name)
        true (c.Color.b > c.Color.r))
    [ 3.; 4.13; 5.27; 6.41; 7.55; 8.69; 9.83 ]

(* The same edge on a doorway, which is the other place a projected foot becomes
   a row. What a doorway hands to the room behind it — and to the haze that
   stands in for one that is not built yet — is bounded by the same strip the
   leaf across it is drawn on, so the three cannot come apart by a row.

   [World.open_doorway] is the reachable case and says so itself: between it and
   the [link] that fills the portal, the doorway "is solid and shows as haze".
   The fill is written flat, with no fog in it, so an exact match against the
   haze finds the fill and nothing else — the air here is thin enough that no
   surface in the room has faded all the way into it. *)
let thin_air =
  Atmosphere.make ~haze:(Color.rgb 255 0 255) ~fog_distance:60.
    ~min_brightness:0.25 ~ambient:0.6 ~directional:0.4 ()

let onto_nothing =
  let floor = Plane.horizontal 0. in
  let surfaces = { Room.plane = floor; material = pale } in
  let ceiling =
    Room.Roof { Room.plane = Plane.above floor 3.; material = dim }
  in
  let walls =
    [
      Room.wall ~height:3. ~material:pale (Vec.make (-4.) (-4.))
        (Vec.make 12. (-4.));
      Room.wall ~height:3. ~material:pale (Vec.make 12. 4.) (Vec.make (-4.) 4.);
      Room.wall ~height:3. ~material:pale (Vec.make (-4.) 4.)
        (Vec.make (-4.) (-4.));
    ]
  in
  let shut =
    Room.make ~floor:surfaces ~ceiling
      (Room.wall ~height:3. ~material:pale (Vec.make 12. (-4.))
         (Vec.make 12. 4.)
      :: walls)
  in
  let jambs, gap =
    Room.doorway ~name:"onward" ~width:1.5 ~opening:2.2 ~height:3.
      ~material:pale (Vec.make 12. (-4.)) (Vec.make 12. 4.)
  in
  let opened =
    Room.make ~thresholds:[ gap ] ~floor:surfaces ~ceiling (jambs @ walls)
  in
  World.open_doorway
    (World.make
       ~rooms:[ ("hall", shut) ]
       ~links:[] ~atmosphere:thin_air
       ~spawn:("hall", Vec.make 0. 0.))
    ~room:0 ~opened

let a_doorway_onto_nothing_stops_where_the_floor_starts () =
  let fb = Framebuffer.offscreen ~width ~height in
  let v = viewport ~floor_z:0. in
  let column = width / 2 in
  let haze = (World.atmosphere onto_nothing).Atmosphere.haze in
  List.iter
    (fun x ->
      let player = looking_east ~pos:(Vec.make x 0.) () in
      Renderer.draw_frame fb onto_nothing player;
      let distance = 12. -. x in
      let y_foot = Viewport.project_height v ~z:0. ~distance
      and y_head = Viewport.project_height v ~z:2.2 ~distance in
      let filled row = Framebuffer.pixel fb ~x:column ~y:row = haze in
      let highest = ref (-1) and lowest = ref (-1) in
      for row = height - 1 downto 0 do
        if filled row then highest := row
      done;
      for row = 0 to height - 1 do
        if filled row then lowest := row
      done;
      let name =
        Printf.sprintf "%.2f cells away, the opening is [%.3f, %.3f)" distance
          y_head y_foot
      in
      Alcotest.(check (pair int int))
        (name ^ ": the rows whose centres are in the opening")
        (covered ~n:height y_head y_foot)
        (!highest, !lowest))
    [ 3.; 4.13; 5.27; 6.41; 7.55 ]

(* And a billboard, which is the same rule in both directions at once: its right
   column and its bottom row are one short of rounding, the same as a wall's
   foot. Compared against [Viewport.sprite_box] exactly rather than within a
   pixel, because a pixel is the whole of what this is about.

   Swept in both x and y so that each of the four edges crosses a pixel centre
   over the run — one placement would only say that one rounding came out right.
*)
let a_billboard_covers_the_pixels_its_box_holds () =
  let v = viewport ~floor_z:0. in
  List.iter
    (fun i ->
      let pos =
        Vec.make (5. +. (float_of_int i *. 0.037)) (float_of_int i *. 0.011)
      in
      let s = Room.sprite ~size:1.6 ~image:square pos in
      let with_it, without = alone [ s ] in
      let player = looking_east () in
      let drawn = box ~with_it ~without player in
      let { Viewport.left = el; top = et; right = er; bottom = eb } =
        Viewport.sprite_box v player ~floor_z:0. ~distance:pos.Vec.x s
      in
      let l, r = covered ~n:width el er and t, b = covered ~n:height et eb in
      Alcotest.(check (list int))
        (Printf.sprintf "at (%.3f, %.3f), the box is (%.3f, %.3f, %.3f, %.3f)"
           pos.Vec.x pos.Vec.y el et er eb)
        [ l; t; r; b ]
        (let l, t, r, b = drawn in
         [ l; t; r; b ]))
    (List.init 12 Fun.id)

let () =
  Alcotest.run "Renderer"
    [
      ( "where a billboard lands",
        [
          case "a sprite on the floor is where the viewport says"
            a_sprite_on_the_floor_is_where_the_viewport_says;
          case "a base lifts it, and does nothing else"
            a_base_lifts_it_and_does_nothing_else;
          case "high enough, it is all above the horizon"
            a_sprite_high_enough_is_all_above_the_horizon;
          case "a wide picture makes a wide billboard"
            a_wide_picture_makes_a_wide_billboard;
          case "a sloped floor carries it" a_sloped_floor_carries_it;
          case "a sprite nearer than the clip is not drawn"
            a_sprite_nearer_than_the_clip_is_not_drawn;
        ] );
      ( "what hides it",
        [
          case "a low wall cuts the bottom off" a_low_wall_cuts_the_bottom_off;
          case "lifting it over the wall reveals more"
            lifting_it_over_the_wall_reveals_more;
          case "a near sprite covers a far one" a_near_sprite_covers_a_far_one;
          case "a sprite behind the player is not drawn"
            a_sprite_behind_the_player_is_not_drawn;
          case "a sprite through a doorway is trimmed to the opening"
            a_sprite_through_a_doorway_is_trimmed_to_the_opening;
        ] );
      ( "doorways you can see through",
        [
          case "a see-through leaf shows the room behind"
            a_see_through_leaf_shows_the_room_behind;
          case "a see-through lintel shows the room behind"
            a_see_through_lintel_shows_the_room_behind;
          case "a bare opening does not show the room above it"
            a_bare_opening_does_not_show_the_room_above_it;
          case "the roof caps what an opening shows"
            the_roof_caps_what_an_opening_shows;
          case "the far room's ceiling begins where the doorway does"
            the_far_rooms_ceiling_begins_where_the_doorway_does;
        ] );
      ( "what a doorway does not show",
        [
          case "what stands in front of a doorway is not drawn through it"
            what_stands_in_front_of_a_doorway_is_not_drawn_through_it;
        ] );
      ( "light on a wall",
        [
          case "a wall is lit by the air it is seen through"
            a_wall_is_lit_by_the_air_it_is_seen_through;
        ] );
      ( "what distance fades into",
        [
          case "a distant wall fades into the haze and not into the dark"
            a_distant_wall_fades_into_the_haze_and_not_into_the_dark;
          case "the floor fades into the haze towards the horizon"
            the_floor_fades_into_the_haze_towards_the_horizon;
          case "a distant sprite fades into the haze"
            a_distant_sprite_fades_into_the_haze;
          case "a sprite is lit by the room it stands in"
            a_sprite_is_lit_by_the_room_it_stands_in;
          case "a glowing sprite keeps its own light"
            a_glowing_sprite_keeps_its_own_light;
          case "a glowing sprite takes none of the haze"
            a_glowing_sprite_takes_none_of_the_haze;
          case "orientation dims a wall rather than fogging it"
            orientation_dims_a_wall_rather_than_fogging_it;
        ] );
      ( "where a surface stops",
        [
          case "a wall stops where the floor starts"
            a_wall_stops_where_the_floor_starts;
          case "so does a doorway onto nothing"
            a_doorway_onto_nothing_stops_where_the_floor_starts;
          case "a billboard covers the pixels its box holds"
            a_billboard_covers_the_pixels_its_box_holds;
          case "a sprite is picked where it is drawn"
            a_sprite_is_picked_where_it_is_drawn;
        ] );
      ( "the size a frame is drawn at",
        [
          case "the buffer is the window at a whole number of pixels"
            the_buffer_is_the_window_at_a_whole_number_of_pixels;
        ] );
      ( "every pixel of a frame",
        [
          case "a roof below the eye leaves nothing behind"
            a_roof_below_the_eye_leaves_nothing_behind;
          case "every pixel of a frame is written"
            every_pixel_of_a_frame_is_written;
          case "the background is the cast the engine exports"
            the_background_is_the_cast_the_engine_exports;
        ] );
      ( "marks on walls",
        [
          case "a decal is drawn on the face it is on"
            a_decal_is_drawn_on_the_face_it_is_on;
          case "a decal on the far face is drawn from behind"
            a_decal_on_the_far_face_is_drawn_from_behind;
          case "a mark is drawn the way it was authored on either face"
            a_mark_is_drawn_the_way_it_was_authored_on_either_face;
          case "a mark lands under the crosshair"
            a_mark_lands_under_the_crosshair;
          case "a grille is picked where it is drawn"
            a_grille_is_picked_where_it_is_drawn;
          case "a room is picked as far in as it is drawn"
            a_room_is_picked_as_far_in_as_it_is_drawn;
          case "the corner of a doorway is picked where it is drawn"
            the_corner_of_a_doorway_is_picked_where_it_is_drawn;
          case "a decal is fogged like the wall it is on"
            a_decal_is_fogged_like_the_wall_it_is_on;
          case "a decal ignores what the wall is made of"
            a_decal_ignores_what_the_wall_is_made_of;
          case "a glowing decal keeps its own light"
            a_glowing_decal_keeps_its_own_light;
          case "a glowing decal takes none of the haze"
            a_glowing_decal_takes_none_of_the_haze;
        ] );
    ]
