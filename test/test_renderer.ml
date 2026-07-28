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

open Camlcast
open Support

let width = 160
let height = 100

(* A sprite is a picture, and these are the two that matter here: one square and
   one twice as wide as it is tall, both solid to the edges so that the pixels
   that change are the whole billboard and not a cut-out inside it. The colour
   is only ever compared against itself. *)
let solid ?height w =
  Image.make ?height w (fun ~u:_ ~v:_ -> (Color.rgb 255 0 255, 255))

let square = solid 16
let wide = solid ~height:8 16

(** The box a sprite covers on screen: the smallest rectangle holding every
    pixel that differs between the two worlds, as [(left, top, right, bottom)],
    or [None] where the sprite reached nothing at all. *)
let drawn ~with_it ~without player =
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

let box ~with_it ~without player =
  match drawn ~with_it ~without player with
  | Some b -> b
  | None -> Alcotest.fail "the sprite was not drawn at all"

(* One room, one sprite, and the same room without it. Both are built from the
   same parts so that the only difference between the two frames is the
   billboard. [floor] is a plane so that the sloped case is the same fixture. *)
let hall ?(floor = Plane.horizontal 0.) ?(extra = []) sprites =
  Room.make
    ~floor:{ Room.plane = floor; material = pale }
    ~ceiling:(Room.Roof { Room.plane = Plane.above floor 3.; material = dim })
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
let looking_east ?(pos = Vec.make 0. 0.) () =
  Player.create ~room:0 ~pos ~angle:0.

(** The viewport a frame of this size is drawn through, over a floor at
    [floor_z] — what {!Viewport.sprite_box} has to be asked with if its answer
    is to be comparable with what landed on the buffer. *)
let viewport ~floor_z =
  Viewport.create ~pitch:0. ~eye_z:(floor_z +. Config.eye_height) ~width ~height

(* A sprite standing on the floor is where it always was: on the rectangle
   Viewport.sprite_box gives, and — its picture being square — as wide as it is
   tall. This is the case every other demo and every other suite already
   depends on, so it is the one that says the new field changed nothing. *)
let a_sprite_on_the_floor_is_where_the_viewport_says () =
  let s = Room.sprite ~size:1.6 ~image:square (Vec.make 5. 0.) in
  let with_it, without = alone [ s ] in
  let l, t, r, b = box ~with_it ~without (looking_east ()) in
  let el, et, er, eb =
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
  let looking = Player.create ~room:0 ~pos:centre ~angle:0. in
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
    let el, _, er, _ =
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

(* Through the bars of a shut grille door, a sprite standing in the next room.
   Behind a solid leaf the same sprite is not drawn at all — the recursion never
   happens, which is the whole of why a closed door is cheap. *)
let a_see_through_leaf_shows_the_room_behind () =
  let looking = Player.create ~room:0 ~pos:centre ~angle:0. in
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
      (Player.create ~room:0 ~pos:centre ~angle:0.)
      ~radians:Config.max_pitch
  in
  let roofed world material =
    let before = World.room world 1 in
    World.replace_room world ~room:1
      ~replacement:
        (Room.make
           ~thresholds:(Array.to_list before.Room.thresholds)
           ~floor:before.Room.floor
           ~ceiling:(Room.Roof { Room.plane = Plane.horizontal 3.; material })
           (Array.to_list before.Room.walls))
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

(* {1 Decals}

   A mark is on one face of a wall. Everything below is that claim, on the
   pixels: a see-through partition standing in the hall, so that both of its
   faces can be looked at without walking through anything, with a mark on one
   of them. *)

let mark = Image.make 8 (fun ~u:_ ~v:_ -> (Color.rgb 0 255 255, 255))

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
let behind () = Player.create ~room:0 ~pos:(Vec.make 10. 0.) ~angle:Float.pi

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

(* And the round-trip, on the pixels this time: aim at a wall, put a mark where
   Sight says the crosshair is, and it lands on the crosshair — the middle of
   the screen, which is where {!Viewport.ray_direction} sends the centre column
   and where {!Paint.crosshair} draws.

   The mark is small — a fifth of a cell either way — so a box around it that
   still holds the centre pixel is a tight claim about the numbers, not a
   bounding box that would hold anything. *)
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
      let l, t, r, b = box ~with_it ~without:world aim in
      let cx = width / 2 and cy = height / 2 in
      Alcotest.(check bool)
        (Printf.sprintf "the crosshair (%d, %d) is inside %d..%d by %d..%d" cx
           cy l r t b)
        true
        (l <= cx && cx <= r && t <= cy && cy <= b);
      (* And it is a small mark on a far wall, not half the screen. *)
      Alcotest.(check bool)
        (Printf.sprintf "and it is small: %d by %d" (r - l + 1) (b - t + 1))
        true
        (r - l < width / 4 && b - t < height / 4)
  | _ -> Alcotest.fail "expected the wall ahead"

(* The wall's own share of the light, which nothing else here pins. A pattern's
   texel is what the surface {e is}, and the air is the only thing between that
   and the screen — so if the light stopped arriving, every wall in the game
   would come out at full pattern colour with no depth in it whatever, and the
   two tests below would go on passing, because they are about decals.

   The same wall at two distances, head on both times, so the face shading is
   identical and cancels; what is left is the ratio of the two fog factors. *)
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
  let expected = Atmosphere.fog air 2. /. Atmosphere.fog air 6. in
  let got = float_of_int near /. float_of_int far in
  Alcotest.(check bool)
    (Printf.sprintf "by the fog factor: %.3f against %.3f" got expected)
    true
    (Float.abs (got -. expected) < 0.05);
  (* And the scale of it, which no ratio can reach: a light that was uniformly
     wrong would keep every ratio in the game intact and darken all of it. The
     wall is a flat pattern, so the one texel it has, under the light Atmosphere
     reports for this face at this distance, is the whole prediction. *)
  let wall = (World.room world 0).Room.walls.(1) in
  let texel =
    (Texture.sample wall.Room.material.Material.pattern ~u:0 ~v:0).Color.r
  in
  let light =
    Atmosphere.face_shading air wall.Room.normal *. Atmosphere.fog air 2.
  in
  let predicted = int_of_float (float_of_int texel *. light) in
  Alcotest.(check bool)
    (Printf.sprintf
       "and the near reading is that texel under that light: %d against %d" near
       predicted)
    true
    (abs (near - predicted) <= 2)

(* A decal is lit by the same one factor the wall under it is: orientation and
   fog. It is what the wall is {e made of} that does not reach it — a poster
   on a red wall is not red — and the two are easy to confuse, so this pins the
   half that does.

   A white picture at two distances, in air that fades over twelve cells. The
   face shading is the same for both (same wall, same normal), so it cancels and
   what is left is the ratio of the two fog factors, exactly. *)
let a_decal_is_fogged_like_the_wall_it_is_on () =
  let white = Image.make 8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255)) in
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
  let expected = Atmosphere.fog air 4. /. Atmosphere.fog air 12. in
  let got = float_of_int near /. float_of_int far in
  Alcotest.(check bool)
    (Printf.sprintf "by the fog factor: %.3f against %.3f" got expected)
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
  let white = Image.make 8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255)) in
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
    ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6 ~directional:0.4

let a_glowing_decal_keeps_its_own_light () =
  let white = Image.make 8 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255)) in
  let lit ~glow ~fog_distance =
    let world = fst (alone []) in
    let marked =
      World.replace_room world ~room:0
        ~replacement:
          (Room.add_decal (World.room world 0) ~wall:1
             (Room.decal ~glow ~along:4. ~z:Config.eye_height ~half_width:1.
                ~half_height:1. white))
    in
    let marked = { marked with World.atmosphere = close_air ~fog_distance } in
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
        ] );
      ( "light on a wall",
        [
          case "a wall is lit by the air it is seen through"
            a_wall_is_lit_by_the_air_it_is_seen_through;
        ] );
      ( "marks on walls",
        [
          case "a decal is drawn on the face it is on"
            a_decal_is_drawn_on_the_face_it_is_on;
          case "a decal on the far face is drawn from behind"
            a_decal_on_the_far_face_is_drawn_from_behind;
          case "a mark lands under the crosshair"
            a_mark_lands_under_the_crosshair;
          case "a decal is fogged like the wall it is on"
            a_decal_is_fogged_like_the_wall_it_is_on;
          case "a decal ignores what the wall is made of"
            a_decal_ignores_what_the_wall_is_made_of;
          case "a glowing decal keeps its own light"
            a_glowing_decal_keeps_its_own_light;
        ] );
    ]
