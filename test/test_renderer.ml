(** What actually arrives in the framebuffer, one pixel at a time.

    {!Renderer.draw_frame} is documented as pure array writes with no SDL calls
    in it, and {!Framebuffer.offscreen} builds a buffer with no window behind it,
    so a whole frame renders here headlessly. Everything below is about sprites:
    where a billboard lands, what hides it, and what trims it. The rest of the
    renderer is covered through {!Plane}, {!Viewport}, {!Material} and
    {!Atmosphere}, whose arithmetic it is.

    {1 How a sprite is found on the screen}

    Not by looking for its colour. What reaches the buffer has been through fog
    and, behind it, through wall shading, so the number in the pixel is nobody's
    idea of the colour that went in — and asserting on it would be asserting on
    {!Atmosphere}, which has its own suite. Instead every test draws the frame
    {e twice}, once with the sprite and once without, and the sprite is exactly
    the pixels that differ. That answers "where was it drawn" without any claim
    about what it was drawn in. *)

open Raycaster
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
    World.make ~rooms:[ ("hall", room) ] ~links:[] ~atmosphere:air
      ~spawn:("hall", Vec.make 0. 0.)
  in
  (one (hall ?floor ?extra sprites), one (hall ?floor ?extra []))

(** At the origin, looking east down the hall, level. *)
let looking_east ?(pos = Vec.make 0. 0.) () =
  Player.create ~room:0 ~pos ~angle:0.

(** The viewport a frame of this size is drawn through, over a floor at
    [floor_z] — what {!Viewport.sprite_box} has to be asked with if its answer is
    to be comparable with what landed on the buffer. *)
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
    (abs ((r - l) - (b - t)) <= 2)

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
  Alcotest.(check (pair int int))
    "the same columns" (gl, gr) (ll, lr);
  Alcotest.(check bool)
    "the same height" true
    (abs ((gb - gt) - (lb - lt)) <= 1);
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
  Alcotest.(check bool)
    "the same height" true
    (abs ((sb - st) - (wb - wt)) <= 1);
  Alcotest.(check bool)
    (Printf.sprintf "twice as wide: %d against %d" (wr - wl) (sr - sl))
    true
    (abs ((wr - wl) - (2 * (sr - sl))) <= 3);
  Alcotest.(check bool)
    "and still centred where the square one was" true
    (abs ((wl + wr) - (sl + sr)) <= 2)

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
  let _, ct, _, cb = box ~with_it:clear_with ~without:clear_without (looking_east ()) in
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
    true
    (!first < !last);
  Alcotest.(check bool)
    (Printf.sprintf "the sprite is drawn in %d..%d, inside it" l r)
    true
    (l >= !first && r <= !last);
  (* And it is genuinely trimmed rather than merely small: unclipped it would be
     wider than the opening it came through. *)
  let unclipped =
    let el, _, er, _ =
      Viewport.sprite_box (viewport ~floor_z:0.) looking ~floor_z:0. ~distance:4.
        sprite
    in
    er -. el
  in
  Alcotest.(check bool)
    (Printf.sprintf "unclipped it would be %.0f columns wide, not %d" unclipped
       (r - l + 1))
    true
    (unclipped > float_of_int (!last - !first + 1))

(* A sprite stands on the floor wherever the floor has got to. Over a plane that
   climbs east, the same sprite at the same place is drawn higher than it is
   over a level one, by what the plane says the ground has risen. *)
let a_sloped_floor_carries_it () =
  let s = Room.sprite ~size:1.6 ~image:square (Vec.make 5. 0.) in
  let slope = Plane.make ~a:0.2 ~b:0. ~c:0. in
  let level_with, level_without = alone [ s ] in
  let slope_with, slope_without = alone ~floor:slope [ s ] in
  let _, _, _, level = box ~with_it:level_with ~without:level_without (looking_east ()) in
  let _, _, _, sloped =
    box ~with_it:slope_with ~without:slope_without (looking_east ())
  in
  (* The eye rises with the floor under the player, at x = 0, where the plane is
     still 0; the ground under the sprite, at x = 5, has risen a whole cell. *)
  let v = viewport ~floor_z:0. in
  let rise =
    Viewport.project_height v ~z:0. ~distance:5.
    -. Viewport.project_height v ~z:(Plane.elevation slope (Vec.make 5. 0.)) ~distance:5.
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
      if Framebuffer.pixel a ~x ~y <> Framebuffer.pixel b ~x ~y then incr differs
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
    ]
