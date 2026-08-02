(** Drawing over a finished frame, asserted at the pixel.

    These are the first tests in the project to read a framebuffer back.
    {!Framebuffer.offscreen} is what makes them possible: the streaming SDL
    texture was the only part of a buffer that needed a window, so it is the
    only part that is optional, and the pixels and the depth are the same ones
    the renderer writes.

    What is being checked is almost entirely the {e clipping}.
    {!Framebuffer.set} and {!Framebuffer.blend} do not check where they are
    writing — the renderer's loops have clipped long before they call them — so
    every guard against writing off the buffer lives in this module, and a
    missing one is a silently corrupted neighbouring row rather than a crash. *)

open Camlcast_core
open Support

let black = Color.rgb 0 0 0
let white = Color.rgb 255 255 255
let buffer () = Framebuffer.offscreen ~width:10 ~height:8

(* A fresh buffer is black rather than whatever the allocator had lying there,
   which is what lets everything below say what it expected. *)
let a_fresh_buffer_is_black () =
  let fb = buffer () in
  for y = 0 to 7 do
    for x = 0 to 9 do
      Alcotest.check color
        (Printf.sprintf "(%d, %d)" x y)
        black
        (Framebuffer.pixel fb ~x ~y)
    done
  done

(* A buffer is refused where it is allocated rather than where it is written to.
   The record keeps the extents it was asked for while the pixels and the depth
   are only as long as their product came out — and that product is the whole of
   the arithmetic {!Framebuffer.set} does without checking, so a buffer whose
   extents and allocation disagree writes outside its own memory on the first
   call every test in this file makes. Two negatives multiply to a positive
   product, which is how one used to get built.

   {!Framebuffer.make} refuses the same pairs on the same terms, before it asks
   SDL for the texture it would otherwise leave behind, but a live renderer is
   exactly what nothing here has. *)
let a_buffer_of_no_size_is_refused () =
  List.iter
    (fun (width, height) ->
      Alcotest.check_raises (Printf.sprintf "%dx%d" width height)
        (Invalid_argument
           "Framebuffer.offscreen: a buffer must have positive extents")
        (fun () -> ignore (Framebuffer.offscreen ~width ~height)))
    [ (0, 8); (10, 0); (0, 0); (-1, -1); (10, -8) ]

(* Positive extents are not on their own enough: it is [width * height] that the
   depth array is as long as, and past the longest array that product wraps
   instead of growing — [max_int] by [max_int] is [1] — so without the check a
   buffer would come back reporting extents with a single pixel behind them.
   Only the refusing is asserted; the pair just inside the limit is one no
   machine should be asked to allocate to prove a point. *)
let a_buffer_too_big_for_an_array_is_refused () =
  List.iter
    (fun (width, height) ->
      Alcotest.check_raises (Printf.sprintf "%dx%d" width height)
        (Invalid_argument
           "Framebuffer.offscreen: a buffer that size does not fit in an array")
        (fun () -> ignore (Framebuffer.offscreen ~width ~height)))
    [ (max_int, max_int); (8, (Sys.max_floatarray_length / 8) + 1) ]

let a_rectangle_fills_what_it_covers () =
  let fb = buffer () in
  Paint.rect fb ~x:2 ~y:3 ~w:3 ~h:2 ~color:(Color.rgb 255 255 255) ~alpha:255;
  Alcotest.check color "the first pixel" white (Framebuffer.pixel fb ~x:2 ~y:3);
  Alcotest.check color "the last" white (Framebuffer.pixel fb ~x:4 ~y:4);
  Alcotest.check color "one before it is untouched" black
    (Framebuffer.pixel fb ~x:1 ~y:3);
  Alcotest.check color "and one past it" black (Framebuffer.pixel fb ~x:5 ~y:4);
  Alcotest.check color "and the row below" black
    (Framebuffer.pixel fb ~x:2 ~y:5)

(* Off each edge in turn, and off a corner. The part that is on the buffer is
   drawn and the part that is not is not — the alternative is writing into the
   next row, which no test could see and every user would. *)
let a_rectangle_is_clipped_to_the_buffer () =
  (* Written out rather than looped, because each edge fails differently and the
     pixel worth checking afterwards is different for each. *)
  let over_left = buffer () in
  Paint.rect over_left ~x:(-3) ~y:2 ~w:5 ~h:2 ~color:(Color.rgb 255 255 255)
    ~alpha:255;
  Alcotest.check color "off the left: the visible part" white
    (Framebuffer.pixel over_left ~x:0 ~y:2);
  Alcotest.check color "off the left: and it stops where it should" black
    (Framebuffer.pixel over_left ~x:2 ~y:2);

  let over_top = buffer () in
  Paint.rect over_top ~x:2 ~y:(-3) ~w:2 ~h:5 ~color:(Color.rgb 255 255 255)
    ~alpha:255;
  Alcotest.check color "off the top: the visible part" white
    (Framebuffer.pixel over_top ~x:2 ~y:0);
  Alcotest.check color "off the top: and it stops" black
    (Framebuffer.pixel over_top ~x:2 ~y:2);

  let over_right = buffer () in
  Paint.rect over_right ~x:8 ~y:2 ~w:5 ~h:2 ~color:(Color.rgb 255 255 255)
    ~alpha:255;
  Alcotest.check color "off the right: the last column is drawn" white
    (Framebuffer.pixel over_right ~x:9 ~y:2);
  (* If the clip were missing, the overflow would land at the start of the next
     row down — which is exactly the pixel to check. *)
  Alcotest.check color "off the right: and nothing wrapped onto the next row"
    black
    (Framebuffer.pixel over_right ~x:0 ~y:3);

  let over_bottom = buffer () in
  Paint.rect over_bottom ~x:2 ~y:6 ~w:2 ~h:5 ~color:(Color.rgb 255 255 255)
    ~alpha:255;
  Alcotest.check color "off the bottom: the last row is drawn" white
    (Framebuffer.pixel over_bottom ~x:2 ~y:7);

  let over_corner = buffer () in
  Paint.rect over_corner ~x:(-2) ~y:(-2) ~w:4 ~h:4
    ~color:(Color.rgb 255 255 255) ~alpha:255;
  Alcotest.check color "off a corner: the quarter that shows" white
    (Framebuffer.pixel over_corner ~x:0 ~y:0);
  Alcotest.check color "off a corner: and no more" black
    (Framebuffer.pixel over_corner ~x:2 ~y:2)

let a_rectangle_entirely_off_the_buffer_draws_nothing () =
  List.iter
    (fun (name, x, y) ->
      let fb = buffer () in
      Paint.rect fb ~x ~y ~w:3 ~h:3 ~color:(Color.rgb 255 255 255) ~alpha:255;
      for py = 0 to 7 do
        for px = 0 to 9 do
          Alcotest.check color
            (Printf.sprintf "%s: (%d, %d)" name px py)
            black
            (Framebuffer.pixel fb ~x:px ~y:py)
        done
      done)
    [
      ("far left", -9, 2);
      ("far right", 12, 2);
      ("above", 2, -9);
      ("below", 2, 11);
    ]

(* Alpha composites against what is already there rather than replacing it,
   which is what lets a panel tint the world behind it instead of hiding it. *)
let alpha_blends_with_what_is_underneath () =
  let fb = buffer () in
  Paint.rect fb ~x:0 ~y:0 ~w:4 ~h:4 ~color:(Color.rgb 200 100 0) ~alpha:255;
  Paint.rect fb ~x:0 ~y:0 ~w:2 ~h:2 ~color:(Color.rgb 0 0 200) ~alpha:128;
  (* (src * a + dst * (255 - a)) / 255, with a = 128. *)
  let mix dst src = ((src * 128) + (dst * 127)) / 255 in
  Alcotest.check color "half of each"
    (Color.rgb (mix 200 0) (mix 100 0) (mix 0 200))
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "and the part not covered is as it was"
    (Color.rgb 200 100 0)
    (Framebuffer.pixel fb ~x:3 ~y:3);
  (* Fully transparent leaves the pixel exactly alone. *)
  Paint.rect fb ~x:3 ~y:3 ~w:1 ~h:1 ~color:(Color.rgb 0 255 0) ~alpha:0;
  Alcotest.check color "alpha 0 changes nothing" (Color.rgb 200 100 0)
    (Framebuffer.pixel fb ~x:3 ~y:3)

(* An alpha out of 255 that overshoots the range saturates rather than wrapping,
   the way Image and Texture clamp the alpha a generator hands them. It matters
   here because P.rect passes a game's own arithmetic straight through: a fade
   worked out as a countdown goes negative one frame past the end, and
   Framebuffer.blend weighs the destination with 255 - alpha into a byte, so a
   negative alpha there comes out as a colour rather than as nothing. *)
let an_alpha_that_overshoots_is_clamped () =
  let fb = buffer () in
  Paint.rect fb ~x:0 ~y:0 ~w:4 ~h:4 ~color:(Color.rgb 200 100 0) ~alpha:255;
  Paint.rect fb ~x:0 ~y:0 ~w:2 ~h:2 ~color:(Color.rgb 0 0 200) ~alpha:(-100);
  Alcotest.check color "an alpha below the floor clamps down to nothing at all"
    (Color.rgb 200 100 0)
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Paint.rect fb ~x:2 ~y:2 ~w:2 ~h:2 ~color:(Color.rgb 0 0 200) ~alpha:400;
  Alcotest.check color "and one above the ceiling clamps up to solid"
    (Color.rgb 0 0 200)
    (Framebuffer.pixel fb ~x:2 ~y:2)

(* And the colour beside it, which is the same argument the comment above
   [rect]'s alpha makes, word for word, about the three channels it used not to
   apply it to. {!Color.rgb} does not clamp — deliberately, so that a value
   reached by arithmetic can be carried about before it is put back — and
   {!Framebuffer.set} stores a byte, so a channel outside 0 .. 255 does not
   saturate there: it takes the low eight bits.

   Written as {e asking for more never gives less}, which is what a wrap breaks
   and what a game writing [base + boost] is relying on, rather than as
   agreement with {!Color.clamp} — a test that asked the clamp what it thought
   would go on passing if both moved together. The sweep runs well past both
   ends, in steps that cross 256 and 0 rather than landing on them. *)
let asking_for_more_of_a_channel_never_gives_less () =
  let painted through v =
    let fb = buffer () in
    through fb (Color.rgb v (255 - v) 0);
    Framebuffer.pixel fb ~x:1 ~y:1
  in
  let solid fb color = Paint.rect fb ~x:0 ~y:0 ~w:4 ~h:4 ~color ~alpha:255
  and blended fb color =
    Paint.rect fb ~x:0 ~y:0 ~w:4 ~h:4 ~color:black ~alpha:255;
    Paint.rect fb ~x:0 ~y:0 ~w:4 ~h:4 ~color ~alpha:128
  and tinted fb tint =
    let white =
      Image.make ~width:4 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
    in
    Paint.image ~tint fb white ~x:0 ~y:0
  in
  List.iter
    (fun (name, through) ->
      let asked = List.init 31 (fun i -> -150 + (i * 27)) in
      let got = List.map (fun v -> painted through v) asked in
      List.iter2
        (fun v (c : Color.t) ->
          Alcotest.(check bool)
            (Printf.sprintf "%s: %d landed on the buffer as %d" name v c.Color.r)
            true
            (c.Color.r >= 0 && c.Color.r <= 255))
        asked got;
      (* Rising on the red, falling on the green, and every step of both in the
         direction it was asked in. A wrap shows up as one step the other way. *)
      ignore
        (List.fold_left2
           (fun (v0, (was : Color.t)) v (c : Color.t) ->
             Alcotest.(check bool)
               (Printf.sprintf
                  "%s: %d asked for more red than %d and got less (%d after %d)"
                  name v v0 c.Color.r was.Color.r)
               true (c.Color.r >= was.Color.r);
             Alcotest.(check bool)
               (Printf.sprintf
                  "%s: %d asked for less green than %d and got more (%d after \
                   %d)"
                  name v v0 c.Color.g was.Color.g)
               true (c.Color.g <= was.Color.g);
             (v, c))
           (List.hd asked, List.hd got)
           (List.tl asked) (List.tl got)))
    [ ("solid", solid); ("blended", blended); ("tinted", tinted) ];
  (* The concrete case the report named, worth pinning on its own: a red
     brightened past full used to come out a dark teal. *)
  let fb = buffer () in
  Paint.rect fb ~x:0 ~y:0 ~w:4 ~h:4
    ~color:(Color.rgb (200 + 80) 40 40)
    ~alpha:255;
  Alcotest.check color "200 boosted by 80 saturates rather than wrapping to 24"
    (Color.rgb 255 40 40)
    (Framebuffer.pixel fb ~x:1 ~y:1)

(* A picture keeps its own per-pixel alpha, so a cut-out one leaves what was
   under it rather than stamping a rectangle of paint. *)
let an_image_keeps_its_own_transparency () =
  let fb = buffer () in
  Paint.rect fb ~x:0 ~y:0 ~w:10 ~h:8 ~color:(Color.rgb 40 40 40) ~alpha:255;
  (* Solid on the left half, clear on the right. *)
  let img =
    Image.make ~height:2 ~width:4 (fun ~u ~v:_ ->
        if u < 2 then (Color.rgb 255 0 0, 255) else Image.clear)
  in
  Paint.image fb img ~x:1 ~y:1;
  Alcotest.check color "the solid part is drawn" (Color.rgb 255 0 0)
    (Framebuffer.pixel fb ~x:1 ~y:1);
  Alcotest.check color "and the clear part shows what was underneath"
    (Color.rgb 40 40 40)
    (Framebuffer.pixel fb ~x:3 ~y:1)

(* A glyph is a rectangle of an atlas, so sub has to take the rectangle asked
   for and no other — and clip it on the destination without sliding the source
   under it. *)
let a_sub_rectangle_takes_what_it_was_asked_for () =
  (* Each pixel's red channel is its own column, so what lands says where it
     came from. *)
  let img =
    Image.make ~height:4 ~width:8 (fun ~u ~v:_ -> (Color.rgb (u * 10) 0 0, 255))
  in
  let fb = buffer () in
  Paint.sub fb img ~x:0 ~y:0 ~sx:3 ~sy:1 ~sw:2 ~sh:2;
  Alcotest.check color "the first column of the rectangle" (Color.rgb 30 0 0)
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "the second" (Color.rgb 40 0 0)
    (Framebuffer.pixel fb ~x:1 ~y:0);
  Alcotest.check color "and not the third" black
    (Framebuffer.pixel fb ~x:2 ~y:0);
  (* Clipped at the left edge, the source has to advance with the destination:
     the column that survives is the second of the rectangle, not the first. *)
  let fb = buffer () in
  Paint.sub fb img ~x:(-1) ~y:0 ~sx:3 ~sy:1 ~sw:2 ~sh:2;
  Alcotest.check color "clipping moves the source with the destination"
    (Color.rgb 40 0 0)
    (Framebuffer.pixel fb ~x:0 ~y:0);
  (* And it never reads past the picture, however much is asked for. *)
  let fb = buffer () in
  Paint.sub fb img ~x:0 ~y:0 ~sx:6 ~sy:0 ~sw:6 ~sh:6;
  Alcotest.check color "the last column of the picture" (Color.rgb 70 0 0)
    (Framebuffer.pixel fb ~x:1 ~y:0);
  Alcotest.check color "and nothing past it" black
    (Framebuffer.pixel fb ~x:2 ~y:0)

(* A rectangle that starts before the picture does. The far edge has always
   stopped at the picture; the near one has to as well, and for a worse reason
   than running off the end: a negative [sx] makes [u] negative, and a negative
   [u] is only out of bounds on the first row. On every row after it,
   [v * width + u] is a perfectly good index into the row above — so what a
   missing near-edge clip costs is not a crash but a picture quietly wound one
   row back. *)
let a_sub_rectangle_before_the_picture_is_clipped_too () =
  (* Column in the red channel and row in the green, both off by one so that
     the first of either is not black and cannot be mistaken for a pixel that
     was never drawn. *)
  let img =
    Image.make ~height:4 ~width:8 (fun ~u ~v ->
        (Color.rgb ((u + 1) * 10) ((v + 1) * 10) 0, 255))
  in
  let fb = buffer () in
  Paint.sub fb img ~x:0 ~y:0 ~sx:(-2) ~sy:0 ~sw:4 ~sh:2;
  Alcotest.check color "the two columns before the picture are left alone" black
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "and so is the second" black
    (Framebuffer.pixel fb ~x:1 ~y:0);
  Alcotest.check color "the picture starts at its own first column"
    (Color.rgb 10 10 0)
    (Framebuffer.pixel fb ~x:2 ~y:0);
  Alcotest.check color "and goes on from there" (Color.rgb 20 10 0)
    (Framebuffer.pixel fb ~x:3 ~y:0);
  Alcotest.check color "and stops where the rectangle asked for stops" black
    (Framebuffer.pixel fb ~x:4 ~y:0);
  (* The same upwards, which is the case that wrapped rather than raised. *)
  let fb = buffer () in
  Paint.sub fb img ~x:0 ~y:0 ~sx:0 ~sy:(-1) ~sw:2 ~sh:3;
  Alcotest.check color "the row before the picture is left alone" black
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "the picture starts at its own first row"
    (Color.rgb 10 10 0)
    (Framebuffer.pixel fb ~x:0 ~y:1);
  Alcotest.check color "and the row below it is the second, not the first"
    (Color.rgb 10 20 0)
    (Framebuffer.pixel fb ~x:0 ~y:2);
  (* And one wholly before the picture draws nothing, exactly as one wholly
     past it does, rather than raising. *)
  let fb = buffer () in
  Paint.sub fb img ~x:0 ~y:0 ~sx:(-20) ~sy:(-20) ~sw:4 ~sh:4;
  Alcotest.check color "nothing at all" black (Framebuffer.pixel fb ~x:0 ~y:0)

let a_tint_multiplies_the_picture () =
  let fb = buffer () in
  let img =
    Image.make ~width:2 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  Paint.image ~tint:(Color.rgb 100 200 50) fb img ~x:0 ~y:0;
  Alcotest.check color "white takes the tint exactly" (Color.rgb 100 200 50)
    (Framebuffer.pixel fb ~x:0 ~y:0);
  let fb = buffer () in
  let half =
    Image.make ~width:2 (fun ~u:_ ~v:_ -> (Color.rgb 128 128 128, 255))
  in
  Paint.image ~tint:(Color.rgb 200 200 200) fb half ~x:0 ~y:0;
  Alcotest.check color "and a grey halves it"
    (Color.rgb (128 * 200 / 255) (128 * 200 / 255) (128 * 200 / 255))
    (Framebuffer.pixel fb ~x:0 ~y:0)

(* A line is walked in whole pixels and clipped like everything else, so one
   that leaves the buffer draws the part that did not. *)
let a_line_is_clipped_too () =
  let fb = buffer () in
  Paint.line fb ~x0:0 ~y0:0 ~x1:20 ~y1:0 ~color:(Color.rgb 255 255 255);
  Alcotest.check color "the start" white (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "the last pixel on the buffer" white
    (Framebuffer.pixel fb ~x:9 ~y:0);
  Alcotest.check color "and nothing wrapped to the row below" black
    (Framebuffer.pixel fb ~x:0 ~y:1)

(* And clipped before it is walked, not while. A ring round something close to
   the eye is placed by dividing by its distance, so an endpoint can be millions
   of pixels off the buffer for a line whose visible part is a few pixels long.
   Walking the whole of that and throwing away each pixel in turn is correct and
   unusable; the range that could land is worked out first.

   The assertion is the same pixels as above — the narrowing must not move them
   — and, in the completing at all, that the walk is the length of what is on
   screen. Without that it is a thousand million iterations. *)
let a_line_from_far_off_the_buffer_is_still_cheap () =
  let fb = buffer () in
  Paint.line fb ~x0:0 ~y0:0 ~x1:1_000_000_000 ~y1:0
    ~color:(Color.rgb 255 255 255);
  Alcotest.check color "the start" white (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "the last pixel on the buffer" white
    (Framebuffer.pixel fb ~x:9 ~y:0);
  Alcotest.check color "and nothing wrapped to the row below" black
    (Framebuffer.pixel fb ~x:0 ~y:1)

(* Both ends outside, on opposite sides, and the part crossing the buffer is
   drawn: the narrowing has to keep the middle of a segment whose ends it
   rejected. A diagonal, so neither axis alone decides it. *)
let a_line_passing_across_the_buffer_draws_the_crossing () =
  let fb = buffer () in
  Paint.line fb ~x0:(-1000) ~y0:(-1000) ~x1:1000 ~y1:1000
    ~color:(Color.rgb 255 255 255);
  Alcotest.check color "the corner it enters at" white
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "and on down the diagonal" white
    (Framebuffer.pixel fb ~x:7 ~y:7);
  Alcotest.check color "but not beside it" black
    (Framebuffer.pixel fb ~x:0 ~y:7)

(* A line that never touches the buffer draws nothing, and says so by not
   raising: the range the two axes leave is empty rather than reversed. *)
let a_line_entirely_off_the_buffer_draws_nothing () =
  let fb = buffer () in
  Paint.line fb ~x0:100 ~y0:0 ~x1:200 ~y1:0 ~color:(Color.rgb 255 255 255);
  Paint.line fb ~x0:0 ~y0:50 ~x1:9 ~y1:50 ~color:(Color.rgb 255 255 255);
  Alcotest.check color "nothing arrived" black (Framebuffer.pixel fb ~x:0 ~y:0)

(* The crosshair's own claim: two arms crossing on the pixel that holds the
   middle of the buffer. Which pixel that is matters to more than this module —
   {!Viewport} casts the straight-ahead ray through it and {!Sight} answers
   about that ray — but that the arms meet there at all is Paint's, and was
   asserted nowhere. An odd buffer, so the middle is one pixel and not two. *)
let a_crosshair_crosses_in_the_middle () =
  let fb = Framebuffer.offscreen ~width:9 ~height:7 in
  Paint.crosshair fb ~color:(Color.rgb 255 255 255);
  Alcotest.check color "the arms meet in the middle" white
    (Framebuffer.pixel fb ~x:4 ~y:3);
  Alcotest.check color "the horizontal arm reaches the left edge" white
    (Framebuffer.pixel fb ~x:0 ~y:3);
  Alcotest.check color "and the vertical one the top" white
    (Framebuffer.pixel fb ~x:4 ~y:0);
  (* Neither arm spills into the quadrants between them. *)
  Alcotest.check color "off the arms is untouched" black
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "and so is the far corner" black
    (Framebuffer.pixel fb ~x:8 ~y:6)

(* Eleven-pixel arms on a buffer of one pixel: every span is clipped away to
   nothing but the one that holds the middle, which is the whole buffer. A
   minimised window is exactly this. *)
let a_crosshair_survives_a_single_pixel () =
  let fb = Framebuffer.offscreen ~width:1 ~height:1 in
  Paint.crosshair fb ~color:(Color.rgb 255 255 255);
  Alcotest.check color "the one pixel is drawn" white
    (Framebuffer.pixel fb ~x:0 ~y:0)

let () =
  Alcotest.run "Paint"
    [
      ( "the buffer",
        [
          case "a fresh buffer is black" a_fresh_buffer_is_black;
          case "a buffer of no size is refused" a_buffer_of_no_size_is_refused;
          case "a buffer too big for an array is refused"
            a_buffer_too_big_for_an_array_is_refused;
          case "a rectangle fills what it covers"
            a_rectangle_fills_what_it_covers;
          case "a crosshair crosses in the middle"
            a_crosshair_crosses_in_the_middle;
          case "a crosshair survives a single pixel"
            a_crosshair_survives_a_single_pixel;
        ] );
      ( "clipping",
        [
          case "a rectangle is clipped to the buffer"
            a_rectangle_is_clipped_to_the_buffer;
          case "a rectangle entirely off the buffer draws nothing"
            a_rectangle_entirely_off_the_buffer_draws_nothing;
          case "a line is clipped too" a_line_is_clipped_too;
          case "a line from far off the buffer is still cheap"
            a_line_from_far_off_the_buffer_is_still_cheap;
          case "a line passing across the buffer draws the crossing"
            a_line_passing_across_the_buffer_draws_the_crossing;
          case "a line entirely off the buffer draws nothing"
            a_line_entirely_off_the_buffer_draws_nothing;
        ] );
      ( "pictures",
        [
          case "alpha blends with what is underneath"
            alpha_blends_with_what_is_underneath;
          case "an alpha that overshoots is clamped"
            an_alpha_that_overshoots_is_clamped;
          case "asking for more of a channel never gives less"
            asking_for_more_of_a_channel_never_gives_less;
          case "an image keeps its own transparency"
            an_image_keeps_its_own_transparency;
          case "a sub rectangle takes what it was asked for"
            a_sub_rectangle_takes_what_it_was_asked_for;
          case "a sub rectangle before the picture is clipped too"
            a_sub_rectangle_before_the_picture_is_clipped_too;
          case "a tint multiplies the picture" a_tint_multiplies_the_picture;
        ] );
    ]
