open Camlcast_core
open Support

(* Over a range small enough that the multiplication is safe, the division has
   to agree with it exactly: for positive integers [height <= limit / width] is
   [width * height <= limit], floor and all. That is what makes the rewrite a
   rewrite rather than an approximation that happens to be conservative, and it
   is worth checking rather than believing — a [<] for the [<=], or the operands
   the other way up, would still refuse the enormous rectangles below while
   quietly refusing a legitimate one at the boundary. *)
let dividing_agrees_with_multiplying () =
  let limit = 997 in
  for width = 1 to 60 do
    for height = 1 to 60 do
      Alcotest.(check bool)
        (Printf.sprintf "%d x %d against %d" width height limit)
        (width * height <= limit)
        (Extent.fits ~limit ~width ~height)
    done
  done

(* And where multiplying cannot be trusted, which is the reason any of this is
   written down. OCaml's arithmetic wraps, so the product of the two largest
   extents there are is not enormous — it is [1]. A check written the obvious
   way says yes to it, and what follows is two arrays of one element under a
   record reporting a size of [max_int]. *)
let the_check_cannot_overflow_what_it_checks () =
  Alcotest.(check bool)
    "the largest rectangle there is does not fit" false
    (Extent.fits ~limit:Sys.max_array_length ~width:max_int ~height:max_int);
  Alcotest.(check bool)
    "though multiplying it out says otherwise" true
    (max_int * max_int <= Sys.max_array_length)

(* The limit is an argument because it is not one number. Texture and Image pass
   [Sys.max_array_length]; Framebuffer passes [Sys.max_floatarray_length],
   which is the tighter bound its depth buffer has to satisfy. The two are equal
   at 64 bits and are not at 32, so on this machine no fixture can tell them
   apart — what can be pinned is that the argument is what decides, rather than
   a number written inside. *)
let the_limit_is_the_caller_s () =
  Alcotest.(check bool)
    "a hundred pixels fit in a hundred" true
    (Extent.fits ~limit:100 ~width:10 ~height:10);
  Alcotest.(check bool)
    "and the same hundred do not fit in ninety-nine" false
    (Extent.fits ~limit:99 ~width:10 ~height:10)

(* The size that made this worth wiring into {!Image.load} and {!Texture.load}
   rather than left to their [make] and [generate] to raise on.

   At 32 bits an array holds [2^22 - 1] entries, and a 2048 by 2048 picture is
   one texel past that — an unremarkable thing to find on disk, not a hostile
   file. So the loaders ask this before allocating and answer with an [`Msg],
   because a file is a condition; reaching the allocation instead would put an
   [Invalid_argument] through a signature that says a bad file comes back as a
   value. Nothing here can be reached on the 64-bit build these tests run on,
   which is why the limit is passed rather than read from [Sys]: the arithmetic
   is the part that has to be right on a machine this is not.

   The pair either side of it is the point. One texel less and the same picture
   is fine, so this pins a boundary rather than the general shape of a refusal.
*)
let a_thirty_two_bit_array_stops_short_of_a_common_texture () =
  let limit = (1 lsl 22) - 1 in
  Alcotest.(check bool)
    "2048 square does not fit a 32-bit array" false
    (Extent.fits ~limit ~width:2048 ~height:2048);
  Alcotest.(check bool)
    "and it is over by exactly one texel" true
    ((2048 * 2048) - 1 = limit);
  Alcotest.(check bool)
    "so one row shorter does fit" true
    (Extent.fits ~limit ~width:2048 ~height:2047);
  Alcotest.(check bool)
    "and the same picture fits the 64-bit bound this runs on" true
    (Extent.fits ~limit:Sys.max_array_length ~width:2048 ~height:2048)

let () =
  Alcotest.run "Extent"
    [
      ( "fitting",
        [
          case "dividing agrees with multiplying"
            dividing_agrees_with_multiplying;
          case "the check cannot overflow what it checks"
            the_check_cannot_overflow_what_it_checks;
          case "the limit is the caller's" the_limit_is_the_caller_s;
          case "a 32-bit array stops short of a common texture"
            a_thirty_two_bit_array_stops_short_of_a_common_texture;
        ] );
    ]
