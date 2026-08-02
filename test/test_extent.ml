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
        ] );
    ]
