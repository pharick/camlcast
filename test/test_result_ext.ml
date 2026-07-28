open Camlcast
open Result_ext
open Support

let ok_chain () =
  let outcome =
    let* a = Ok 2 in
    let+ b = Ok 3 in
    a + b
  in
  Alcotest.(check (result int string))
    "a chain of successes succeeds" (Ok 5) outcome

let errors_are_propagated () =
  let outcome =
    let* a = Ok 2 in
    let+ b = Error "boom" in
    a + b
  in
  Alcotest.(check (result int string)) "the error wins" (Error "boom") outcome

let nothing_after_an_error_runs () =
  let evaluated = ref false in
  let outcome =
    let* () = Error "boom" in
    evaluated := true;
    Ok ()
  in
  Alcotest.(check (result unit string))
    "the error escapes" (Error "boom") outcome;
  Alcotest.(check bool) "the rest of the chain is skipped" false !evaluated

let iter_range_visits_every_index () =
  let visited = ref [] in
  let outcome =
    iter_range ~first:2 ~last:5 (fun i ->
        visited := i :: !visited;
        Ok ())
  in
  Alcotest.(check (result unit string)) "the loop succeeds" (Ok ()) outcome;
  Alcotest.(check (list int))
    "the range is inclusive and in order" [ 2; 3; 4; 5 ] (List.rev !visited)

let iter_range_stops_at_the_first_error () =
  let visited = ref [] in
  let outcome =
    iter_range ~first:0 ~last:100 (fun i ->
        visited := i :: !visited;
        if i = 3 then Error "boom" else Ok ())
  in
  Alcotest.(check (result unit string))
    "the error escapes the loop" (Error "boom") outcome;
  Alcotest.(check (list int))
    "iteration stops there" [ 0; 1; 2; 3 ] (List.rev !visited)

let iter_range_over_an_empty_range () =
  let called = ref false in
  let outcome =
    iter_range ~first:5 ~last:4 (fun _ ->
        called := true;
        Ok ())
  in
  Alcotest.(check (result unit string))
    "an empty range succeeds" (Ok ()) outcome;
  Alcotest.(check bool) "and calls nothing" false !called

let () =
  Alcotest.run "Result_ext"
    [
      ( "binding operators",
        [
          case "a chain of successes" ok_chain;
          case "errors are propagated" errors_are_propagated;
          case "nothing after an error runs" nothing_after_an_error_runs;
        ] );
      ( "iter_range",
        [
          case "visits every index" iter_range_visits_every_index;
          case "stops at the first error" iter_range_stops_at_the_first_error;
          case "over an empty range" iter_range_over_an_empty_range;
        ] );
    ]
