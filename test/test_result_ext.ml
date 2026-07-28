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

let () =
  Alcotest.run "Result_ext"
    [
      ( "binding operators",
        [
          case "a chain of successes" ok_chain;
          case "errors are propagated" errors_are_propagated;
          case "nothing after an error runs" nothing_after_an_error_runs;
        ] );
    ]
