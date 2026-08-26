(** Writing a component, and moving part of one into a file of its own.

    What is checked is mostly one thing: that what comes out compiles as OCaml
    and has the shape {!Camlcast.Element.declare} requires. Both halves of that
    shape are correctness rules whose symptom is state that silently stops
    working, so a scaffolder is worth having only if it holds them. *)

let case name body = Alcotest.test_case name `Quick body

let parses source =
  match Camlcast_edit.Span.parse ~path:"written.ml" source with
  | Ok _ -> true
  | Error _ -> false

let holds text needle =
  let rec find index =
    index + String.length needle <= String.length text
    && (String.sub text index (String.length needle) = needle
       || find (index + 1))
  in
  find 0

let a_written_component_is_ocaml () =
  let file =
    Camlcast_edit.Scaffold.component ~name:"gallery_wall"
      ~body:
        "P.wall ~key:\"north\" ~height:2.5 ~material:Surfaces.stone\n\
        \    (Vec.make (-3.) 1.) (Vec.make 3. 1.)"
  in
  Alcotest.(check string) "named for its module" "gallery_wall.ml" file.path;
  Alcotest.(check bool) "and it parses" true (parses file.source)

(* The two halves of the rule, asserted as text because that is what they are:
   a shape in a file. Declared at the top level, and written as a partial
   application rather than as a function of its props. *)
let a_written_component_has_the_shape_the_rule_wants () =
  let file =
    Camlcast_edit.Scaffold.component ~name:"torch" ~body:"Element.empty"
  in
  Alcotest.(check bool)
    "declared at the top level, not inside another render" true
    (holds file.source "\nlet torch =\n");
  Alcotest.(check bool)
    "as a partial application, which is what keeps its props monomorphic" true
    (holds file.source "Element.declare ~name:\"torch\" @@ fun () ->");
  Alcotest.(check bool)
    "and not written out as a function of its props" false
    (holds file.source "?key")

let a_name_that_is_not_a_module_is_a_mistake_in_the_program () =
  List.iter
    (fun name ->
      Alcotest.check_raises
        (Printf.sprintf "%S is not a module name" name)
        (Invalid_argument
           (Printf.sprintf
              "Scaffold: %S cannot be a module name; use lowercase letters, \
               digits and underscores, starting with a letter"
              name))
        (fun () ->
          ignore (Camlcast_edit.Scaffold.component ~name ~body:"Element.empty")))
    [ ""; "Gallery"; "gallery wall"; "2nd"; "gallery-wall" ]

(* {1 Extracting} *)

let fixture =
  {ocaml|open Camlcast

let level =
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height:3. ~material:Surfaces.stone
          ~floor:(floor ~plane:flat Surfaces.ground)
          ~ceiling:(roof Surfaces.soffit)
          [
            spawn (Vec.make 0. 0.);
            sprite ~key:"torch" ~size:0.9 ~image:Pictures.flame
              (Vec.make 2. (-3.));
          ];
      ])
|ocaml}

let parsed () =
  match Camlcast_edit.Span.parse ~path:"level.ml" fixture with
  | Ok parsed -> parsed
  | Error (`Msg m) -> Alcotest.failf "the fixture does not parse: %s" m

let sprite_span () =
  match
    List.find_opt
      (fun (c : Camlcast_edit.Span.call) -> c.callee = "sprite")
      (Camlcast_edit.Span.calls (parsed ()))
  with
  | Some call -> call.span
  | None -> Alcotest.fail "no sprite in the fixture"

let extracting_writes_a_file_and_the_call_that_replaces_it () =
  match
    Camlcast_edit.Scaffold.extract (parsed ()) ~name:"torch" (sprite_span ())
  with
  | Error (`Msg m) -> Alcotest.failf "extract refused: %s" m
  | Ok (file, edit) -> (
      Alcotest.(check string) "the new file" "torch.ml" file.path;
      Alcotest.(check bool)
        "holding what was selected" true
        (holds file.source "~key:\"torch\"");
      Alcotest.(check bool) "and it parses" true (parses file.source);
      match Camlcast_edit.Span.splice (parsed ()) [ edit ] with
      | Error (`Msg m) -> Alcotest.failf "splice refused: %s" m
      | Ok written ->
          Alcotest.(check bool)
            "the level now calls it" true (holds written "torch ()");
          Alcotest.(check bool)
            "does not still hold what moved out" false
            (holds written "~size:0.9");
          Alcotest.(check bool) "and still parses" true (parses written))

(* Both halves are handed back together because writing one without the other
   leaves something that does not build, in one direction or the other. *)
let a_blank_selection_is_refused () =
  Alcotest.(check bool)
    "an empty span" true
    (Result.is_error
       (Camlcast_edit.Scaffold.extract (parsed ()) ~name:"torch"
          { Camlcast_edit.Span.start = 10; stop = 10 }));
  (* The blank line after the opening, found rather than assumed. *)
  let blank =
    let rec find index =
      if index + 2 > String.length fixture then
        Alcotest.fail "the fixture should have a blank line in it"
      else if String.sub fixture index 2 = "\n\n" then index
      else find (index + 1)
    in
    find 0
  in
  Alcotest.(check bool)
    "and one covering only blank space" true
    (Result.is_error
       (Camlcast_edit.Scaffold.extract (parsed ()) ~name:"torch"
          { Camlcast_edit.Span.start = blank; stop = blank + 2 }))

let () =
  Alcotest.run "Scaffold"
    [
      ( "writing one",
        [
          case "a written component is OCaml" a_written_component_is_ocaml;
          case "a written component has the shape the rule wants"
            a_written_component_has_the_shape_the_rule_wants;
          case "a name that is not a module is a mistake in the program"
            a_name_that_is_not_a_module_is_a_mistake_in_the_program;
        ] );
      ( "extracting",
        [
          case "extracting writes a file and the call that replaces it"
            extracting_writes_a_file_and_the_call_that_replaces_it;
          case "a blank selection is refused" a_blank_selection_is_refused;
        ] );
    ]
