(** Changing a file, and taking the change back.

    Most of this is driven through an in-memory reader and writer, because what
    is under test is the rule — read afresh, splice, check it parses, write,
    remember — and not the file system. One case goes to a real directory,
    because the promise about arriving by one rename is about a real one. *)

let case name body = Alcotest.test_case name `Quick body

let level =
  {ocaml|open Camlcast

(* A wall, and a comment that must survive. *)
let level = P.(wall ~height:2.5 ~material:stone (Vec.make (-3.) 1.) (Vec.make 3. 1.))
|ocaml}

(* A disk of one file, so the rule can be checked without one. *)
let paper contents =
  let store = ref contents in
  let read path =
    match List.assoc_opt path !store with
    | Some text -> Ok text
    | None -> Error (`Msg (path ^ ": no such file"))
  in
  let write path text =
    store := (path, text) :: List.remove_assoc path !store;
    Ok ()
  in
  (store, Camlcast_edit.Revise.create ~read ~write)

let held store path =
  match List.assoc_opt path !store with
  | Some text -> text
  | None -> Alcotest.failf "%s is not there" path

let height_span source =
  match Camlcast_edit.Span.parse ~path:"level.ml" source with
  | Error (`Msg m) -> Alcotest.failf "fixture: %s" m
  | Ok parsed -> (
      match
        List.find_opt
          (fun (c : Camlcast_edit.Span.call) -> c.callee = "wall")
          (Camlcast_edit.Span.calls parsed)
      with
      | None -> Alcotest.fail "no wall"
      | Some call -> (
          match
            List.find_opt
              (fun (a : Camlcast_edit.Span.argument) -> a.label = Some "height")
              call.arguments
          with
          | Some { value = Numbers (span :: _); _ } -> span
          | _ -> Alcotest.fail "no height"))

let an_edit_lands_and_leaves_the_rest_alone () =
  let store, revise = paper [ ("level.ml", level) ] in
  match
    Camlcast_edit.Revise.apply revise ~path:"level.ml"
      [ (height_span level, "3.5") ]
  with
  | Error (`Msg m) -> Alcotest.failf "refused: %s" m
  | Ok () ->
      let written = held store "level.ml" in
      Alcotest.(check string)
        "one number, and the comment still there"
        {ocaml|open Camlcast

(* A wall, and a comment that must survive. *)
let level = P.(wall ~height:3.5 ~material:stone (Vec.make (-3.) 1.) (Vec.make 3. 1.))
|ocaml}
        written;
      Alcotest.(check int)
        "one change recorded" 1
        (Camlcast_edit.Revise.depth revise)

let taking_it_back_puts_the_text_back () =
  let store, revise = paper [ ("level.ml", level) ] in
  ignore
    (Camlcast_edit.Revise.apply revise ~path:"level.ml"
       [ (height_span level, "3.5") ]);
  match Camlcast_edit.Revise.undo revise with
  | Error (`Msg m) -> Alcotest.failf "undo refused: %s" m
  | Ok (Some (Restored path)) ->
      Alcotest.(check string) "the file it put back" "level.ml" path;
      Alcotest.(check string) "exactly as it was" level (held store "level.ml");
      Alcotest.(check int)
        "and nothing left to take back" 0
        (Camlcast_edit.Revise.depth revise)
  | Ok _ -> Alcotest.fail "that file existed before; it should be restored"

let nothing_to_take_back_is_a_state () =
  let _, revise = paper [] in
  Alcotest.(check bool)
    "asked and answered, not raised" true
    (match Camlcast_edit.Revise.undo revise with Ok None -> true | _ -> false)

(* A file the editor made has no earlier text to put back, and removing it is
   not this module's decision to make. It says which file, so the caller can
   offer. *)
let a_file_that_did_not_exist_is_said_to_be_created () =
  let store, revise = paper [] in
  ignore
    (Camlcast_edit.Revise.write revise ~path:"torch.ml"
       "open Camlcast\n\nlet torch = Element.empty\n");
  match Camlcast_edit.Revise.undo revise with
  | Ok (Some (Created path)) ->
      Alcotest.(check string) "named" "torch.ml" path;
      Alcotest.(check bool)
        "and left where it is" true
        (List.mem_assoc "torch.ml" !store)
  | _ -> Alcotest.fail "a file with no earlier text should be reported created"

(* The cheapest check that an edit did what it claimed. A refusal must leave
   the file untouched, which is the only outcome worth having when the
   alternative is a game's source left broken by a tool. *)
let text_that_does_not_parse_is_never_written () =
  let store, revise = paper [ ("level.ml", level) ] in
  let whole = { Camlcast_edit.Span.start = 0; stop = String.length level } in
  Alcotest.(check bool)
    "refused" true
    (Result.is_error
       (Camlcast_edit.Revise.apply revise ~path:"level.ml"
          [ (whole, "let x = (") ]));
  Alcotest.(check string)
    "and the file is what it was" level (held store "level.ml");
  Alcotest.(check int)
    "with nothing recorded" 0
    (Camlcast_edit.Revise.depth revise)

let a_file_that_is_not_there_is_a_condition () =
  let _, revise = paper [] in
  Alcotest.(check bool)
    "reported, not raised" true
    (Result.is_error
       (Camlcast_edit.Revise.apply revise ~path:"gone.ml"
          [ ({ Camlcast_edit.Span.start = 0; stop = 0 }, "x") ]))

(* {1 On a real one} *)

let it_arrives_on_a_real_file () =
  let directory =
    Filename.concat (Filename.get_temp_dir_name ()) "camlcast-revise"
  in
  (try Sys.mkdir directory 0o755 with Sys_error _ -> ());
  let path = Filename.concat directory "level.ml" in
  Out_channel.with_open_bin path (fun out ->
      Out_channel.output_string out level);
  let revise =
    Camlcast_edit.Revise.create
      ~read:(Camlcast_edit.Revise.from_disk ())
      ~write:(Camlcast_edit.Revise.to_disk ())
  in
  (match
     Camlcast_edit.Revise.apply revise ~path [ (height_span level, "9.5") ]
   with
  | Error (`Msg m) -> Alcotest.failf "refused: %s" m
  | Ok () -> ());
  let back = In_channel.with_open_bin path In_channel.input_all in
  Alcotest.(check bool)
    "the number changed on disk" true
    (String.length back > 0
    &&
    let rec holds i =
      i + 12 <= String.length back
      && (String.sub back i 12 = "~height:9.5 " || holds (i + 1))
    in
    holds 0);
  (* And nothing left beside it: a temporary that survived would look like a
     partial write. *)
  Alcotest.(check (list string))
    "one file in the directory, and it is the one meant" [ "level.ml" ]
    (List.sort String.compare (Array.to_list (Sys.readdir directory)));
  Sys.remove path;
  Sys.rmdir directory

let () =
  Alcotest.run "Revise"
    [
      ( "changing a file",
        [
          case "an edit lands and leaves the rest alone"
            an_edit_lands_and_leaves_the_rest_alone;
          case "text that does not parse is never written"
            text_that_does_not_parse_is_never_written;
          case "a file that is not there is a condition"
            a_file_that_is_not_there_is_a_condition;
        ] );
      ( "taking it back",
        [
          case "taking it back puts the text back"
            taking_it_back_puts_the_text_back;
          case "nothing to take back is a state" nothing_to_take_back_is_a_state;
          case "a file that did not exist is said to be created"
            a_file_that_did_not_exist_is_said_to_be_created;
        ] );
      ( "on a real one",
        [ case "it arrives on a real file" it_arrives_on_a_real_file ] );
    ]
