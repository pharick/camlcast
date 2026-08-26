(** From a wall in a built world back to the line that describes it.

    This is the whole chain in one suite, which is the only place it can be
    checked: the rewriter puts a position on an element, the reconciler carries
    it onto the committed node, and {!Camlcast_edit.Link} reads the file back
    and finds the expression. Each half is testable alone and neither half
    proves the two agree.

    The file it reads is this one. That is not a trick — it is the shortest
    honest fixture, because what a position names is the source it was compiled
    from, and this suite is compiled from this source. *)

open Camlcast

let case name body = Alcotest.test_case name `Quick body

let stone =
  Material.make ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 1 1 1))

let boundary =
  P.corners
    [
      Vec.make (-4.) (-4.); Vec.make 4. (-4.); Vec.make 4. 4.; Vec.make (-4.) 4.;
    ]

(* The wall this suite follows. Written out, so every argument is a literal and
   the whole of it can be dragged. *)
let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:boundary
          [
            spawn (Vec.make 0. 0.);
            wall ~key:"north" ~height:2.5 ~material:stone (Vec.make (-3.) 1.)
              (Vec.make 3. 1.);
          ];
      ])

(* The committed forest, which is where a position ends up. A patch is handed
   it on its way to being assembled and is the only thing that is. *)
let forest description =
  let taken = ref [] in
  let mount = Mount.create () in
  ignore
    (Mount.render mount description ~patch:(fun forest ->
         taken := forest;
         forest));
  Mount.destroy mount;
  !taken

let rec walk (node : Prim.t Camlcast_loom.Host.node) =
  node :: List.concat_map walk node.Camlcast_loom.Host.children

let nodes description = List.concat_map walk (forest description)

(* By key, and not merely "the first wall". A room's outline is four walls
   this file did not write: P.room builds them from the corners, inside
   lib/p.ml, which nothing preprocesses. They therefore carry no position, and
   rightly -- the author wrote an outline, not four walls, and the outline is
   what an editor should offer to change. See the case below, which pins it. *)
let keyed key =
  match
    List.find_opt
      (fun (node : Prim.t Camlcast_loom.Host.node) ->
        Filename.check_suffix
          (Camlcast_loom.Path.to_debug_string node.Camlcast_loom.Host.path)
          ("[" ^ key ^ "]"))
      (nodes level)
  with
  | Some node -> node
  | None -> Alcotest.failf "no node keyed %S" key

let the_wall () = keyed "north"

(* Reads this suite's own source. A position names the path the compiler was
   given, which is relative to the build root; a suite runs beside its own
   source. Both are tried so that the reader does not depend on how the
   executable was started. *)
let reader path =
  let rec attempt = function
    | [] -> Error (`Msg (path ^ ": not found from " ^ Sys.getcwd ()))
    | candidate :: rest -> (
        match In_channel.with_open_bin candidate In_channel.input_all with
        | source -> Ok source
        | exception Sys_error _ -> attempt rest)
  in
  attempt [ Filename.basename path; path ]

let link () = Camlcast_edit.Link.create reader

(* Whether this build carries positions at all, from dune rather than from the
   rewriter -- see test/ppx/dune, and test_ppx.ml, which makes the same
   distinction for the same reason. A release build carries none by design, so
   the chain below has nothing to follow and says so instead of failing. *)
let positioned =
  match Sys.getenv_opt "CAMLCAST_PROFILE" with
  | Some "release" -> false
  | Some _ | None -> true

let a_wall_in_the_world_finds_the_line_that_wrote_it () =
  match Camlcast_edit.Link.find (link ()) (the_wall ()) with
  | Unpositioned when not positioned -> ()
  | Unpositioned ->
      Alcotest.fail
        "no position: is this stanza preprocessed, and CAMLCAST_POSITIONS set?"
  | Unreadable message -> Alcotest.failf "unreadable: %s" message
  | Found source ->
      Alcotest.(check bool)
        "the file this suite is compiled from" true
        (Filename.check_suffix source.file "test_link.ml");
      Alcotest.(check string)
        "the constructor, as written" "wall" source.call.callee;
      Alcotest.(check bool)
        "and something about it can be dragged" true
        (Camlcast_edit.Link.editable source)

(* The arguments, in the terms an editor acts on: a height and two points that
   can be dragged, a material that can only be swapped, and a key that is a
   string and so neither. *)
let its_arguments_say_which_can_be_changed () =
  match Camlcast_edit.Link.find (link ()) (the_wall ()) with
  | Unpositioned when not positioned -> ()
  | Found source ->
      let describe (argument : Camlcast_edit.Span.argument) =
        ( Option.value argument.label ~default:"_",
          match argument.value with
          | Numbers spans -> Printf.sprintf "numbers %d" (List.length spans)
          | Name _ -> "name"
          | Computed -> "computed" )
      in
      Alcotest.(check (list (pair string string)))
        "per argument, which is the only place the question has an answer"
        [
          ("key", "computed");
          ("height", "numbers 1");
          ("material", "name");
          ("_", "numbers 2");
          ("_", "numbers 2");
        ]
        (List.map describe source.call.arguments)
  | _ -> Alcotest.fail "the wall should have been found"

(* The consequence of the note above, stated as a case so it cannot quietly
   stop being true: a wall the game did not write has nowhere to send an
   editor. This is not a gap to be filled by wrapping p.ml -- those walls have
   no expression of their own in anyone's source. What an editor offers for one
   of them is the outline it came from. *)
let a_wall_the_game_did_not_write_has_no_position () =
  let outline_wall =
    List.find_opt
      (fun (node : Prim.t Camlcast_loom.Host.node) ->
        match node.Camlcast_loom.Host.prim with
        | Prim.Wall _ ->
            not
              (Filename.check_suffix
                 (Camlcast_loom.Path.to_debug_string
                    node.Camlcast_loom.Host.path)
                 "[north]")
        | _ -> false)
      (nodes level)
  in
  match outline_wall with
  | None -> Alcotest.fail "the room's outline should have made walls"
  | Some node ->
      Alcotest.(check bool)
        "a leg of the outline, built inside the engine" true
        (match Camlcast_edit.Link.find (link ()) node with
        | Unpositioned -> true
        | _ -> false)

let a_node_with_no_position_says_so () =
  Alcotest.(check bool)
    "not an error -- a release build carries none by design" true
    (match Camlcast_edit.Link.locate (link ()) None with
    | Unpositioned -> true
    | _ -> false)

let a_file_that_cannot_be_read_says_so () =
  let link = Camlcast_edit.Link.create (fun _ -> Error (`Msg "no such file")) in
  Alcotest.(check bool)
    "reported, not raised" true
    (match Camlcast_edit.Link.locate link (Some ("gone.ml", 1, 0, 0)) with
    | Unreadable _ -> true
    | _ -> false)

(* The predicate, as the overlay actually shows it. A room's outline walls are
   built inside lib/p.ml and carry no position; the wall and the spawn written
   in this file carry one. The plan draws the first sort in one colour and the
   second in another, and that difference is the whole of what tells a reader
   which parts of the room are theirs to drag.

   Checked here rather than in test/edit/ because nothing there is
   preprocessed: without positions every line would be the same colour and the
   assertion would pass while saying nothing. *)
let the_plan_marks_what_the_game_wrote () =
  let session =
    Camlcast_edit.Session.create
      ~read:(fun path -> Error (`Msg (path ^ ": no files here")))
      ~write:(fun _ _ -> Ok ())
      ()
  in
  let watch = Camlcast_edit.Session.watch session in
  let mount = Mount.create () in
  ignore
    (Mount.render mount level ?trace:watch.Watch.trace ?patch:watch.Watch.patch);
  Mount.destroy mount;
  Camlcast_edit.Session.show session Camlcast_edit.Session.Plan;
  let body =
    (* Past the two header lines, which are the panel's own. *)
    match Camlcast_edit.Session.lines session with
    | _ :: _ :: body -> body
    | short -> short
  in
  let colour (line : Camlcast_edit.Panel.line) = line.color in
  let distinct = List.sort_uniq compare (List.map colour body) in
  (* Without positions nothing is distinguishable, and one colour is the right
     answer there rather than a failure -- a release build carries none. *)
  Alcotest.(check int)
    "two colours: what the game wrote, and what the engine built for it"
    (if positioned then 2 else 1)
    (List.length distinct);
  if not positioned then ()
  else
    let of_text needle =
      match
        List.find_opt
          (fun (line : Camlcast_edit.Panel.line) ->
            String.length line.text >= String.length needle
            && String.sub line.text 0 (String.length needle) = needle)
          body
      with
      | Some line -> colour line
      | None -> Alcotest.failf "no line starting %S" needle
    in
    Alcotest.(check bool)
      "the wall this file wrote is not drawn like a leg of the outline" false
      (of_text "wall (-3,1)" = of_text "wall (-4,-4)")

let () =
  Alcotest.run "Link"
    [
      ( "the whole chain",
        [
          case "a wall in the world finds the line that wrote it"
            a_wall_in_the_world_finds_the_line_that_wrote_it;
          case "its arguments say which can be changed"
            its_arguments_say_which_can_be_changed;
        ] );
      ( "what the overlay shows",
        [
          case "the plan marks what the game wrote"
            the_plan_marks_what_the_game_wrote;
        ] );
      ( "what it cannot find",
        [
          case "a wall the game did not write has no position"
            a_wall_the_game_did_not_write_has_no_position;
          case "a node with no position says so" a_node_with_no_position_says_so;
          case "a file that cannot be read says so"
            a_file_that_cannot_be_read_says_so;
        ] );
    ]
