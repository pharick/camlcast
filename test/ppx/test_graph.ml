(** The world as a graph, and joining two doorways that lead nowhere.

    In the preprocessed directory because {!Camlcast_edit.Graph.join} has to
    follow each doorway back to the [cut] that placed it: what a connection
    names is the {e binding} the door was made under, and only the source says
    what that is. A door's own name is for diagnostics — this fixture calls one
    ["east"] while binding it to [plaza_east], which is the difference that
    makes following it back necessary rather than tidy. *)

open Camlcast

let case name body = Alcotest.test_case name `Quick body

let stone =
  Material.make ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 1 1 1))

let square half =
  P.corners
    [
      Vec.make (-.half) (-.half);
      Vec.make half (-.half);
      Vec.make half half;
      Vec.make (-.half) half;
    ]

(* Made once, at the top level, because a door carries the identity a
   connection joins by and one made inside a render is a different door every
   frame. *)
let plaza_east = P.door ~name:"east" ~width:1.6 ~clearance:2.2 ()
let hall_west = P.door ~name:"west" ~width:1.6 ~clearance:2.2 ()
let plaza_north = P.door ~name:"north" ~width:1.6 ~clearance:2.2 ()

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:(square 4.)
          [
            spawn (Vec.make 0. 0.);
            cut plaza_east ~along:(Vec.make 4. (-4.), Vec.make 4. 4.);
            cut plaza_north ~along:(Vec.make 4. 4., Vec.make (-4.) 4.);
          ];
        room ~name:"hall" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:(square 3.)
          [ cut hall_west ~along:(Vec.make (-3.) (-3.), Vec.make (-3.) 3.) ];
      ])

let forest () =
  let taken = ref [] in
  let mount = Mount.create () in
  ignore
    (Mount.render mount level ~patch:(fun forest ->
         taken := forest;
         forest));
  Mount.destroy mount;
  !taken

let graph () = Camlcast_edit.Graph.read (forest ())

(* Whether this build carries positions, from dune rather than from the
   rewriter — see test/ppx/dune, and test_ppx.ml, which draws the same
   distinction for the same reason. Joining needs them: what a connection has
   to name is the binding a doorway was made under, and only the source says
   what that is. A release build carries none by design, so join refuses, and
   refusing is the behaviour to assert there. *)
let positioned =
  match Sys.getenv_opt "CAMLCAST_PROFILE" with
  | Some "release" -> false
  | Some _ | None -> true

let reader path =
  let rec attempt = function
    | [] -> Error (`Msg (path ^ ": not found from " ^ Sys.getcwd ()))
    | candidate :: rest -> (
        match In_channel.with_open_bin candidate In_channel.input_all with
        | source -> Ok source
        | exception Sys_error _ -> attempt rest)
  in
  attempt [ Filename.basename path; path ]

let source () =
  match reader "test_graph.ml" with
  | Ok source -> source
  | Error (`Msg m) -> Alcotest.failf "cannot read this suite's source: %s" m

let parsed () =
  match Camlcast_edit.Span.parse ~path:"test_graph.ml" (source ()) with
  | Ok parsed -> parsed
  | Error (`Msg m) -> Alcotest.failf "this suite does not parse: %s" m

(* Whether [needle] appears anywhere in [text]. *)
let holds text needle =
  let rec find index =
    index + String.length needle <= String.length text
    && (String.sub text index (String.length needle) = needle
       || find (index + 1))
  in
  find 0

let the_world_call () =
  match
    List.find_opt
      (fun (c : Camlcast_edit.Span.call) -> c.callee = "world")
      (Camlcast_edit.Span.calls (parsed ()))
  with
  | Some call -> call
  | None -> Alcotest.fail "no world call in this suite's source"

let door_named name =
  let g = graph () in
  match
    List.concat_map (fun (r : Camlcast_edit.Graph.room) -> r.doors) g.rooms
    |> List.find_opt (fun (d : Camlcast_edit.Graph.door) -> d.name = Some name)
  with
  | Some door -> door
  | None -> Alcotest.failf "no doorway named %S" name

(* {1 Reading} *)

let the_graph_holds_the_rooms_and_their_doorways () =
  let g = graph () in
  Alcotest.(check (list (pair (option string) int)))
    "two rooms, with two doorways and one"
    [ (Some "plaza", 2); (Some "hall", 1) ]
    (List.map
       (fun (r : Camlcast_edit.Graph.room) -> (r.name, List.length r.doors))
       g.rooms);
  Alcotest.(check (list string))
    "and every doorway leads nowhere yet, which is a state and not a mistake"
    [ "east"; "north"; "west" ]
    (List.concat_map (fun (r : Camlcast_edit.Graph.room) -> r.doors) g.rooms
    |> List.filter (fun (d : Camlcast_edit.Graph.door) -> not d.joined)
    |> List.filter_map (fun (d : Camlcast_edit.Graph.door) -> d.name))

(* {1 Placing} *)

let the_layout_does_not_move () =
  let g = graph () in
  let once = Camlcast_edit.Graph.place g ~x:0 ~y:0 ~width:200 ~height:200 in
  let twice = Camlcast_edit.Graph.place g ~x:0 ~y:0 ~width:200 ~height:200 in
  Alcotest.(check (list (pair int int)))
    "the same picture until the world itself changes"
    (List.map (fun (p : Camlcast_edit.Graph.placed) -> (p.x, p.y)) once)
    (List.map (fun (p : Camlcast_edit.Graph.placed) -> (p.x, p.y)) twice)

let a_room_is_found_where_it_was_placed () =
  let g = graph () in
  let view = Camlcast_edit.Graph.place g ~x:0 ~y:0 ~width:200 ~height:200 in
  let first = List.hd view in
  match Camlcast_edit.Graph.hit view (first.x, first.y) with
  | Room placed ->
      Alcotest.(check (option string))
        "the room at its own middle" (Some "plaza") placed.room.name
  | Door _ -> Alcotest.fail "the middle of a room is not a doorway"
  | Nothing -> Alcotest.fail "nothing found where the room was placed"

(* {1 Joining} *)

let joining_two_stubs_writes_a_connection () =
  let parsed = parsed () in
  match
    Camlcast_edit.Graph.join parsed ~world:(the_world_call ())
      (door_named "east") (door_named "west")
  with
  | Error (`Msg m) when positioned -> Alcotest.failf "join refused: %s" m
  | Error (`Msg m) ->
      (* Without positions there is no binding to name, and saying so is the
         right answer rather than a failure. *)
      Alcotest.(check bool) "refused, and says why" true (holds m "no position")
  | Ok ((_, written_line) as edit) -> (
      (* Asserted on the inserted text rather than on the file it lands in.
         The fixture here is this suite's own source, so a needle looked for in
         the whole file would be found in the line that looks for it. *)
      Alcotest.(check bool)
        "names the bindings the doorways were made under, not the names the \
         description gave them"
        true
        (holds written_line "connect plaza_east hall_west");
      (* No leading separator here: this fixture's list of children already
         ends in one. A list that does not gets a "; " in front, which is the
         whole of what that arithmetic is for. *)
      Alcotest.(check bool)
        "and does not double the separator the list already has" false
        (holds written_line ";;" || holds written_line "; ;");
      match Camlcast_edit.Span.splice parsed [ edit ] with
      | Error (`Msg m) -> Alcotest.failf "splice refused: %s" m
      | Ok written ->
          (* And what it wrote is OCaml, which is the claim that matters. *)
          Alcotest.(check bool)
            "what was written still parses" true
            (match Camlcast_edit.Span.parse ~path:"test_graph.ml" written with
            | Ok _ -> true
            | Error _ -> false);
          Alcotest.(check bool)
            "and the connection is in it" true
            (holds written "connect plaza_east hall_west"))

let a_doorway_cannot_be_joined_to_itself () =
  let east = door_named "east" in
  Alcotest.(check bool)
    "refused in the editor's terms, not as a world that will not assemble" true
    (match
       Camlcast_edit.Graph.join (parsed ()) ~world:(the_world_call ()) east east
     with
    | Error _ -> true
    | Ok _ -> false)

let () =
  Alcotest.run "Graph"
    [
      ( "reading",
        [
          case "the graph holds the rooms and their doorways"
            the_graph_holds_the_rooms_and_their_doorways;
        ] );
      ( "placing",
        [
          case "the layout does not move" the_layout_does_not_move;
          case "a room is found where it was placed"
            a_room_is_found_where_it_was_placed;
        ] );
      ( "joining",
        [
          case "joining two stubs writes a connection"
            joining_two_stubs_writes_a_connection;
          case "a doorway cannot be joined to itself"
            a_doorway_cannot_be_joined_to_itself;
        ] );
    ]
