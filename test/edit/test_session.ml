(** The overlay's own state, and what it says.

    Driven without a window. What a session holds between frames — which panel
    is up, which room the plan is of, what is not yet written — is a value, and
    the panel's contents are a list of lines before they are pixels. Both can
    be read here; what cannot is how it feels to use, which is what the worked
    example is for. *)

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

let east = P.door ~name:"east" ~width:1.2 ~clearance:2. ()

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:(square 4.)
          [
            spawn (Vec.make 0. 0.);
            cut east ~along:(Vec.make 4. (-4.), Vec.make 4. 4.);
          ];
        room ~name:"cellar" ~height:2. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:(square 2.) [];
      ])

(* Frames through the session's own watch, which is how a session comes to know
   anything: the patch is handed the committed forest.

   One mount across all of them, because that is what a run is. Rendering into
   a fresh mount each time would mount every component again at the same paths
   — which is a remount, is counted as one, and would have every row of the
   tree reporting a fault this fixture does not have. *)
let frames session count =
  let mount = Mount.create () in
  let watch = Camlcast_edit.Session.watch session in
  for _ = 1 to count do
    ignore
      (Mount.render mount level ?trace:watch.Watch.trace
         ?patch:watch.Watch.patch)
  done;
  Mount.destroy mount

let render session = frames session 1

let session () =
  Camlcast_edit.Session.create
    ~read:(fun path -> Error (`Msg (path ^ ": this test has no files")))
    ~write:(fun _ _ -> Ok ())
    ()

let texts session =
  List.map
    (fun (line : Camlcast_edit.Panel.line) -> line.text)
    (Camlcast_edit.Session.lines session)

let a_closed_session_says_nothing () =
  let s = session () in
  render s;
  Alcotest.(check bool)
    "closed to begin with" false
    (Camlcast_edit.Session.is_open s);
  Alcotest.(check (list string)) "and draws nothing at all" [] (texts s)

let asking_for_a_panel_opens_it () =
  let s = session () in
  Camlcast_edit.Session.show s Camlcast_edit.Session.Graph;
  Alcotest.(check bool)
    "asking to see one is asking to see it" true
    (Camlcast_edit.Session.is_open s);
  Alcotest.(check bool)
    "and it is the one asked for" true
    (Camlcast_edit.Session.panel s = Camlcast_edit.Session.Graph)

let the_plan_lists_one_room_at_a_time () =
  let s = session () in
  render s;
  Camlcast_edit.Session.show s Camlcast_edit.Session.Plan;
  let plaza = texts s in
  Camlcast_edit.Session.show_room s 1;
  let cellar = texts s in
  Alcotest.(check bool)
    "the plaza's spawn is on its own plan" true
    (List.exists (fun t -> t = "spawn at (0,0)") plaza);
  Alcotest.(check bool)
    "and not on the cellar's" false
    (List.exists (fun t -> t = "spawn at (0,0)") cellar);
  Alcotest.(check bool)
    "each says which room it is of" true
    (List.hd plaza = "plan  room 0" && List.hd cellar = "plan  room 1")

let the_graph_says_which_doorways_lead_nowhere () =
  let s = session () in
  render s;
  Camlcast_edit.Session.show s Camlcast_edit.Session.Graph;
  let said = texts s in
  Alcotest.(check bool)
    "both rooms are named" true
    (List.exists (fun t -> t = "plaza") said
    && List.exists (fun t -> t = "cellar") said);
  Alcotest.(check bool)
    "and the one doorway is reported unjoined, which is a state and not a fault"
    true
    (List.exists (fun t -> t = "  east leads nowhere") said)

let the_tree_is_what_the_description_came_to () =
  let s = session () in
  frames s 3;
  Camlcast_edit.Session.show s Camlcast_edit.Session.Tree;
  let said = texts s in
  Alcotest.(check bool)
    "the world at the margin" true
    (List.exists (fun t -> t = "world") said);
  Alcotest.(check bool)
    "and its rooms indented under it" true
    (List.exists (fun t -> t = "  room plaza") said);
  (* Three frames of a description that never changes shape: nothing should be
     reported as having remounted, and a row that had would carry a count. *)
  Alcotest.(check bool)
    "with nothing reported as rebuilt" false
    (List.exists
       (fun t ->
         (* The mark is an x and a count -- "  x3" -- so the digit is part of
            what is looked for. Without it this matches any x after a space,
            which the panel's own legend has. *)
         let rec holds i =
           i + 3 <= String.length t
           && ((String.sub t i 2 = " x"
               && match t.[i + 2] with '0' .. '9' -> true | _ -> false)
              || holds (i + 1))
         in
         holds 0)
       said)

(* A count above zero means the screen and the source agree and the built
   program does not yet, which is the one thing a panel must never be quiet
   about. *)
let it_says_when_something_is_not_written_yet () =
  let s = session () in
  render s;
  Camlcast_edit.Session.show s Camlcast_edit.Session.Plan;
  Alcotest.(check bool)
    "nothing outstanding to begin with" true
    (List.exists (fun t -> t = "saved") (texts s));
  Alcotest.(check int) "and none counted" 0 (Camlcast_edit.Session.pending s)

let nothing_to_take_back_is_a_state () =
  let s = session () in
  Alcotest.(check bool)
    "asked and answered" true
    (match Camlcast_edit.Session.undo s with Ok None -> true | _ -> false)

let () =
  Alcotest.run "Session"
    [
      ( "what it is doing",
        [
          case "a closed session says nothing" a_closed_session_says_nothing;
          case "asking for a panel opens it" asking_for_a_panel_opens_it;
          case "nothing to take back is a state" nothing_to_take_back_is_a_state;
        ] );
      ( "what it says",
        [
          case "the plan lists one room at a time"
            the_plan_lists_one_room_at_a_time;
          case "the graph says which doorways lead nowhere"
            the_graph_says_which_doorways_lead_nowhere;
          case "the tree is what the description came to"
            the_tree_is_what_the_description_came_to;
          case "it says when something is not written yet"
            it_says_when_something_is_not_written_yet;
        ] );
    ]
