(* Reading a description for what a compiler cannot see.

   Every case here is a world that builds, or nearly does, and is wrong. What is
   asserted is the summary line each one produces, because that line is the
   whole product: a diagnostic nobody can act on is worth no more than the bare
   Invalid_argument it replaced. *)

open Camlcast_core
open Camlcast
open Support

let summaries description =
  List.map (fun (d : Check.t) -> d.Check.summary) (Check.report description)

let severities description =
  List.map
    (fun (d : Check.t) ->
      match d.Check.severity with
      | Check.Error -> "error"
      | Check.Warning -> "warning")
    (Check.report description)

let lines = Alcotest.(list string)

(* Details, for the one case below that is about the detail. The file
   otherwise asserts summaries on purpose (see the header). This helper exists
   because a detail once misstated the engine's own vocabulary. A sentence
   that teaches a game developer the words is worth pinning once. *)
let details report =
  List.concat_map (fun (d : Check.t) -> d.Check.detail) report

(* {!Support.mentions} takes the haystack first; this is it over a list of
   lines, which is the shape a detail comes in. *)
let said_anywhere lines needle =
  List.exists (fun line -> mentions line needle) lines

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let height = 4.
let flat = Plane.horizontal 0.
let floor_at z = P.floor ~plane:(Plane.horizontal z) stone
let floor = floor_at 0.
let ceiling = P.roof ~plane:(Plane.above flat height) stone

(* Two rooms, each a closed outline, sharing the leg between them. Every world
   below is a variation of this. *)
let west_outline ?(corner = Vec.make 0. (-4.)) () =
  P.corners [ corner; Vec.make 0. 4.; Vec.make (-6.) 4.; Vec.make (-6.) (-4.) ]

let east_outline =
  P.corners
    [ Vec.make 0. (-4.); Vec.make 6. (-4.); Vec.make 6. 4.; Vec.make 0. 4. ]

let opening ?name ?(width = 2.) () = P.door ?name ~width ~clearance:2.5 ()
let west_leg = (Vec.make 0. (-4.), Vec.make 0. 4.)
let east_leg = (Vec.make 0. 4., Vec.make 0. (-4.))

let west_room ?key ?name ?corner ?leaf ?(floor = floor)
    ?(spawn_at = Vec.make (-3.) 0.) ?(holding = []) door =
  P.room ?key ?name ~height ~material:stone ~floor ~ceiling
    ~outline:(west_outline ?corner ())
    (P.spawn spawn_at :: P.cut door ?leaf ~along:west_leg :: holding)

let east_room ?name ?leaf ?(floor = floor) ?(holding = []) door =
  P.room ?name ~height ~material:stone ~floor ~ceiling ~outline:east_outline
    (P.cut door ?leaf ~along:east_leg :: holding)

(* One pair of doors, shared by the worlds below. Each of them is built on its
   own, so the same two identities standing for the same opening in all of them
   is no more a collision than the same corners are. *)
let west_way = opening ~name:"east" ()
let east_way = opening ~name:"west" ()

(* The two rooms joined, and nothing wrong with them. *)
let good =
  P.world ~atmosphere:Atmosphere.default
    [ west_room west_way; east_room east_way; P.connect west_way east_way ]

let nothing_to_report =
  [
    case "a world that is right says nothing" (fun () ->
        Alcotest.check lines "silence" [] (summaries good));
    case "and formats as such" (fun () ->
        Alcotest.(check string)
          "one line" "nothing to report"
          (Check.format (Check.report good)));
  ]

let structure =
  [
    case "a description with no world in it" (fun () ->
        Alcotest.check lines "said plainly"
          [ "a wall (0,0)-(1,0) is not a world" ]
          (summaries
             (P.wall ~height ~material:stone (Vec.make 0. 0.) (Vec.make 1. 0.))));
    case "a room inside a room" (fun () ->
        Alcotest.check lines "the inner one is the complaint"
          [ "a room inner cannot go in a room" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  P.room ~name:"outer" ~floor ~ceiling
                    [
                      P.spawn (Vec.make 0. 0.);
                      P.room ~name:"inner" ~floor ~ceiling [];
                    ];
                ])));
    case "a sprite where a room should be" (fun () ->
        Alcotest.check lines "not in a world"
          [ "a sprite (0,0) cannot go in a world" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [ P.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ])));
    case "a sprite hung on a wall" (fun () ->
        Alcotest.check lines "only decals go there"
          [ "a sprite (0,0) cannot go on a wall" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  P.room ~name:"room" ~floor ~ceiling
                    [
                      P.wall ~height ~material:stone
                        ~decals:
                          [ P.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ]
                        (Vec.make 0. 0.) (Vec.make 1. 0.);
                    ];
                ])));
  ]

let naming =
  [
    case "two rooms of the same name" (fun () ->
        Alcotest.check lines "the second one is the duplicate"
          [ {|there is already a room called "west"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  P.room ~name:"west" ~height ~material:stone ~floor ~ceiling
                    ~outline:(west_outline ())
                    [ P.spawn (Vec.make (-3.) 0.) ];
                  P.room ~name:"west" ~height ~material:stone ~floor ~ceiling
                    ~outline:east_outline [];
                ])));
    case "two doorways of the same name in one room" (fun () ->
        Alcotest.check lines "a link could not tell them apart"
          [ {|there is already a doorway called "east"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way
                    ~holding:
                      [
                        P.cut (opening ~name:"east" ())
                          ~along:(Vec.make (-6.) (-4.), Vec.make (-6.) 4.);
                      ];
                ])));
  ]

(* The link section was here. Its diagnostics — a link naming a room that is
   not there, a room with no doorway of that name — existed because a link
   joined two doorways by the names their rooms gave them. A connection is
   handed the two doors, so there is no name in it to be wrong, and the cases
   went with the form. What is left of joining a pair is whether the two sides
   agree and whether each is joined exactly once, which "connections" below
   asks. *)

let refused_in_the_same_words name description =
  case name (fun () ->
      let said = summaries description in
      Alcotest.(check bool)
        "the checker has something to say about it" true (said <> []);
      match Mount.build description with
      | _ -> Alcotest.fail "the engine built what the checker refused"
      | exception Host.Malformed message ->
          Alcotest.(check bool)
            (Printf.sprintf "the engine's %S is one the checker wrote" message)
            true
            (List.exists (fun s -> mentions message s) said))

let agrees_with_the_engine =
  let pair ?east_floor ?dw ?de ?(w = 2.) ?(e = 2.) () =
    let a = opening ~name:"east" ~width:w ()
    and b = opening ~name:"west" ~width:e () in
    P.world ~atmosphere:Atmosphere.default
      [
        west_room a ?leaf:dw;
        east_room b ?leaf:de ?floor:east_floor;
        P.connect a b;
      ]
  in
  [
    refused_in_the_same_words "a sprite where a room should be"
      (P.world ~atmosphere:Atmosphere.default
         [ P.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ]);
    refused_in_the_same_words "a room inside a room"
      (P.world ~atmosphere:Atmosphere.default
         [
           P.room ~name:"outer" ~floor ~ceiling
             [ P.room ~name:"inner" ~floor ~ceiling [] ];
         ]);
    refused_in_the_same_words "a description that is not a world at all"
      (P.wall ~height ~material:stone (Vec.make 0. 0.) (Vec.make 1. 0.));
    case "a width difference the engine tolerates is not a complaint" (fun () ->
        (* 1e-7 is inside World's epsilon of 1e-6, so this world builds and
           runs. Reporting it produced a fatal-sounding error about a world
           with nothing wrong with it. *)
        Alcotest.check lines "nothing to say" []
          (summaries (pair ~e:(2. +. 1e-7) ())));
    case "a width difference the engine refuses still is" (fun () ->
        Alcotest.check lines "1e-3 is outside the tolerance"
          [ "the two sides of this opening are different widths" ]
          (summaries (pair ~e:(2. +. 1e-3) ())));
    case "a doorway too narrow to join is named here" (fun () ->
        (* Below World's epsilon, so the engine refuses it. Named here rather
           than left to arrive as the engine's own message under "the engine
           refused to build this world", which says where but not what. *)
        Alcotest.check lines "once for each side"
          [
            "this doorway is too narrow to join";
            "this doorway is too narrow to join";
          ]
          (summaries (pair ~w:5e-7 ~e:5e-7 ())));
    case "two sides disagreeing about an open door" (fun () ->
        (* Both sides have a leaf, so a check on presence alone saw nothing and
           the engine refused it afterwards on the state. *)
        Alcotest.check lines "the state, not just the leaf"
          [
            "the two sides of this opening disagree about whether the door is \
             open";
          ]
          (summaries
             (pair
                ~dw:(Door.make ~state:Door.Open stone)
                ~de:(Door.make ~state:Door.Closed stone)
                ())));
    case "a door on one side and none on the other, still" (fun () ->
        Alcotest.check lines "the presence case is not lost"
          [ "one side of this opening has a door and the other does not" ]
          (summaries (pair ~dw:(Door.make stone) ())));
    case "a tolerated difference hides nothing below it" (fun () ->
        (* The tiers short-circuit: a link complaint stops the checks only an
           assembled world can answer. So a spurious link complaint used to
           cost the reader every diagnostic behind it. *)
        Alcotest.check lines "the seam is still reported"
          [
            {|the floor steps by 0.5 through the doorway "east"|};
            {|the floor steps by 0.5 through the doorway "west"|};
          ]
          (summaries (pair ~east_floor:(floor_at 0.5) ~e:(2. +. 1e-7) ())));
    case "a primitive refusing inside a component is reported, not raised"
      (fun () ->
        (* The doorway is wider than the wall it is cut into, which
           Room.doorway refuses. Built inside a component, where a game builds
           one, the refusal has to be caught and reported: a module written to
           replace a crash must not come out of Check.report as
           Invalid_argument. *)
        let bad =
          Camlcast_loom.Element.declare ~name:"BadRoom" @@ fun () ->
          west_room (opening ~name:"east" ~width:100. ())
        in
        let report =
          Check.report
            (P.world ~atmosphere:Atmosphere.default
               [ bad (); east_room east_way; P.connect west_way east_way ])
        in
        Alcotest.check lines "reported"
          [ "the engine refused to build this world" ]
          (List.map (fun (d : Check.t) -> d.Check.summary) report);
        (* The component is named, but in the message rather than in [where].
           A cut is inert until assembly, so the refusal no longer happens
           inside the component's own render the way Room.doorway's did — it
           happens when the room is built, and the layer puts the path in the
           sentence because that is the only place left to put it. *)
        Alcotest.(check bool)
          "and the component it came out of is named" true
          (said_anywhere (details report) "BadRoom"));
  ]

(* What only an assembled world can answer. Each of these builds cleanly. *)
(* A threshold whose end meets no wall. A cut door cannot leave one: its jambs
   are legs of the outline it is cut from, so its ends are wall ends by
   construction. Only a bare {!Camlcast_core.Room.threshold} can, which is what
   a generator composing {!Camlcast_core.Room.make} does — so the world is built
   there and read by {!Check.assembled}, the half of Check that takes a world
   rather than a description. *)
let with_a_gap () =
  let gate =
    Room.threshold ~name:"east" ~height:2.5 (Vec.make 0. (-4.)) (Vec.make 0. 4.)
  in
  let short =
    Room.make ~thresholds:[ gate ]
      ~floor:(Room.floor ~plane:flat ~material:stone)
      ~ceiling:(Room.roof ~plane:(Plane.above flat height) ~material:stone)
      (Room.path ~closed:false ~height ~material:stone
         [
           Vec.make 0. (-3.);
           Vec.make (-6.) (-4.);
           Vec.make (-6.) 4.;
           Vec.make 0. 4.;
         ])
  in
  World.make
    ~rooms:[ ("west", short) ]
    ~links:[] ~atmosphere:Atmosphere.default
    ~spawn:("west", Vec.make (-3.) 0.)

let assembled_summaries world =
  List.filter_map
    (fun (d : Check.t) ->
      match d.Check.severity with
      | Check.Error -> Some d.Check.summary
      | Check.Warning -> None)
    (Check.assembled world)

let the_world_it_makes =
  [
    (* A spawn is a child of the room it is in, so it cannot name a room that
       is not there and the diagnostic that said so is gone. The two ways it
       can still be wrong are these. *)
    case "nothing says where the player starts" (fun () ->
        Alcotest.check lines "at the root, because no component wrote it"
          [ "this description does not say where the player starts" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  P.room ~name:"west" ~height ~material:stone ~floor ~ceiling
                    ~outline:(west_outline ()) [];
                ])));
    case "and two of them saying it" (fun () ->
        Alcotest.check lines "the second one, the way an overruled camera is"
          [ "this description says twice where the player starts" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way ~holding:[ P.spawn (Vec.make (-2.) 0.) ];
                  east_room east_way;
                  P.connect west_way east_way;
                ])));
    case "two cameras, and the one that is not being listened to" (fun () ->
        (* Host takes the last and says nothing about the rest, so the ones it
           dropped are exactly what a reader of the description cannot see.

           There is no longer a camera that names a room wrongly: a camera is a
           child of the room it looks from, so the room it means is the room it
           was written in. The pair of cases that checked that name are gone
           with the name. *)
        Alcotest.check lines "the earlier one, named"
          [ "this camera is overruled by a later one" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way
                    ~holding:
                      [
                        P.camera ~pos:(Vec.make (-3.) 0.) ~angle:0. ();
                        P.camera ~pos:(Vec.make (-2.) 0.) ~angle:0. ();
                      ];
                  east_room east_way;
                  P.connect west_way east_way;
                ])));
    case "which is a warning, because the world still runs" (fun () ->
        Alcotest.check lines "one of the two is obeyed" [ "warning" ]
          (severities
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way
                    ~holding:
                      [
                        P.camera ~pos:(Vec.make (-3.) 0.) ~angle:0. ();
                        P.camera ~pos:(Vec.make (-2.) 0.) ~angle:0. ();
                      ];
                  east_room east_way;
                  P.connect west_way east_way;
                ])));
    case "which leaves the world buildable, and so still checked" (fun () ->
        (* A warning and not an error, because an error stops the tiers below
           it: raise the severity here and a dead camera takes the geometry
           checks down with it. This world's spawn is in a wall, and the report
           has to reach that. *)
        Alcotest.check lines "the camera, and the thing behind it"
          [
            "the player starts inside a wall";
            "this camera is overruled by a later one";
          ]
          (List.sort compare
             (summaries
                (P.world ~atmosphere:Atmosphere.default
                   [
                     west_room west_way ~spawn_at:(Vec.make (-6.) 0.)
                       ~holding:
                         [
                           P.camera ~pos:(Vec.make (-3.) 0.) ~angle:0. ();
                           P.camera ~pos:(Vec.make (-2.) 0.) ~angle:0. ();
                         ];
                     east_room east_way;
                     P.connect west_way east_way;
                   ]))));
    case "two children under one key are reported, not thrown" (fun () ->
        (* Reconciling refuses this outright, which is a crash where a check is
           supposed to be a report. *)
        Alcotest.check lines "named where the pair of them sit"
          [ {|two of these children are keyed "side"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way
                    ~holding:
                      [
                        P.cut (opening ~name:"south" ()) ~key:"side"
                          ~along:(Vec.make (-6.) (-4.), Vec.make (-6.) 4.);
                        P.cut (opening ~name:"north" ()) ~key:"side"
                          ~along:(Vec.make (-6.) 4., Vec.make 0. 4.);
                      ];
                  east_room east_way;
                  P.connect west_way east_way;
                ])));
    case "the player starts inside a wall" (fun () ->
        Alcotest.check lines "the first step would be refused"
          [ "the player starts inside a wall" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way ~spawn_at:(Vec.make (-6.) 0.);
                  east_room east_way;
                  P.connect west_way east_way;
                ])));
    case "a room nothing leads to" (fun () ->
        Alcotest.check lines "content nobody can reach"
          [ "no doorway leads to this room" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                [
                  west_room west_way;
                  east_room east_way;
                  (* No doorways at all, so nothing is unlinked and nothing
                     reaches it either. *)
                  P.room ~name:"cellar" ~height ~material:stone ~floor ~ceiling
                    ~outline:
                      (P.corners
                         [
                           Vec.make 10. 0.;
                           Vec.make 14. 0.;
                           Vec.make 14. 4.;
                           Vec.make 10. 4.;
                         ])
                    [];
                  P.connect west_way east_way;
                ])));
    case "a doorway with a gap beside it" (fun () ->
        Alcotest.check lines "the corner meets nothing"
          [ {|the doorway "east" has a corner that meets no wall|} ]
          (assembled_summaries (with_a_gap ())));
    case "and the reason it gives keeps the two words apart" (fun () ->
        (* The two words are not interchangeable, and this detail is where they
           are easiest to run together. A doorway is an opening {e and its
           jambs} (see {!Camlcast_core.Room}), and being made with its jambs is
           why a cut one cannot be the thing this complaint is about. Only a
           bare threshold can, so the explanation has to say which of the two
           makes this gap and which cannot. *)
        let said = details (Check.assembled (with_a_gap ())) in
        Alcotest.(check bool)
          "it names the form that cannot leave one" true
          (said_anywhere said "outline");
        Alcotest.(check bool)
          "and the form that can" true
          (said_anywhere said "Room.threshold");
        Alcotest.(check bool)
          "and does not define a doorway as an opening" false
          (said_anywhere said "A doorway is an opening"));
    case "a step in the floor is a warning and not an error" (fun () ->
        let stepped =
          P.world ~atmosphere:Atmosphere.default
            [
              west_room west_way;
              east_room east_way ~floor:(floor_at 0.5);
              P.connect west_way east_way;
            ]
        in
        Alcotest.check lines "measured, so it can be judged"
          [
            {|the floor steps by 0.5 through the doorway "east"|};
            {|the floor steps by 0.5 through the doorway "west"|};
          ]
          (List.sort compare (summaries stepped));
        Alcotest.check lines "it draws and it is walkable"
          [ "warning"; "warning" ] (severities stepped));
  ]

(* {!Check.assembled} and {!Camlcast_core.World.check} are the same two words
   the other way round, and running one is not running the other. They overlap
   in nothing. World.check asserts what World.make guarantees, over a world
   grown by add_room and link instead of made in one go, and raises on the
   first break. Check.assembled reads a world that is already sound for the
   four ways it can still be wrong to play, and hands them back.

   Both directions are tested, because knowing only one of them invites
   thinking the world is fully checked. *)
let the_other_check =
  let world_of description = (Mount.build description).Scene.world in
  [
    case "a structural break is World.check's alone" (fun () ->
        (* A second doorway cut into the first room and left unlinked. The room
           is still reached through the doorway that is linked, its corners
           still meet walls, the floors still agree and the spawn is still
           clear, so all four of Check.assembled's questions answer well. *)
        let first = World.room two_rooms 0 in
        let jambs, extra =
          Room.doorway ~name:"unfinished" ~width:1. ~opening:2. ~height:3.
            ~material:stone (Vec.make 0. 0.) (Vec.make 4. 0.)
        in
        let grown =
          World.open_doorway two_rooms ~room:0
            ~opened:
              (Room.make
                 ~thresholds:
                   (List.init
                      (Room.threshold_count first)
                      (Room.threshold_at first)
                   @ [ extra ])
                 ~floor:(Room.floor_surface first) ~ceiling:(Room.ceiling first)
                 (List.init (Room.wall_count first) (Room.wall_at first) @ jambs))
        in
        Alcotest.check lines "Check.assembled has nothing to say" []
          (List.map
             (fun (d : Check.t) -> d.Check.summary)
             (Check.assembled grown));
        (* Neither check's business now: a door that nothing connects is a
           level part-built, and Check.report is where it is reported, as a
           warning rather than a break. *)
        World.check grown);
    case "and a step in the floor is Check.assembled's alone" (fun () ->
        let stepped =
          world_of
            (P.world ~atmosphere:Atmosphere.default
               [
                 west_room west_way;
                 east_room east_way ~floor:(floor_at 0.5);
                 P.connect west_way east_way;
               ])
        in
        Alcotest.check lines "Check.assembled measures it"
          [
            {|the floor steps by 0.5 through the doorway "east"|};
            {|the floor steps by 0.5 through the doorway "west"|};
          ]
          (List.sort compare
             (List.map
                (fun (d : Check.t) -> d.Check.summary)
                (Check.assembled stepped)));
        (* And World.check is satisfied: every invariant it knows about holds
           in a world nobody can walk through without the camera jolting. *)
        World.check stepped);
  ]

(* A diagnostic names the component that wrote the offending part, which is the
   whole difference from the message the engine raised before. *)
let where_it_says =
  let gallery =
    Camlcast_loom.Element.declare ~name:"gallery" @@ fun () ->
    west_room west_way ~spawn_at:(Vec.make (-6.) 0.)
  in
  let annexe =
    Camlcast_loom.Element.declare ~name:"annexe" @@ fun () -> east_room east_way
  in
  [
    case "the component, not the room" (fun () ->
        let report =
          Check.report
            (P.world ~atmosphere:Atmosphere.default
               [ gallery (); annexe (); P.connect west_way east_way ])
        in
        Alcotest.check lines "named by where it was written" [ "gallery" ]
          (List.map (fun (d : Check.t) -> d.Check.where) report));
    case "the component that placed the camera" (fun () ->
        (* A spawn is an argument of the world and belongs to no component, so
           it can only say "(root)". A camera is written somewhere. *)
        let eye =
          Camlcast_loom.Element.declare ~name:"eye" @@ fun () ->
          P.camera ~pos:(Vec.make 0. 0.) ~angle:0. ()
        in
        let report =
          Check.report
            (P.world ~atmosphere:Atmosphere.default
               [ gallery (); annexe (); P.connect west_way east_way; eye () ])
        in
        Alcotest.check lines "named by where it was written" [ "eye" ]
          (List.map (fun (d : Check.t) -> d.Check.where) report));
  ]

let () =
  Alcotest.run "Check"
    [
      ("nothing to report", nothing_to_report);
      ("structure", structure);
      ("naming", naming);
      ("agrees with the engine", agrees_with_the_engine);
      ("the world it makes", the_world_it_makes);
      ("the other check", the_other_check);
      ("where it says", where_it_says);
    ]
