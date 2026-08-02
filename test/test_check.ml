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

(* Details, for the one case below that is about the detail. The file otherwise
   asserts summaries on purpose — see the header — and this does not change
   that: it is here because a detail said something about the engine's own
   vocabulary that was not true, and a sentence a game developer is taught the
   words by is worth pinning once. *)
let details description =
  List.concat_map
    (fun (d : Check.t) -> d.Check.detail)
    (Check.report description)

(* {!Support.mentions} takes the haystack first; this is it over a list of
   lines, which is the shape a detail comes in. *)
let said_anywhere lines needle =
  List.exists (fun line -> mentions line needle) lines

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let height = 4.
let flat = Plane.horizontal 0.
let floor_at z = Room.floor ~plane:(Plane.horizontal z) ~material:stone
let floor = floor_at 0.
let ceiling = Room.roof ~plane:(Plane.above flat height) ~material:stone

(* Three sides run open and the fourth cut, which together close the boundary.
   Everything below is this, bent one way or another. *)
let west_side ?key ?(corner = Vec.make 0. (-4.)) () =
  P.boundary ~closed:false ?key ~height ~material:stone
    (P.corners
       [ Vec.make 0. 4.; Vec.make (-6.) 4.; Vec.make (-6.) (-4.); corner ])

let east_side =
  P.boundary ~closed:false ~height ~material:stone
    (P.corners
       [ Vec.make 0. (-4.); Vec.make 6. (-4.); Vec.make 6. 4.; Vec.make 0. 4. ])

let west_door ?key ?door ?(width = 2.) ?(name = "east") () =
  P.doorway ?key ?door ~name ~width ~opening:2.5 ~height ~material:stone
    (Vec.make 0. (-4.)) (Vec.make 0. 4.)

let east_door ?door ?(width = 2.) ?(name = "west") () =
  P.doorway ?door ~name ~width ~opening:2.5 ~height ~material:stone
    (Vec.make 0. 4.) (Vec.make 0. (-4.))

let good =
  P.world ~atmosphere:Atmosphere.default
    ~spawn:("west", Vec.make (-3.) 0.)
    [
      P.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ];
      P.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ];
      P.link ("west", "east") ("east", "west");
    ]

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
                ~spawn:("outer", Vec.make 0. 0.)
                [
                  P.room ~name:"outer" ~floor ~ceiling
                    [ P.room ~name:"inner" ~floor ~ceiling [] ];
                ])));
    case "a sprite where a room should be" (fun () ->
        Alcotest.check lines "not in a world"
          [ "a sprite (0,0) cannot go in a world" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("nowhere", Vec.make 0. 0.)
                [ P.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ])));
    case "a sprite hung on a wall" (fun () ->
        Alcotest.check lines "only decals go there"
          [ "a sprite (0,0) cannot go on a wall" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("room", Vec.make 0. 0.)
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
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling [ west_side () ];
                  P.room ~name:"west" ~floor ~ceiling [ east_side ];
                ])));
    case "two doorways of the same name in one room" (fun () ->
        Alcotest.check lines "a link could not tell them apart"
          [ {|there is already a doorway called "east"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [
                      west_side ();
                      west_door ();
                      P.doorway ~name:"east" ~width:2. ~opening:2.5 ~height
                        ~material:stone (Vec.make (-6.) (-4.))
                        (Vec.make (-6.) 4.);
                    ];
                ])));
  ]

let links =
  let two_rooms ~link_to =
    P.world ~atmosphere:Atmosphere.default
      ~spawn:("west", Vec.make (-3.) 0.)
      [
        P.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ];
        P.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ];
        P.link ("west", "east") link_to;
      ]
  in
  [
    case "a link to a room that is not there" (fun () ->
        Alcotest.check lines "named, so it can be looked for"
          [
            {|the doorway "west" leads nowhere|};
            {|this link names a room called "cellar"|};
          ]
          (List.sort compare (summaries (two_rooms ~link_to:("cellar", "up")))));
    case "a link to a doorway that is not there" (fun () ->
        Alcotest.check lines "the room exists and the doorway does not"
          [
            {|the doorway "west" leads nowhere|};
            {|the room "east" has no doorway called "north"|};
          ]
          (List.sort compare (summaries (two_rooms ~link_to:("east", "north")))));
    case "a doorway nothing links" (fun () ->
        Alcotest.check lines "both ends of an unmade link"
          [
            {|the doorway "east" leads nowhere|};
            {|the doorway "west" leads nowhere|};
          ]
          (List.sort compare
             (summaries
                (P.world ~atmosphere:Atmosphere.default
                   ~spawn:("west", Vec.make (-3.) 0.)
                   [
                     P.room ~name:"west" ~floor ~ceiling
                       [ west_side (); west_door () ];
                     P.room ~name:"east" ~floor ~ceiling
                       [ east_side; east_door () ];
                   ]))));
    case "a doorway two links claim" (fun () ->
        Alcotest.check lines "a place that cannot exist"
          [ {|the doorway "west" is linked 2 times|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door (); west_door ~name:"south" () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                  P.link ("west", "south") ("east", "west");
                ])));
    case "two sides of one doorway that are different widths" (fun () ->
        Alcotest.check lines "they are the same opening"
          [ "the two sides of this link are different widths" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door ~width:3. () ];
                  P.link ("west", "east") ("east", "west");
                ])));
    case "a door on one side and none on the other" (fun () ->
        Alcotest.check lines "one leaf hangs in one opening"
          [ "one side of this link has a door and the other does not" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door ~door:(Door.make stone) () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                ])));
  ]

(* Where this module has to say what the engine says, because a checker that
   answers a question differently from the thing it models is worse than no
   checker: it fails worlds that run and passes worlds that do not, and either
   way the reader stops believing it.

   Every case here was a disagreement. The tolerance ones are the reason
   World.has_length and its three neighbours are public — this file used to
   measure with its own 1e-9 against the engine's 1e-6 — and the last is the
   reason Element.Render_refused exists. *)
(* Agreeing about which descriptions are wrong is half of it. The other half is
   saying so in the same words, and by the time this was written the two shared
   the rule (Prim.may_contain), the traversal (Nesting.misplaced) and the nouns
   (Prim.describe, Prim.inside) — everything but the sentence. Host raised
   "... does not belong in a world" where the checker reported "a ... cannot go
   in a world": one offence under two names, which a developer who met one and
   then the other had no way to connect. Every part anyone had thought to share
   was shared, which is why the last part was not.

   Not written as the words themselves — those are pinned in "structure" above,
   and pinning them twice would only mean two places to edit. Written as the
   relation: whatever the engine puts in its exception has to contain what the
   checker puts in its summary. Containment and not equality because Host
   prefixes the path it was found at and the checker carries that in a field of
   its own, which is the one difference between them that is about the job
   rather than about the words. *)
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
    P.world ~atmosphere:Atmosphere.default
      ~spawn:("west", Vec.make (-3.) 0.)
      [
        P.room ~name:"west" ~floor ~ceiling
          [ west_side (); west_door ?door:dw ~width:w () ];
        P.room ~name:"east"
          ~floor:(Option.value east_floor ~default:floor)
          ~ceiling
          [ east_side; east_door ?door:de ~width:e () ];
        P.link ("west", "east") ("east", "west");
      ]
  in
  [
    refused_in_the_same_words "a sprite where a room should be"
      (P.world ~atmosphere:Atmosphere.default
         ~spawn:("nowhere", Vec.make 0. 0.)
         [ P.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ]);
    refused_in_the_same_words "a room inside a room"
      (P.world ~atmosphere:Atmosphere.default
         ~spawn:("outer", Vec.make 0. 0.)
         [
           P.room ~name:"outer" ~floor ~ceiling
             [ P.room ~name:"inner" ~floor ~ceiling [] ];
         ]);
    refused_in_the_same_words "a description that is not a world at all"
      (P.wall ~height ~material:stone (Vec.make 0. 0.) (Vec.make 1. 0.));
    case "a width difference the engine tolerates is not a complaint" (fun () ->
        (* 1e-7: inside World's epsilon of 1e-6, so this world builds and runs.
           Reported, it was a fatal-sounding error about a world with nothing
           wrong with it. *)
        Alcotest.check lines "nothing to say" []
          (summaries (pair ~e:(2. +. 1e-7) ())));
    case "a width difference the engine refuses still is" (fun () ->
        Alcotest.check lines "1e-3 is outside the tolerance"
          [ "the two sides of this link are different widths" ]
          (summaries (pair ~e:(2. +. 1e-3) ())));
    case "a doorway too narrow to link is named here" (fun () ->
        (* Below World's epsilon, so the engine refuses it. There was no check
           for this at all, and it arrived as the engine's own message under
           "the engine refused to build this world". *)
        Alcotest.check lines "once for each side"
          [
            "this doorway is too narrow to link";
            "this doorway is too narrow to link";
          ]
          (summaries (pair ~w:5e-7 ~e:5e-7 ())));
    case "two sides disagreeing about an open door" (fun () ->
        (* Both sides have a leaf, so a check on presence alone saw nothing and
           the engine refused it afterwards on the state. *)
        Alcotest.check lines "the state, not just the leaf"
          [
            "the two sides of this link disagree about whether the door is open";
          ]
          (summaries
             (pair
                ~dw:(Door.make ~state:Door.Open stone)
                ~de:(Door.make ~state:Door.Closed stone)
                ())));
    case "a door on one side and none on the other, still" (fun () ->
        Alcotest.check lines "the presence case is not lost"
          [ "one side of this link has a door and the other does not" ]
          (summaries (pair ~dw:(Door.make stone) ())));
    case "a tolerated difference hides nothing below it" (fun () ->
        (* The tiers short-circuit: a link complaint stops the checks only an
           assembled world can answer. So a spurious one cost the reader every
           diagnostic behind it, which is what this is really about. *)
        Alcotest.check lines "the seam is still reported"
          [
            {|the floor steps by 0.5 through the doorway "east"|};
            {|the floor steps by 0.5 through the doorway "west"|};
          ]
          (summaries (pair ~east_floor:(floor_at 0.5) ~e:(2. +. 1e-7) ())));
    case "a primitive refusing inside a component is reported, not raised"
      (fun () ->
        (* The doorway is wider than the wall it is cut into, which Room.doorway
           refuses. Built inside a component — where a game builds one — it used
           to come straight out of Check.report as Invalid_argument, so the
           module written to replace a crash ended in one. *)
        let bad =
          Camlcast_loom.Element.declare ~name:"BadRoom" @@ fun () ->
          P.room ~name:"west" ~floor ~ceiling
            [ west_side (); west_door ~width:100. () ]
        in
        let report =
          Check.report
            (P.world ~atmosphere:Atmosphere.default
               ~spawn:("west", Vec.make (-3.) 0.)
               [
                 bad ();
                 P.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ];
                 P.link ("west", "east") ("east", "west");
               ])
        in
        Alcotest.check lines "reported"
          [ "this part of the description was refused" ]
          (List.map (fun (d : Check.t) -> d.Check.summary) report);
        Alcotest.check lines "and named by the component it came out of"
          [ "#0/BadRoom#0" ]
          (List.map (fun (d : Check.t) -> d.Check.where) report));
  ]

(* What only an assembled world can answer. Each of these builds cleanly. *)
let the_world_it_makes =
  [
    case "the player starts in a room that is not there" (fun () ->
        Alcotest.check lines "named, so it can be looked for"
          [ {|the player starts in a room called "cellar"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("cellar", Vec.make 0. 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                ])));
    case "the camera is in a room that is not there" (fun () ->
        (* The same mistake as the spawn above, and the engine's own words for
           it. Host raises on this from inside assembling the world, which used
           to come back out through here as a crash rather than a report. *)
        Alcotest.check lines "named, so it can be looked for"
          [ {|the camera is in a room called "cellar"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                  P.camera ~room:"cellar" ~pos:(Vec.make 0. 0.) ~angle:0. ();
                ])));
    case "and one that names a room there is nothing to say about" (fun () ->
        Alcotest.check lines "silence" []
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                  P.camera ~room:"east" ~pos:(Vec.make 3. 0.) ~angle:0. ();
                ])));
    case "two cameras, and the one that is not being listened to" (fun () ->
        (* Host takes the last and says nothing about the rest, so the ones it
           dropped are exactly what a reader of the description cannot see. *)
        Alcotest.check lines "the earlier one, named"
          [ "this camera is overruled by a later one" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                  P.camera ~room:"west" ~pos:(Vec.make (-3.) 0.) ~angle:0. ();
                  P.camera ~room:"east" ~pos:(Vec.make 3. 0.) ~angle:0. ();
                ])));
    case "which is a warning, because the world still runs" (fun () ->
        Alcotest.check lines "one of the two is obeyed" [ "warning" ]
          (severities
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                  P.camera ~room:"west" ~pos:(Vec.make (-3.) 0.) ~angle:0. ();
                  P.camera ~room:"east" ~pos:(Vec.make 3. 0.) ~angle:0. ();
                ])));
    case "and an overruled one is not judged on the room it names" (fun () ->
        (* Host looks the last camera's room up and drops the rest without
           reading them, so this one's "cellar" is never looked for and nothing
           is broken by it. What there is to say is that the camera does
           nothing — not that the nothing it does is in the wrong place. *)
        let two_cameras =
          P.world ~atmosphere:Atmosphere.default
            ~spawn:("west", Vec.make (-3.) 0.)
            [
              P.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ];
              P.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ];
              P.link ("west", "east") ("east", "west");
              P.camera ~room:"cellar" ~pos:(Vec.make 0. 0.) ~angle:0. ();
              P.camera ~room:"east" ~pos:(Vec.make 3. 0.) ~angle:0. ();
            ]
        in
        Alcotest.check lines "the overruling, and only that"
          [ "this camera is overruled by a later one" ]
          (summaries two_cameras);
        Alcotest.check lines "and nothing here is broken" [ "warning" ]
          (severities two_cameras));
    case "which leaves the world buildable, and so still checked" (fun () ->
        (* The room check used to error here, and an error stops the tiers
           below it — so a dead camera naming a typo took the geometry checks
           down with it. This world's spawn is in a wall, and that is what a
           report of it has to be able to reach. *)
        Alcotest.check lines "the camera, and the thing behind it"
          [
            "the player starts inside a wall";
            "this camera is overruled by a later one";
          ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-6.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                  P.camera ~room:"cellar" ~pos:(Vec.make 0. 0.) ~angle:0. ();
                  P.camera ~room:"east" ~pos:(Vec.make 3. 0.) ~angle:0. ();
                ])));
    case "two children under one key are reported, not thrown" (fun () ->
        (* Reconciling refuses this outright, which is a crash where a check is
           supposed to be a report. *)
        Alcotest.check lines "named where the pair of them sit"
          [ {|two of these children are keyed "side"|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side ~key:"side" (); west_door ~key:"side" () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                ])));
    case "the player starts inside a wall" (fun () ->
        Alcotest.check lines "the first step would be refused"
          [ "the player starts inside a wall" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-6.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  P.link ("west", "east") ("east", "west");
                ])));
    case "a room nothing leads to" (fun () ->
        Alcotest.check lines "content nobody can reach"
          [ "no doorway leads to this room" ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  (* No doorways at all, so nothing is unlinked and nothing
                     reaches it either. *)
                  P.room ~name:"cellar" ~floor ~ceiling
                    [
                      P.boundary ~height ~material:stone
                        (P.corners
                           [
                             Vec.make 10. 0.;
                             Vec.make 14. 0.;
                             Vec.make 14. 4.;
                             Vec.make 10. 4.;
                           ]);
                    ];
                  P.link ("west", "east") ("east", "west");
                ])));
    case "a doorway with a gap beside it" (fun () ->
        (* The boundary stops a cell short of the opening, so the room shows its
           floor and sky to the horizon through the corner. The doorway is as
           wide as the wall it is cut into, which leaves no jamb to hide it. *)
        Alcotest.check lines "the corner meets nothing"
          [ {|the doorway "east" has a corner that meets no wall|} ]
          (summaries
             (P.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  P.room ~name:"west" ~floor ~ceiling
                    [
                      west_side ~corner:(Vec.make 0. (-3.)) ();
                      west_door ~width:8. ();
                    ];
                  P.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door ~width:8. () ];
                  P.link ("west", "east") ("east", "west");
                ])));
    case "and the reason it gives keeps the two words apart" (fun () ->
        (* The detail used to open "A doorway is an opening in a boundary",
           which is the one sentence in the engine that says so. A doorway is an
           opening {e and its jambs} — see {!Camlcast_core.Room} — and being
           made with its jambs is exactly why a doorway cannot be the thing this
           complaint is about. Only a bare threshold can, so the explanation now
           says which of the two makes one and which cannot. *)
        let gap =
          P.world ~atmosphere:Atmosphere.default
            ~spawn:("west", Vec.make (-3.) 0.)
            [
              P.room ~name:"west" ~floor ~ceiling
                [
                  west_side ~corner:(Vec.make 0. (-3.)) ();
                  west_door ~width:8. ();
                ];
              P.room ~name:"east" ~floor ~ceiling
                [ east_side; east_door ~width:8. () ];
              P.link ("west", "east") ("east", "west");
            ]
        in
        let said = details gap in
        Alcotest.(check bool)
          "it names the form that cannot leave one" true
          (said_anywhere said "P.doorway");
        Alcotest.(check bool)
          "and the form that can" true
          (said_anywhere said "P.threshold");
        Alcotest.(check bool)
          "and does not define a doorway as an opening" false
          (said_anywhere said "A doorway is an opening"));
    case "a step in the floor is a warning and not an error" (fun () ->
        let stepped =
          P.world ~atmosphere:Atmosphere.default
            ~spawn:("west", Vec.make (-3.) 0.)
            [
              P.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ];
              P.room ~name:"east" ~floor:(floor_at 0.5) ~ceiling
                [ east_side; east_door () ];
              P.link ("west", "east") ("east", "west");
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

(* {!Check.assembled} and {!Camlcast_core.World.check} are the same two words the
   other way round, and running one is not running the other. They overlap in
   nothing: World.check asserts what World.make guarantees, over a world grown
   by add_room and link instead of made in one go, and raises on the first
   break. Check.assembled reads a world that is already sound for the four ways it
   can still be wrong to play, and hands them back.

   Both directions below, because a reader who knows only one of them is a
   reader who thinks their world is checked. *)
let the_other_check =
  let world_of description = (Mount.build description).Scene.world in
  [
    case "a structural break is World.check's alone" (fun () ->
        (* A second doorway cut into the first room and left unlinked. The room
           is still reached through the doorway that is linked, its corners
           still meet walls, the floors still agree and the spawn is still
           clear — so all four of Check.assembled's questions answer well. *)
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
        Alcotest.check_raises "and World.check will not have it"
          (Invalid_argument
             "World.check: nothing links threshold first.unfinished") (fun () ->
            World.check grown));
    case "and a step in the floor is Check.assembled's alone" (fun () ->
        let stepped =
          world_of
            (P.world ~atmosphere:Atmosphere.default
               ~spawn:("west", Vec.make (-3.) 0.)
               [
                 P.room ~name:"west" ~floor ~ceiling
                   [ west_side (); west_door () ];
                 P.room ~name:"east" ~floor:(floor_at 0.5) ~ceiling
                   [ east_side; east_door () ];
                 P.link ("west", "east") ("east", "west");
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
        (* And World.check is satisfied, which is the half that matters: every
           invariant it knows about holds in a world nobody can walk through
           without the camera jolting. *)
        World.check stepped);
  ]

(* A diagnostic names the component that wrote the offending part, which is the
   whole difference from the message the engine raised before. *)
let where_it_says =
  let gallery =
    Camlcast_loom.Element.declare ~name:"gallery" @@ fun () ->
    P.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ]
  in
  let annexe =
    Camlcast_loom.Element.declare ~name:"annexe" @@ fun () ->
    P.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ]
  in
  [
    case "the component, not the room" (fun () ->
        let report =
          Check.report
            (P.world ~atmosphere:Atmosphere.default
               ~spawn:("west", Vec.make (-6.) 0.)
               [
                 gallery (); annexe (); P.link ("west", "east") ("east", "west");
               ])
        in
        Alcotest.check lines "named by where it was written" [ "gallery" ]
          (List.map (fun (d : Check.t) -> d.Check.where) report));
    case "the component that placed the camera" (fun () ->
        (* A spawn is an argument of the world and belongs to no component, so
           it can only say "(root)". A camera is written somewhere. *)
        let eye =
          Camlcast_loom.Element.declare ~name:"eye" @@ fun () ->
          P.camera ~room:"cellar" ~pos:(Vec.make 0. 0.) ~angle:0. ()
        in
        let report =
          Check.report
            (P.world ~atmosphere:Atmosphere.default
               ~spawn:("west", Vec.make (-3.) 0.)
               [
                 gallery ();
                 annexe ();
                 P.link ("west", "east") ("east", "west");
                 eye ();
               ])
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
      ("links", links);
      ("agrees with the engine", agrees_with_the_engine);
      ("the world it makes", the_world_it_makes);
      ("the other check", the_other_check);
      ("where it says", where_it_says);
    ]
