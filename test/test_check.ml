(* Reading a description for what a compiler cannot see.

   Every case here is a world that builds, or nearly does, and is wrong. What is
   asserted is the summary line each one produces, because that line is the
   whole product: a diagnostic nobody can act on is worth no more than the bare
   Invalid_argument it replaced. *)

open Camlcast
open Camlcast_stage
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
let west_side ?(corner = Vec.make 0. (-4.)) () =
  Parts.path ~height ~material:stone
    [ Vec.make 0. 4.; Vec.make (-6.) 4.; Vec.make (-6.) (-4.); corner ]

let east_side =
  Parts.path ~height ~material:stone
    [ Vec.make 0. (-4.); Vec.make 6. (-4.); Vec.make 6. 4.; Vec.make 0. 4. ]

let west_door ?door ?(width = 2.) ?(name = "east") () =
  Parts.doorway ?door ~name ~width ~opening:2.5 ~height ~material:stone
    (Vec.make 0. (-4.)) (Vec.make 0. 4.)

let east_door ?door ?(width = 2.) ?(name = "west") () =
  Parts.doorway ?door ~name ~width ~opening:2.5 ~height ~material:stone
    (Vec.make 0. 4.) (Vec.make 0. (-4.))

let good =
  Parts.world ~atmosphere:Atmosphere.default
    ~spawn:("west", Vec.make (-3.) 0.)
    [
      Parts.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ];
      Parts.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ];
      Parts.link ("west", "east") ("east", "west");
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
             (Parts.wall ~height ~material:stone (Vec.make 0. 0.)
                (Vec.make 1. 0.))));
    case "a room inside a room" (fun () ->
        Alcotest.check lines "the inner one is the complaint"
          [ "a room inner cannot go in a room" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("outer", Vec.make 0. 0.)
                [
                  Parts.room ~name:"outer" ~floor ~ceiling
                    [ Parts.room ~name:"inner" ~floor ~ceiling [] ];
                ])));
    case "a sprite where a room should be" (fun () ->
        Alcotest.check lines "not in a world"
          [ "a sprite (0,0) cannot go in a world" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("nowhere", Vec.make 0. 0.)
                [ Parts.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ])));
    case "a sprite hung on a wall" (fun () ->
        Alcotest.check lines "only decals go there"
          [ "a sprite (0,0) cannot hang on a wall" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("room", Vec.make 0. 0.)
                [
                  Parts.room ~name:"room" ~floor ~ceiling
                    [
                      Parts.wall ~height ~material:stone
                        ~decals:
                          [
                            Parts.sprite ~size:1. ~image:poster (Vec.make 0. 0.);
                          ]
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
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling [ west_side () ];
                  Parts.room ~name:"west" ~floor ~ceiling [ east_side ];
                ])));
    case "two doorways of the same name in one room" (fun () ->
        Alcotest.check lines "a link could not tell them apart"
          [ {|there is already a doorway called "east"|} ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [
                      west_side ();
                      west_door ();
                      Parts.doorway ~name:"east" ~width:2. ~opening:2.5 ~height
                        ~material:stone (Vec.make (-6.) (-4.))
                        (Vec.make (-6.) 4.);
                    ];
                ])));
  ]

let links =
  let two_rooms ~link_to =
    Parts.world ~atmosphere:Atmosphere.default
      ~spawn:("west", Vec.make (-3.) 0.)
      [
        Parts.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ];
        Parts.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ];
        Parts.link ("west", "east") link_to;
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
                (Parts.world ~atmosphere:Atmosphere.default
                   ~spawn:("west", Vec.make (-3.) 0.)
                   [
                     Parts.room ~name:"west" ~floor ~ceiling
                       [ west_side (); west_door () ];
                     Parts.room ~name:"east" ~floor ~ceiling
                       [ east_side; east_door () ];
                   ]))));
    case "a doorway two links claim" (fun () ->
        Alcotest.check lines "a place that cannot exist"
          [ {|the doorway "west" is linked 2 times|} ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door (); west_door ~name:"south" () ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  Parts.link ("west", "east") ("east", "west");
                  Parts.link ("west", "south") ("east", "west");
                ])));
    case "two sides of one doorway that are different widths" (fun () ->
        Alcotest.check lines "they are the same opening"
          [ "the two sides of this link are different widths" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door ~width:3. () ];
                  Parts.link ("west", "east") ("east", "west");
                ])));
    case "a door on one side and none on the other" (fun () ->
        Alcotest.check lines "one leaf hangs in one opening"
          [ "one side of this link has a door and the other does not" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door ~door:(Door.make stone) () ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  Parts.link ("west", "east") ("east", "west");
                ])));
  ]

(* What only an assembled world can answer. Each of these builds cleanly. *)
let the_world_it_makes =
  [
    case "the player starts in a room that is not there" (fun () ->
        Alcotest.check lines "named, so it can be looked for"
          [ {|the player starts in a room called "cellar"|} ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("cellar", Vec.make 0. 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  Parts.link ("west", "east") ("east", "west");
                ])));
    case "the player starts inside a wall" (fun () ->
        Alcotest.check lines "the first step would be refused"
          [ "the player starts inside a wall" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-6.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  Parts.link ("west", "east") ("east", "west");
                ])));
    case "a room nothing leads to" (fun () ->
        Alcotest.check lines "content nobody can reach"
          [ "no doorway leads to this room" ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [ west_side (); west_door () ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door () ];
                  (* No doorways at all, so nothing is unlinked and nothing
                     reaches it either. *)
                  Parts.room ~name:"cellar" ~floor ~ceiling
                    [
                      Parts.outline ~height ~material:stone
                        [
                          Vec.make 10. 0.;
                          Vec.make 14. 0.;
                          Vec.make 14. 4.;
                          Vec.make 10. 4.;
                        ];
                    ];
                  Parts.link ("west", "east") ("east", "west");
                ])));
    case "a doorway with a gap beside it" (fun () ->
        (* The boundary stops a cell short of the opening, so the room shows its
           floor and sky to the horizon through the corner. The doorway is as
           wide as the wall it is cut into, which leaves no jamb to hide it. *)
        Alcotest.check lines "the corner meets nothing"
          [ {|the doorway "east" has a corner that meets no wall|} ]
          (summaries
             (Parts.world ~atmosphere:Atmosphere.default
                ~spawn:("west", Vec.make (-3.) 0.)
                [
                  Parts.room ~name:"west" ~floor ~ceiling
                    [
                      west_side ~corner:(Vec.make 0. (-3.)) ();
                      west_door ~width:8. ();
                    ];
                  Parts.room ~name:"east" ~floor ~ceiling
                    [ east_side; east_door ~width:8. () ];
                  Parts.link ("west", "east") ("east", "west");
                ])));
    case "a step in the floor is a warning and not an error" (fun () ->
        let stepped =
          Parts.world ~atmosphere:Atmosphere.default
            ~spawn:("west", Vec.make (-3.) 0.)
            [
              Parts.room ~name:"west" ~floor ~ceiling
                [ west_side (); west_door () ];
              Parts.room ~name:"east" ~floor:(floor_at 0.5) ~ceiling
                [ east_side; east_door () ];
              Parts.link ("west", "east") ("east", "west");
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

(* A diagnostic names the component that wrote the offending part, which is the
   whole difference from the message the engine raised before. *)
let where_it_says =
  let gallery =
    Camlcast_loom.Element.declare ~name:"gallery" @@ fun () ->
    Parts.room ~name:"west" ~floor ~ceiling [ west_side (); west_door () ]
  in
  let annexe =
    Camlcast_loom.Element.declare ~name:"annexe" @@ fun () ->
    Parts.room ~name:"east" ~floor ~ceiling [ east_side; east_door () ]
  in
  [
    case "the component, not the room" (fun () ->
        let report =
          Check.report
            (Parts.world ~atmosphere:Atmosphere.default
               ~spawn:("west", Vec.make (-6.) 0.)
               [
                 gallery ();
                 annexe ();
                 Parts.link ("west", "east") ("east", "west");
               ])
        in
        Alcotest.check lines "named by where it was written" [ "gallery" ]
          (List.map (fun (d : Check.t) -> d.Check.where) report));
  ]

let () =
  Alcotest.run "Check"
    [
      ("nothing to report", nothing_to_report);
      ("structure", structure);
      ("naming", naming);
      ("links", links);
      ("the world it makes", the_world_it_makes);
      ("where it says", where_it_says);
    ]
