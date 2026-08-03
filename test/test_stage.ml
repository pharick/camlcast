(* Descriptions, and the worlds they assemble into.

   Nothing here opens a window either. A description becomes a World and a World
   is a value, so the whole of this step can be checked by comparing one against
   the world the old API builds by hand — which is the strongest thing available
   until there are pixels to compare, and stronger than reading the code twice.

   The reference is the one-room world the core API builds by hand, restated inline
   below and built with the tree. If the declarative version stops agreeing with it,
   one of the two has moved. *)

(* {1 Comparing two worlds}

   World.t is abstract, so this walks it through the accessors a renderer uses.
   That is the right level: two worlds that answer every one of these the same
   way draw the same picture, whatever they are made of underneath. *)

open Camlcast_core
open Camlcast
open Support

let same_wall where (expected : Room.wall) (actual : Room.wall) =
  Alcotest.check vec (where ^ ": from") expected.a actual.a;
  Alcotest.check vec (where ^ ": to") expected.b actual.b;
  Alcotest.check close (where ^ ": height") expected.height actual.height;
  Alcotest.(check int)
    (where ^ ": decals")
    (List.length expected.decals)
    (List.length actual.decals)

let same_room where expected actual =
  Alcotest.(check int)
    (where ^ ": walls") (Room.wall_count expected) (Room.wall_count actual);
  for index = 0 to Room.wall_count expected - 1 do
    same_wall
      (Printf.sprintf "%s: wall %d" where index)
      (Room.wall_at expected index)
      (Room.wall_at actual index)
  done;
  Alcotest.(check int)
    (where ^ ": thresholds")
    (Room.threshold_count expected)
    (Room.threshold_count actual);
  for index = 0 to Room.threshold_count expected - 1 do
    let one = Room.threshold_at expected index
    and other = Room.threshold_at actual index in
    Alcotest.(check string)
      (Printf.sprintf "%s: threshold %d name" where index)
      one.Room.name other.Room.name;
    Alcotest.check vec
      (Printf.sprintf "%s: threshold %d from" where index)
      one.Room.a other.Room.a;
    Alcotest.check vec
      (Printf.sprintf "%s: threshold %d to" where index)
      one.Room.b other.Room.b
  done;
  Alcotest.(check int)
    (where ^ ": sprites")
    (Room.sprite_count expected)
    (Room.sprite_count actual);
  for index = 0 to Room.sprite_count expected - 1 do
    Alcotest.check vec
      (Printf.sprintf "%s: sprite %d" where index)
      (Room.sprite_at expected index).Room.pos
      (Room.sprite_at actual index).Room.pos
  done;
  let plane (surface : Room.surface) = surface.plane in
  let one = plane (Room.floor_surface expected)
  and other = plane (Room.floor_surface actual) in
  Alcotest.check close (where ^ ": floor a") one.Plane.a other.Plane.a;
  Alcotest.check close (where ^ ": floor b") one.Plane.b other.Plane.b;
  Alcotest.check close (where ^ ": floor c") one.Plane.c other.Plane.c

let same_world expected actual =
  Alcotest.(check int)
    "rooms"
    (World.room_count expected)
    (World.room_count actual);
  for index = 0 to World.room_count expected - 1 do
    Alcotest.(check string)
      (Printf.sprintf "room %d name" index)
      (World.name expected index)
      (World.name actual index);
    same_room
      (Printf.sprintf "room %s" (World.name expected index))
      (World.room expected index)
      (World.room actual index)
  done;
  let one = World.spawn expected and other = World.spawn actual in
  Alcotest.(check int) "spawn room" one.World.room other.World.room;
  Alcotest.check vec "spawn spot" one.World.pos other.World.pos;
  (* Portals are what a link comes to, and the only thing that says two rooms
     were joined at all. *)
  for room = 0 to World.room_count expected - 1 do
    for threshold = 0 to World.doorway_count expected ~room - 1 do
      match
        ( World.portal expected ~room ~threshold,
          World.portal actual ~room ~threshold )
      with
      | None, None -> ()
      | Some one, Some other ->
          Alcotest.(check int)
            (Printf.sprintf "portal %d.%d to" room threshold)
            one.World.to_room other.World.to_room;
          Alcotest.(check int)
            (Printf.sprintf "portal %d.%d twin" room threshold)
            one.World.twin other.World.twin
      | _ ->
          Alcotest.failf "portal %d.%d: one is linked and the other is not" room
            threshold
    done
  done

(* {1 The reference: the guide's one room, said twice} *)

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let stone =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 150 150 160)))

let ground =
  Material.make
    ~pattern:(Texture.generate (checker ~color:(Color.rgb 116 110 98)))

let height = 4.
let ground_plane = Plane.horizontal 0.
let floor = Room.floor ~plane:ground_plane ~material:ground
let ceiling = Room.roof ~plane:(Plane.above ground_plane height) ~material:stone
let spawn = ("room", Vec.make (-4.5) 0.)

(* By hand, exactly as the guide and the README have it. *)
let built =
  World.make
    ~rooms:
      [
        ( "room",
          Room.make ~floor ~ceiling
            (Room.rectangle ~height ~material:stone (Vec.make (-6.) (-6.))
               (Vec.make 6. 6.)) );
      ]
    ~links:[] ~atmosphere:Atmosphere.default ~spawn

(* And described. *)
let corners =
  [ Vec.make (-6.) (-6.); Vec.make 6. (-6.); Vec.make 6. 6.; Vec.make (-6.) 6. ]

let described =
  P.world ~atmosphere:Atmosphere.default ~spawn
    [
      P.room ~name:"room" ~floor ~ceiling
        [ P.boundary ~height ~material:stone (P.corners corners) ];
    ]

let the_smallest_game =
  [
    case "a described room is the room the platform builds" (fun () ->
        same_world built (Mount.build described).Scene.world);
    case "describing it twice into one mount changes nothing" (fun () ->
        let mount = Mount.create () in
        ignore (Mount.render mount described);
        same_world built (Mount.render mount described).Scene.world);
  ]

(* {1 Winding}

   The trap this step exists to close. *)

(* {!P.boundary} with nothing said at any leg has to be the two helpers it
   generalizes, or it is a third way of building a boundary rather than one way
   with more available. *)
(* {!P.boundary} is now the only way to lay a run of wall in a description, so
   what has to be pinned is that its two axes still mean what the two functions
   it replaced meant: [closed] shuts the loop, and {!P.polygon} hands it the
   corners {!Room.regular_polygon} would have used. *)
let runs =
  let described children =
    (Mount.build
       (P.world ~atmosphere:Atmosphere.default ~spawn
          [ P.room ~name:"room" ~floor ~ceiling children ]))
      .Scene.world
  in
  let walls world =
    let r = World.room world 0 in
    List.init (Room.wall_count r) (Room.wall_at r)
  in
  [
    case "closed shuts the loop and open leaves it" (fun () ->
        let shut =
          described [ P.boundary ~height ~material:stone (P.corners corners) ]
        and ajar =
          described
            [
              P.boundary ~closed:false ~height ~material:stone
                (P.corners corners);
            ]
        in
        Alcotest.(check int)
          "four corners, four walls shut" 4
          (List.length (walls shut));
        Alcotest.(check int) "and three open" 3 (List.length (walls ajar));
        (* The one the open run leaves out is the one back to the first
           corner. *)
        let last = List.nth (walls shut) 3 in
        Alcotest.check vec "it is the closing wall that is missing"
          (List.nth corners 3) last.Room.a;
        Alcotest.check vec "and it comes back to the start" (List.nth corners 0)
          last.Room.b);
    case "a polygon boundary is Room.regular_polygon" (fun () ->
        (* The corners were split out of that function so a pillar could carry
           per-leg handlers; splitting them must not have moved one. *)
        let center = Vec.make 1.5 (-2.5) in
        let built =
          Room.regular_polygon ~center ~radius:0.8 ~sides:5 ~rotation:0.3
            ~height ~material:stone
        in
        let described_walls =
          walls
            (described
               [
                 P.boundary ~height ~material:stone
                   (P.polygon ~center ~radius:0.8 ~sides:5 ~rotation:0.3);
               ])
        in
        Alcotest.(check int)
          "same count" (List.length built)
          (List.length described_walls);
        List.iteri
          (fun i ((one : Room.wall), (other : Room.wall)) ->
            Alcotest.check vec
              (Printf.sprintf "wall %d from" i)
              one.Room.a other.Room.a;
            Alcotest.check vec
              (Printf.sprintf "wall %d to" i)
              one.Room.b other.Room.b)
          (List.combine built described_walls));
    case "the last corner of an open run cannot carry anything" (fun () ->
        (* It leaves no wall, so a handler there would never fire — the one
           mistake the "describes the wall leaving it" shape invites. *)
        Alcotest.check_raises "said plainly"
          (Invalid_argument
             "P.boundary: the last corner of an open run leaves no wall, so it \
              can carry nothing") (fun () ->
            ignore
              (P.boundary ~closed:false ~height ~material:stone
                 (match corners with
                 | p :: q :: r :: _ ->
                     [ P.corner p; P.corner q; P.corner r ~key:"nowhere" ]
                 | _ -> assert false)));
        (* A closed run has no such corner, so the same list is fine shut. *)
        ignore
          (described
             [
               P.boundary ~height ~material:stone
                 (match corners with
                 | p :: q :: r :: _ ->
                     [ P.corner p; P.corner q; P.corner r ~key:"back" ]
                 | _ -> assert false);
             ]));
  ]

let winding =
  [
    case "corners in either order build the same room" (fun () ->
        let forwards =
          P.world ~atmosphere:Atmosphere.default ~spawn
            [
              P.room ~name:"room" ~floor ~ceiling
                [ P.boundary ~height ~material:stone (P.corners corners) ];
            ]
        and backwards =
          P.world ~atmosphere:Atmosphere.default ~spawn
            [
              P.room ~name:"room" ~floor ~ceiling
                [
                  P.boundary ~height ~material:stone
                    (P.corners (List.rev corners));
                ];
            ]
        in
        same_world (Mount.build forwards).Scene.world
          (Mount.build backwards).Scene.world);
    case "and it is the winding Room.rectangle uses" (fun () ->
        (* The engine documents rectangle as impossible to wind wrong, so it is
           the definition rather than a rule restated alongside it. *)
        same_world built
          (Mount.build
             (P.world ~atmosphere:Atmosphere.default ~spawn
                [
                  P.room ~name:"room" ~floor ~ceiling
                    [
                      P.boundary ~height ~material:stone
                        (P.corners (List.rev corners));
                    ];
                ]))
            .Scene.world);
    (* {!P.boundary} is the winding above with the whole of {!P.wall} at every leg,
       so the question it has to answer is whether a leg stays on the wall its
       corner named when the run comes out wound the other way. Written in both
       orders, every wall must end up with the same material on it.

       This is the case the implementation warns about: a leg describes the wall
       it {e leaves}, so reversing the corners and letting each leg travel with
       its own corner puts every leg one wall out. That version passes "the same
       walls are there" and fails this. *)
    case "a leg stays on its own wall through the winding" (fun () ->
        let brick =
          Material.make
            ~pattern:(Texture.generate (checker ~color:(Color.rgb 190 90 70)))
        in
        (* One corner of the four is bricked, so the answer is asymmetric: a leg
           shifted by one, or the whole run reflected, lands the brick somewhere
           else and the walls below stop matching pairwise. *)
        let legs cs =
          match cs with
          | [ p; q; r; s ] ->
              [ P.corner p; P.corner q ~material:brick; P.corner r; P.corner s ]
          | _ -> assert false
        in
        let describe cs =
          (Mount.build
             (P.world ~atmosphere:Atmosphere.default ~spawn
                [
                  P.room ~name:"room" ~floor ~ceiling
                    [
                      P.boundary ~closed:true ~height ~material:stone (legs cs);
                    ];
                ]))
            .Scene.world
        in
        let forwards = describe corners
        and backwards = describe (List.rev corners) in
        same_world forwards backwards;
        (* And it is the brick that moved with its wall, not merely four walls
           in the same places: find it by its endpoints in each. *)
        let bricked world =
          let room = World.room world 0 in
          List.find_map
            (fun i ->
              let w = Room.wall_at room i in
              if w.Room.material == brick then Some (w.Room.a, w.Room.b)
              else None)
            (List.init (Room.wall_count room) Fun.id)
        in
        let a, b = Option.get (bricked forwards) in
        let c, d = Option.get (bricked backwards) in
        Alcotest.check vec "the brick wall starts in the same place" a c;
        Alcotest.check vec "and ends in the same place" b d);
    case "every normal faces into the room" (fun () ->
        (* The symptom of a reversed boundary is a room black from inside, and
           this is that stated as arithmetic: from the middle of the room, every
           wall's normal points back towards you. *)
        let room =
          World.room
            (Mount.build
               (P.world ~atmosphere:Atmosphere.default ~spawn
                  [
                    P.room ~name:"room" ~floor ~ceiling
                      [
                        P.boundary ~height ~material:stone
                          (P.corners (List.rev corners));
                      ];
                  ]))
              .Scene.world
            0
        in
        let centre = Vec.make 0. 0. in
        for index = 0 to Room.wall_count room - 1 do
          let wall = Room.wall_at room index in
          let towards_centre = Vec.sub centre wall.Room.a in
          Alcotest.(check bool)
            (Printf.sprintf "wall %d faces inward" index)
            true
            (Vec.dot wall.Room.normal towards_centre > 0.)
        done);
  ]

(* {1 Doorways and links} *)

(* Three sides run as a path and the fourth cut as a doorway, which together
   close the boundary. An outline of all four corners *and* a doorway along one
   of them would be a solid wall standing behind an opening — six walls where
   five were meant, and a doorway you cannot walk through. *)
let two_room_world ~door =
  P.world ~atmosphere:Atmosphere.default
    ~spawn:("west", Vec.make (-3.) 0.)
    [
      P.room ~name:"west" ~floor ~ceiling
        [
          P.boundary ~closed:false ~height ~material:stone
            (P.corners
               [
                 Vec.make 0. 4.;
                 Vec.make (-6.) 4.;
                 Vec.make (-6.) (-4.);
                 Vec.make 0. (-4.);
               ]);
          P.doorway ?door ~name:"east" ~width:2. ~opening:2.5 ~height
            ~material:stone (Vec.make 0. (-4.)) (Vec.make 0. 4.);
        ];
      P.room ~name:"east" ~floor ~ceiling
        [
          P.boundary ~closed:false ~height ~material:stone
            (P.corners
               [
                 Vec.make 0. (-4.);
                 Vec.make 6. (-4.);
                 Vec.make 6. 4.;
                 Vec.make 0. 4.;
               ]);
          P.doorway ?door ~name:"west" ~width:2. ~opening:2.5 ~height
            ~material:stone (Vec.make 0. 4.) (Vec.make 0. (-4.));
        ];
      P.link ("west", "east") ("east", "west");
    ]

(* {!Support.vec} allows 1e-9, which is the right slack for anything that has
   been through a [cos] or a division and is eight orders too much for the case
   below: the two forms of the cut agreed to 6.21e-17 the whole time they
   disagreed. Nothing here goes through anything inexact — the claim is that two
   expressions produce the same floats — so the comparison is [=], and the
   printer shows enough digits for a failure to be legible. *)
let exactly =
  Alcotest.testable
    (fun ppf (v : Vec.t) -> Format.fprintf ppf "(%.17g, %.17g)" v.Vec.x v.Vec.y)
    (fun (a : Vec.t) (b : Vec.t) -> a.Vec.x = b.Vec.x && a.Vec.y = b.Vec.y)

let doorways =
  (* P.opening does the same arithmetic P.doorway does, so it has to refuse what
     P.doorway refuses. Unrefused, each of these is a pair of nans that comes
     back much later as a transform that will not invert, a long way from the
     two points that were wrong. *)
  let refuses name ~width ~refused a b =
    case name (fun () ->
        Alcotest.check_raises "in the words of the function that was called"
          (Invalid_argument refused) (fun () -> ignore (P.opening ~width a b)))
  in
  [
    (* And refusing the same things is the cheap half of "the same arithmetic".
       The expensive half is landing in the same place, which nothing asked
       about — so P.opening restated the formula, restated the superseded form
       of it, and went on refusing everything it was supposed to.

       Bit-for-bit and not [close], because approximately-equal is exactly what
       was true while it was wrong: the two agreed to 6.21e-17, which is a
       cancellation away from an invisible wall a player walks into. Read off
       the threshold P.doorway actually built rather than recomputed here, so
       the fixture cannot drift into agreeing with the wrong one. *)
    case "P.opening lands where P.doorway cuts" (fun () ->
        let stone =
          Material.make
            ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 150))
        in
        let ends ~width a b =
          let _, t =
            Room.doorway ~name:"d" ~width ~opening:2. ~height:3. ~material:stone
              a b
          in
          (t.Room.a, t.Room.b)
        in
        let same ~width a b =
          let ra, rb = ends ~width a b and pa, pb = P.opening ~width a b in
          Alcotest.check exactly
            (Printf.sprintf "near end at width %g" width)
            ra pa;
          Alcotest.check exactly
            (Printf.sprintf "far end at width %g" width)
            rb pb
        in
        (* Axis-aligned, where the coordinates cancel and both forms agreed all
           along; then oblique, where they did not. *)
        same ~width:2. (Vec.make 0. 0.) (Vec.make 4. 0.);
        same ~width:4. (Vec.make 0. 0.) (Vec.make 4. 0.);
        let a = Vec.make 0.1 0.2 and b = Vec.make 0.7 1.3 in
        let span = Vec.length (Vec.sub b a) in
        same ~width:(span /. 3.) a b;
        same ~width:(span /. 2.) a b;
        (* The case that bit: a whole side that is one opening. Both ends have
           to come back as the very floats they went in as, or the jamb that is
           supposed to vanish is a wall instead. *)
        same ~width:span a b;
        let pa, pb = P.opening ~width:span a b in
        Alcotest.check exactly "and the near end is the corner itself" a pa;
        Alcotest.check exactly "as is the far one" b pb);
    case "a doorway cuts jambs and an opening out of one wall" (fun () ->
        let world = (Mount.build (two_room_world ~door:None)).Scene.world in
        let west = World.room world 0 in
        (* Three sides run as a path, plus the two jambs the doorway left either
           side of its opening. *)
        Alcotest.(check int) "walls" 5 (Room.wall_count west);
        Alcotest.(check int) "thresholds" 1 (Room.threshold_count west));
    case "a link makes two doorways into one" (fun () ->
        let world = (Mount.build (two_room_world ~door:None)).Scene.world in
        match World.portal world ~room:0 ~threshold:0 with
        | None -> Alcotest.fail "the west door leads nowhere"
        | Some portal ->
            Alcotest.(check int) "into the east room" 1 portal.World.to_room;
            Alcotest.(check int) "and back through its own" 0 portal.World.twin);
    case "the world it builds is one the engine agrees with" (fun () ->
        (* World.check is what the engine asserts about a world it did not build
           itself, so passing it is the engine's own opinion of the result. *)
        let world = (Mount.build (two_room_world ~door:None)).Scene.world in
        World.check world);
    case "a door is carried through to the threshold" (fun () ->
        let leaf = Door.make stone in
        let world =
          (Mount.build (two_room_world ~door:(Some leaf))).Scene.world
        in
        let threshold = Room.threshold_at (World.room world 0) 0 in
        Alcotest.(check bool)
          "the leaf is hung" true
          (Option.is_some threshold.Room.door));
    refuses "an opening in no wall at all" ~width:2.
      ~refused:"P.opening: no wall to cut an opening into" (Vec.make 1. 1.)
      (Vec.make 1. 1.);
    refuses "an opening of no width" ~width:0.
      ~refused:"P.opening: an opening has to have a width" (Vec.make 0. 0.)
      (Vec.make 4. 0.);
    refuses "an opening wider than its wall" ~width:5.
      ~refused:"P.opening: wider than the wall it is cut into" (Vec.make 0. 0.)
      (Vec.make 4. 0.);
  ]

(* {1 What is not a world} *)

let malformed =
  let fails what description =
    case what (fun () ->
        match Mount.build description with
        | _ -> Alcotest.failf "%s was accepted" what
        | exception Host.Malformed _ -> ())
  in
  [
    fails "a description with no world in it"
      (P.boundary ~height ~material:stone (P.corners corners));
    fails "a wall loose at the top level"
      (P.wall ~height ~material:stone (Vec.make 0. 0.) (Vec.make 1. 0.));
    fails "a room inside a room"
      (P.world ~atmosphere:Atmosphere.default ~spawn
         [
           P.room ~name:"outer" ~floor ~ceiling
             [ P.room ~name:"inner" ~floor ~ceiling [] ];
         ]);
    fails "a sprite where a room should be"
      (P.world ~atmosphere:Atmosphere.default ~spawn
         [ P.sprite ~size:1. ~image:poster (Vec.make 0. 0.) ]);
  ]

(* {1 One nesting rule, read the same way twice}

   Prim.may_contain is the rule and both readers have always shared it. What
   they did not share was the walk, and that is where they came apart: Host
   looked only under the four primitives that hold anything, so a child hung on
   a camera or a doorway went unlooked-at and ran; and its hud walk asked about
   every descendant as though the hud were its parent, so a bar inside a
   rectangle drew at runtime and failed in Check. Neither is reachable through P
   — its hud pieces take no children and only P.hud nests — but Element and Prim
   are public, so neither was unreachable either.

   What is asserted is agreement rather than either answer on its own. A checker
   that says a description is wrong is only worth reading if the thing it models
   refuses the same description. *)
let both_readers =
  let module E = Camlcast_loom.Element in
  let only_room =
    P.room ~name:"room" ~floor ~ceiling
      [ P.boundary ~height ~material:stone (P.corners corners) ]
  in
  let world_with extra =
    E.prim
      (Prim.World { atmosphere = Atmosphere.default; spawn })
      ~children:(only_room :: extra)
  in
  let bar =
    Prim.Bar
      {
        x = 4;
        y = 4;
        w = 4;
        h = 2;
        fraction = 0.5;
        color = Color.rgb 255 255 255;
      }
  in
  let rect =
    Prim.Rect
      { x = 0; y = 0; w = 8; h = 8; color = Color.rgb 0 0 0; alpha = 200 }
  in
  let agree what ~misplaced description =
    case what (fun () ->
        let reported =
          List.filter
            (fun (d : Check.t) ->
              String.length d.Check.summary > 1
              && String.ends_with ~suffix:"cannot go there" d.Check.summary)
            (Check.report description)
        in
        Alcotest.(check int)
          "Check says so"
          (if misplaced then 1 else 0)
          (List.length reported);
        match Mount.build description with
        | _ when misplaced ->
            Alcotest.failf "%s: Check refused it and Host did not" what
        | _ -> ()
        | exception Host.Malformed _ when misplaced -> ()
        | exception Host.Malformed m ->
            Alcotest.failf "%s: Host refused it and Check did not: %s" what m)
  in
  [
    agree "a bar inside a rectangle on the hud" ~misplaced:true
      (world_with
         [ E.prim Prim.Hud ~children:[ E.prim rect ~children:[ E.prim bar ] ] ]);
    agree "a room hung under a camera" ~misplaced:true
      (world_with
         [
           E.prim
             (Prim.Camera
                { room = "room"; pos = Vec.make 0. 0.; angle = 0.; pitch = 0. })
             ~children:[ P.room ~name:"stowaway" ~floor ~ceiling [] ];
         ]);
    agree "a wall hung under a cursor" ~misplaced:true
      (world_with
         [
           E.prim Prim.Cursor
             ~children:
               [
                 P.wall ~height ~material:stone (Vec.make 0. 0.)
                   (Vec.make 1. 0.);
               ];
         ]);
    agree "a hud grouped inside a hud, which is the way to group one"
      ~misplaced:false
      (world_with
         [
           E.prim Prim.Hud
             ~children:[ E.prim Prim.Hud ~children:[ E.prim bar ] ];
         ]);
    agree "a bar directly on the hud" ~misplaced:false
      (world_with [ E.prim Prim.Hud ~children:[ E.prim bar ] ]);
  ]

(* {1 What goes in a room} *)

let furnishing =
  let dressed =
    P.world ~atmosphere:Atmosphere.default ~spawn
      [
        P.room ~name:"room" ~floor ~ceiling
          [
            P.boundary ~height ~material:stone (P.corners corners);
            P.wall ~height:2. ~material:stone
              ~decals:
                [
                  P.decal ~along:1. ~z:1.5 ~half_width:0.5 ~half_height:0.5
                    poster;
                ]
              (Vec.make (-2.) 2.) (Vec.make 2. 2.);
            P.sprite ~key:"barrel" ~size:0.9 ~image:poster (Vec.make 1. (-1.));
            P.sprite ~key:"lamp" ~base:1.2 ~size:0.5 ~image:poster
              (Vec.make (-1.) (-1.));
          ];
      ]
  in
  [
    case "walls, sprites and decals each land where they belong" (fun () ->
        let room = World.room (Mount.build dressed).Scene.world 0 in
        Alcotest.(check int)
          "four sides and a partition" 5 (Room.wall_count room);
        Alcotest.(check int) "two sprites" 2 (Room.sprite_count room);
        let partition = Room.wall_at room 4 in
        Alcotest.(check int)
          "the decal is on the partition" 1
          (List.length partition.Room.decals);
        Alcotest.(check int)
          "and on nothing else" 0
          (List.length (Room.wall_at room 0).Room.decals));
    case "a sprite keeps what it was given" (fun () ->
        let room = World.room (Mount.build dressed).Scene.world 0 in
        let lamp = Room.sprite_at room 1 in
        Alcotest.check close "floated off the floor" 1.2 lamp.Room.base;
        Alcotest.check vec "where it was put" (Vec.make (-1.) (-1.))
          lamp.Room.pos);
  ]

(* {1 The pixels}

   Two worlds that answer every accessor alike ought to draw alike, and up to
   here that has been an argument rather than a measurement. This measures it.

   Framebuffer.offscreen has no streaming texture behind it and
   Renderer.draw_frame makes no SDL call, so a whole frame can be drawn and read
   back with no window open. That is the engine's own testing trick, and it is
   what makes the strongest available gate for this rewrite cost nothing: draw
   the described world and the hand-built one from the same eye, and compare
   every pixel. *)

let render_from world player ~width ~height =
  let buffer = Framebuffer.offscreen ~width ~height in
  Renderer.draw_frame buffer world player;
  buffer

let differing_pixel one other ~width ~height =
  let rec scan x y =
    if y >= height then None
    else if x >= width then scan 0 (y + 1)
    else if Framebuffer.pixel one ~x ~y = Framebuffer.pixel other ~x ~y then
      scan (x + 1) y
    else Some (x, y)
  in
  scan 0 0

(* Several angles, because one eye can miss a difference by facing away from
   it: straight ahead, two turns into the corners, and most of the way round. *)
let the_same_picture =
  let width = 320 and height = 240 in
  List.map
    (fun angle ->
      case
        (Printf.sprintf "the described world draws the built one, facing %g"
           angle) (fun () ->
          let player = Player.make ~room:0 ~pos:(Vec.make (-4.5) 0.) ~angle in
          let expected = render_from built player ~width ~height
          and actual =
            render_from (Mount.build described).Scene.world player ~width
              ~height
          in
          match differing_pixel expected actual ~width ~height with
          | None -> ()
          | Some (x, y) ->
              let colour buffer =
                let c = Framebuffer.pixel buffer ~x ~y in
                Printf.sprintf "#%02x%02x%02x" c.Color.r c.Color.g c.Color.b
              in
              Alcotest.failf "pixel (%d, %d) is %s and should be %s" x y
                (colour actual) (colour expected)))
    [ 0.; 0.7; 2.4; 4.1 ]

let () =
  Alcotest.run "Stage"
    [
      ("the smallest game", the_smallest_game);
      ("the same picture", the_same_picture);
      ("winding", winding);
      ("runs", runs);
      ("doorways", doorways);
      ("malformed", malformed);
      ("both readers", both_readers);
      ("furnishing", furnishing);
    ]
