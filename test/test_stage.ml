(* Descriptions, and the worlds they assemble into.

   Nothing here opens a window either. A description becomes a World and a World
   is a value, so the whole of this step can be checked by comparing one against
   the world the old API builds by hand — which is the strongest thing available
   until there are pixels to compare, and stronger than reading the code twice.

   The reference is examples/room.ml, quoted in the README and compiled with the
   tree. If the declarative version of it stops agreeing with it, one of the two
   has moved. *)

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

(* {1 The reference: examples/room.ml, said twice} *)

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
        [ P.outline ~height ~material:stone corners ];
    ]

let the_smallest_game =
  [
    case "a described room is the room examples/room.ml builds" (fun () ->
        same_world built (Mount.build described).Scene.world);
    case "describing it twice into one mount changes nothing" (fun () ->
        let mount = Mount.create () in
        ignore (Mount.render mount described);
        same_world built (Mount.render mount described).Scene.world);
  ]

(* {1 Winding}

   The trap this step exists to close. *)

let winding =
  [
    case "corners in either order build the same room" (fun () ->
        let forwards =
          P.world ~atmosphere:Atmosphere.default ~spawn
            [
              P.room ~name:"room" ~floor ~ceiling
                [ P.outline ~height ~material:stone corners ];
            ]
        and backwards =
          P.world ~atmosphere:Atmosphere.default ~spawn
            [
              P.room ~name:"room" ~floor ~ceiling
                [ P.outline ~height ~material:stone (List.rev corners) ];
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
                    [ P.outline ~height ~material:stone (List.rev corners) ];
                ]))
            .Scene.world);
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
                      [ P.outline ~height ~material:stone (List.rev corners) ];
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
          P.path ~height ~material:stone
            [
              Vec.make 0. 4.;
              Vec.make (-6.) 4.;
              Vec.make (-6.) (-4.);
              Vec.make 0. (-4.);
            ];
          P.doorway ?door ~name:"east" ~width:2. ~opening:2.5 ~height
            ~material:stone (Vec.make 0. (-4.)) (Vec.make 0. 4.);
        ];
      P.room ~name:"east" ~floor ~ceiling
        [
          P.path ~height ~material:stone
            [
              Vec.make 0. (-4.);
              Vec.make 6. (-4.);
              Vec.make 6. 4.;
              Vec.make 0. 4.;
            ];
          P.doorway ?door ~name:"west" ~width:2. ~opening:2.5 ~height
            ~material:stone (Vec.make 0. 4.) (Vec.make 0. (-4.));
        ];
      P.link ("west", "east") ("east", "west");
    ]

let doorways =
  [
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
      (P.outline ~height ~material:stone corners);
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

(* {1 What goes in a room} *)

let furnishing =
  let dressed =
    P.world ~atmosphere:Atmosphere.default ~spawn
      [
        P.room ~name:"room" ~floor ~ceiling
          [
            P.outline ~height ~material:stone corners;
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
      ("doorways", doorways);
      ("malformed", malformed);
      ("furnishing", furnishing);
    ]
