(* What the crosshair is on, and who hears about it.

   Aim.crosshair is everything an interacting frame does, written as a function
   of values, so the whole of this can be driven with nothing open. Sight casts
   the same ray the renderer draws with, so a test that stands the player in a
   room and faces them at a wall is asking the same question a player would. *)

open Camlcast_core
open Camlcast
open Support

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let height = 4.
let flat = Plane.horizontal 0.

(* A square room whose four sides are separate walls, so each can be given
   handlers of its own and told apart by which one fires. *)
let corners =
  [ Vec.make (-4.) (-4.); Vec.make 4. (-4.); Vec.make 4. 4.; Vec.make (-4.) 4. ]

let side index =
  let a = List.nth corners index and b = List.nth corners ((index + 1) mod 4) in
  (a, b)

let world_of walls =
  P.(
    world ~atmosphere:Atmosphere.default
      ~spawn:("room", Vec.make 0. 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:flat ~material:stone)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          walls;
      ])

(* Facing due east from the middle is facing the wall from (4,-4) to (4,4),
   which is side 1 of the loop above. *)
let facing angle = Player.make ~room:0 ~pos:(Vec.make 0. 0.) ~angle
let east = facing 0.
let west = facing Float.pi

let plain index =
  let a, b = side index in
  P.wall ~height ~material:stone a b

let () =
  Alcotest.run "Aim"
    [
      ( "being looked at",
        [
          case "the wall the crosshair is on is told, and only it" (fun () ->
              let heard = ref [] in
              let watched index =
                let a, b = side index in
                P.wall ~height ~material:stone
                  ~on_gaze:(fun here -> heard := (index, here) :: !heard)
                  a b
              in
              let scene =
                Mount.build (world_of (List.init 4 (fun i -> watched i)))
              in
              let seen =
                Aim.crosshair scene.Scene.targets scene.Scene.world east
                  ~was:None ~used:false
              in
              Alcotest.(check bool)
                "something was found" true (Option.is_some seen);
              Alcotest.(check (list (pair int bool)))
                "the east wall, and nothing else"
                [ (1, true) ]
                !heard);
          case "turning away tells it so, once" (fun () ->
              let heard = ref [] in
              let watched index =
                let a, b = side index in
                P.wall ~height ~material:stone
                  ~on_gaze:(fun here -> heard := (index, here) :: !heard)
                  a b
              in
              let scene =
                Mount.build (world_of (List.init 4 (fun i -> watched i)))
              in
              let was =
                Aim.crosshair scene.Scene.targets scene.Scene.world east
                  ~was:None ~used:false
              in
              heard := [];
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world west ~was
                   ~used:false);
              Alcotest.(check (list (pair int bool)))
                "east let go before west took hold"
                [ (1, false); (3, true) ]
                (List.rev !heard));
          case "holding still says nothing more" (fun () ->
              let heard = ref 0 in
              let watched index =
                let a, b = side index in
                P.wall ~height ~material:stone
                  ~on_gaze:(fun _ -> incr heard)
                  a b
              in
              let scene =
                Mount.build (world_of (List.init 4 (fun i -> watched i)))
              in
              let was =
                Aim.crosshair scene.Scene.targets scene.Scene.world east
                  ~was:None ~used:false
              in
              Alcotest.(check int) "an enter" 1 !heard;
              let was =
                Aim.crosshair scene.Scene.targets scene.Scene.world east ~was
                  ~used:false
              in
              let _ =
                Aim.crosshair scene.Scene.targets scene.Scene.world east ~was
                  ~used:false
              in
              Alcotest.(check int)
                "and not one word since — it is an enter, not a poll" 1 !heard);
          case "a wall that asked for nothing is passed over" (fun () ->
              let heard = ref 0 in
              let scene =
                Mount.build
                  (world_of
                     [
                       plain 0;
                       (let a, b = side 1 in
                        P.wall ~height ~material:stone a b);
                       plain 2;
                       (let a, b = side 3 in
                        P.wall ~height ~material:stone
                          ~on_gaze:(fun _ -> incr heard)
                          a b);
                     ])
              in
              let seen =
                Aim.crosshair scene.Scene.targets scene.Scene.world east
                  ~was:None ~used:false
              in
              Alcotest.(check bool)
                "nothing to report on the east wall" true (Option.is_none seen);
              Alcotest.(check int) "and the west one never heard" 0 !heard);
        ] );
      ( "being used",
        [
          case "the use control reaches what is looked at" (fun () ->
              let opened = ref 0 in
              let scene =
                Mount.build
                  (world_of
                     (List.init 4 (fun index ->
                          let a, b = side index in
                          if index = 1 then
                            P.wall ~height ~material:stone
                              ~on_use:(fun _ -> incr opened)
                              a b
                          else plain index)))
              in
              let was =
                Aim.crosshair scene.Scene.targets scene.Scene.world east
                  ~was:None ~used:false
              in
              Alcotest.(check int) "not yet" 0 !opened;
              let was =
                Aim.crosshair scene.Scene.targets scene.Scene.world east ~was
                  ~used:true
              in
              Alcotest.(check int) "worked" 1 !opened;
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world west ~was
                   ~used:true);
              Alcotest.(check int)
                "and not while looking at something else" 1 !opened);
          case "a sprite can be used as well as a wall" (fun () ->
              let taken = ref 0 in
              let scene =
                Mount.build
                  (world_of
                     (List.init 4 plain
                     @ [
                         P.sprite ~size:1.6 ~image:poster
                           ~on_use:(fun _ -> incr taken)
                           (Vec.make 2. 0.);
                       ]))
              in
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world east
                   ~was:None ~used:true);
              Alcotest.(check int) "the barrel in front of the wall" 1 !taken);
        ] );
      ( "where on it",
        [
          case "a wall is told where the crosshair landed on it" (fun () ->
              (* What marking a wall needs, and the only thing the old chalk
                 demo reached into Sight for. *)
              let seen = ref None in
              let scene =
                Mount.build
                  (world_of
                     (List.init 4 (fun index ->
                          let a, b = side index in
                          if index = 1 then
                            P.wall ~height ~material:stone
                              ~on_use:(fun spot -> seen := Some spot)
                              a b
                          else plain index)))
              in
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world east
                   ~was:None ~used:true);
              match !seen with
              | None -> Alcotest.fail "the east wall was not told"
              | Some { Aim.where = Aim.On_wall { along; z; _ }; distance; _ } ->
                  (* Facing due east from the middle of a room reaching four
                     cells: the wall is four away, and the eye is half a cell
                     up, which is where a level ray meets it. *)
                  Alcotest.check close "how far" 4. distance;
                  Alcotest.check close "how high up the wall" Config.eye_height
                    z;
                  (* The wall runs from (4,-4) to (4,4), so the middle of it is
                     four cells along. *)
                  Alcotest.check close "how far along it" 4. along
              | Some _ -> Alcotest.fail "a wall should report where on it");
          case "a sprite has no where to report" (fun () ->
              let seen = ref None in
              let scene =
                Mount.build
                  (world_of
                     (List.init 4 plain
                     @ [
                         P.sprite ~size:1.6 ~image:poster
                           ~on_use:(fun spot -> seen := Some spot.Aim.where)
                           (Vec.make 2. 0.);
                       ]))
              in
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world east
                   ~was:None ~used:true);
              Alcotest.(check bool)
                "a picture that turns to face you has no coordinate" true
                (!seen = Some Aim.On_sprite));
          case "a mark can be left where the crosshair was" (fun () ->
              (* The chalk demo, without a rebuild anybody had to write: the
                 component keeps a list of marks and describes them as decals,
                 and the wall they are on is the wall that was told. *)
              let chalk =
                Element.declare ~name:"chalk" @@ fun () ->
                let marks, set_marks = Hook.use_state [] in
                world_of
                  (List.init 4 (fun index ->
                       let a, b = side index in
                       if index = 1 then
                         P.wall ~height ~material:stone
                           ~on_use:(fun spot ->
                             match spot.Aim.where with
                             | Aim.On_wall { along; z; _ } ->
                                 set_marks ((along, z) :: marks)
                             | _ -> ())
                           ~decals:
                             (List.map
                                (fun (along, z) ->
                                  P.decal ~along ~z ~half_width:0.2
                                    ~half_height:0.2 poster)
                                marks)
                           a b
                       else plain index))
              in
              let mount = Mount.create () in
              let render () = Mount.render mount (chalk ()) in
              let decals scene =
                List.length
                  (Room.wall_at (World.room scene.Scene.world 0) 1).Room.decals
              in
              let scene = render () in
              Alcotest.(check int) "a clean wall" 0 (decals scene);
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world east
                   ~was:None ~used:true);
              Alcotest.(check int) "and a mark on it" 1 (decals (render ())));
        ] );
      ( "a door that opens itself",
        [
          case "a component holds the door, and the doorway works it" (fun () ->
              (* The whole of what the demos call `doors`, without a callback
                 the engine handed anybody: the state belongs to the component,
                 the opening says what to do when it is used, and the world it
                 describes follows. *)
              let leaf = Door.make stone in
              let door =
                Element.declare ~name:"door" @@ fun () ->
                let shut, set_shut = Hook.use_state true in
                P.(
                  world ~atmosphere:Atmosphere.default
                    ~spawn:("west", Vec.make (-2.) 0.)
                    [
                      room ~name:"west"
                        ~floor:(floor ~plane:flat ~material:stone)
                        ~ceiling:
                          (roof ~plane:(Plane.above flat height) ~material:stone)
                        [
                          path ~height ~material:stone
                            [
                              Vec.make 0. 4.;
                              Vec.make (-6.) 4.;
                              Vec.make (-6.) (-4.);
                              Vec.make 0. (-4.);
                            ];
                          doorway
                            ?door:(if shut then Some leaf else None)
                            ~on_use:(fun _ -> set_shut (not shut))
                            ~name:"east" ~width:2. ~opening:2.5 ~height
                            ~material:stone (Vec.make 0. (-4.)) (Vec.make 0. 4.);
                        ];
                      room ~name:"east"
                        ~floor:(floor ~plane:flat ~material:stone)
                        ~ceiling:
                          (roof ~plane:(Plane.above flat height) ~material:stone)
                        [
                          path ~height ~material:stone
                            [
                              Vec.make 0. (-4.);
                              Vec.make 6. (-4.);
                              Vec.make 6. 4.;
                              Vec.make 0. 4.;
                            ];
                          doorway
                            ?door:(if shut then Some leaf else None)
                            ~name:"west" ~width:2. ~opening:2.5 ~height
                            ~material:stone (Vec.make 0. 4.) (Vec.make 0. (-4.));
                        ];
                      link ("west", "east") ("east", "west");
                    ])
              in
              let mount = Mount.create () in
              let render () = Mount.render mount (door ()) in
              let shut_now scene =
                Room.shut (Room.threshold_at (World.room scene.Scene.world 0) 0)
              in
              let scene = render () in
              Alcotest.(check bool) "shut to begin with" true (shut_now scene);
              let eye =
                Player.make ~room:0 ~pos:(Vec.make (-2.) 0.) ~angle:0.
              in
              ignore
                (Aim.crosshair scene.Scene.targets scene.Scene.world eye
                   ~was:None ~used:true);
              (* A setter shows up the frame after the one that called it. *)
              Alcotest.(check bool)
                "and open the frame after" false
                (shut_now (render ())));
        ] );
    ]
