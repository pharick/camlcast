(* Worlds that change under the player, and the things in them that move.

   The reconciler's whole claim is that a description can be rebuilt from
   scratch every frame and everything that should survive does. This is where
   that is asked of the parts of a world rather than of a mock host: rooms
   appearing, disappearing and changing order, and sprites that move. *)

open Camlcast_core
open Camlcast
open Support

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let height = 4.
let flat = Plane.horizontal 0.

(* A row of square rooms, each four cells across, side by side along x, so any
   of them can be named and none of them touch. *)
let box ~name ~at contents =
  P.(
    room ~name
      ~floor:(floor ~plane:flat ~material:stone)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
      (outline ~height ~material:stone
         [
           Vec.make (at -. 2.) (-2.);
           Vec.make (at +. 2.) (-2.);
           Vec.make (at +. 2.) 2.;
           Vec.make (at -. 2.) 2.;
         ]
      :: contents))

let world_of ~spawn rooms = P.world ~atmosphere:Atmosphere.default ~spawn rooms

let play mount frame description =
  Mount.render mount (Element.provide Events.context frame [ description ])

let where scene player = World.name scene.Scene.world player.Player.room

(* Two rooms and the doorway between them, so a frame can be made to cross. *)
let joined =
  P.(
    world ~atmosphere:Atmosphere.default
      ~spawn:("west", Vec.make (-2.) 0.)
      [
        room ~name:"west"
          ~floor:(floor ~plane:flat ~material:stone)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
            path ~height ~material:stone
              [
                Vec.make 0. 3.;
                Vec.make (-5.) 3.;
                Vec.make (-5.) (-3.);
                Vec.make 0. (-3.);
              ];
            doorway ~name:"east" ~width:2. ~opening:2.5 ~height ~material:stone
              (Vec.make 0. (-3.)) (Vec.make 0. 3.);
          ];
        room ~name:"east"
          ~floor:(floor ~plane:flat ~material:stone)
          ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
          [
            path ~height ~material:stone
              [
                Vec.make 0. (-3.);
                Vec.make 5. (-3.);
                Vec.make 5. 3.;
                Vec.make 0. 3.;
              ];
            doorway ~name:"west" ~width:2. ~opening:2.5 ~height ~material:stone
              (Vec.make 0. 3.) (Vec.make 0. (-3.));
          ];
        link ("west", "east") ("east", "west");
      ])

let () =
  Alcotest.run "Dynamics"
    [
      ( "a world that changes shape",
        [
          case "the player stays in the room, not in the index" (fun () ->
              (* The rooms are written in the other order on the second frame.
                 A player holding a bare index would be in the wrong one. *)
              let first =
                Mount.build
                  (world_of
                     ~spawn:("cellar", Vec.make 10. 0.)
                     [
                       box ~name:"hall" ~at:0. []; box ~name:"cellar" ~at:10. [];
                     ])
              in
              let player = Player.spawn first.Scene.world in
              Alcotest.(check string)
                "starts in the cellar" "cellar" (where first player);
              let second =
                Mount.build
                  (world_of
                     ~spawn:("cellar", Vec.make 10. 0.)
                     [
                       box ~name:"cellar" ~at:10. []; box ~name:"hall" ~at:0. [];
                     ])
              in
              let carried = Run.carry second ~was:"cellar" player in
              Alcotest.(check string)
                "still in the cellar" "cellar" (where second carried);
              Alcotest.check vec "and has not moved" player.Player.pos
                carried.Player.pos);
          case "a room added before it does not move the player" (fun () ->
              let first =
                Mount.build
                  (world_of
                     ~spawn:("hall", Vec.make 0. 0.)
                     [ box ~name:"hall" ~at:0. [] ])
              in
              let player = Player.spawn first.Scene.world in
              let grown =
                Mount.build
                  (world_of
                     ~spawn:("hall", Vec.make 0. 0.)
                     [
                       box ~name:"porch" ~at:20. []; box ~name:"hall" ~at:0. [];
                     ])
              in
              Alcotest.(check string)
                "still the hall" "hall"
                (where grown (Run.carry grown ~was:"hall" player)));
          case "facing and pitch are carried, not just the room" (fun () ->
              let first =
                Mount.build
                  (world_of
                     ~spawn:("hall", Vec.make 0. 0.)
                     [ box ~name:"hall" ~at:0. [] ])
              in
              let player =
                Player.pitch_by
                  (Player.make ~room:0 ~pos:(Vec.make 1. 1.) ~angle:0.9)
                  ~radians:0.2
              in
              let moved =
                Mount.build
                  (world_of
                     ~spawn:("hall", Vec.make 0. 0.)
                     [
                       box ~name:"attic" ~at:20. []; box ~name:"hall" ~at:0. [];
                     ])
              in
              ignore first;
              let carried = Run.carry moved ~was:"hall" player in
              Alcotest.check vec "where" player.Player.pos carried.Player.pos;
              Alcotest.check vec "facing" player.Player.dir carried.Player.dir;
              Alcotest.check close "pitch" player.Player.pitch
                carried.Player.pitch);
          case "a room that stops being described puts the player at the spawn"
            (fun () ->
              let first =
                Mount.build
                  (world_of
                     ~spawn:("hall", Vec.make 0. 0.)
                     [
                       box ~name:"hall" ~at:0. []; box ~name:"attic" ~at:10. [];
                     ])
              in
              let up = Player.make ~room:1 ~pos:(Vec.make 10.5 0.5) ~angle:0. in
              Alcotest.(check string)
                "was in the attic" "attic" (where first up);
              let shrunk =
                Mount.build
                  (world_of
                     ~spawn:("hall", Vec.make 0. 0.)
                     [ box ~name:"hall" ~at:0. [] ])
              in
              let carried = Run.carry shrunk ~was:"attic" up in
              Alcotest.(check string)
                "and lands at the spawn" "hall" (where shrunk carried);
              Alcotest.check vec "which is where the world says"
                (Vec.make 0. 0.) carried.Player.pos);
        ] );
      ( "going through a doorway",
        [
          case "a frame that crosses says which doorway, by name" (fun () ->
              (* What the trail demo reaches into Player.crossing for, without
                 the indices: a doorway is what the description called it. *)
              let scene = Mount.build joined in
              let at x = Player.make ~room:0 ~pos:(Vec.make x 0.) ~angle:0. in
              let movement =
                Engine.move scene.Scene.world (at (-0.4))
                  { Input.still with Input.forward = 1. }
              in
              Alcotest.(check bool)
                "the step went through" true (Player.crossed movement);
              Alcotest.(check string)
                "and ended up east" "east"
                (World.name scene.Scene.world movement.Player.player.Player.room);
              let named =
                List.map
                  (fun (c : Player.crossing) ->
                    ( World.name scene.Scene.world c.Player.from_room,
                      (Room.threshold_at
                         (World.room scene.Scene.world c.Player.from_room)
                         c.Player.from_threshold)
                        .Room.name,
                      World.name scene.Scene.world c.Player.to_room ))
                  movement.Player.crossings
              in
              Alcotest.(check (list (triple string string string)))
                "out of the west room by its east door"
                [ ("west", "east", "east") ]
                named);
          case "a component is told, once per doorway" (fun () ->
              let heard = ref [] in
              let watcher =
                Element.declare ~name:"watcher" @@ fun () ->
                Events.use_crossed (fun c ->
                    heard := (c.Events.from_room, c.Events.to_room) :: !heard);
                joined
              in
              let mount = Mount.create () in
              let play crossings =
                Mount.render mount
                  (Element.provide Events.context
                     { Events.still with Events.crossings }
                     [ watcher () ])
              in
              ignore (play []);
              Alcotest.(check (list (pair string string)))
                "nowhere yet" [] !heard;
              ignore
                (play
                   [
                     {
                       Events.from_room = "west";
                       from_doorway = "east";
                       to_room = "east";
                       to_doorway = "west";
                     };
                   ]);
              Alcotest.(check (list (pair string string)))
                "and now once"
                [ ("west", "east") ]
                !heard;
              ignore (play []);
              Alcotest.(check (list (pair string string)))
                "and not again"
                [ ("west", "east") ]
                !heard);
        ] );
      ( "things that move",
        [
          case "a component moves its own sprite every frame" (fun () ->
              let mote =
                Element.declare ~name:"mote" @@ fun (start : float) ->
                let y, set_y = Hook.use_state start in
                Events.use_frame (fun ~dt -> set_y (y +. dt));
                P.sprite ~size:0.4 ~image:poster (Vec.make 0. y)
              in
              let swarm =
                Element.declare ~name:"swarm" @@ fun () ->
                world_of
                  ~spawn:("hall", Vec.make 0. 0.)
                  [
                    box ~name:"hall" ~at:0.
                      (List.init 3 (fun index ->
                           mote
                             ~key:("mote" ^ string_of_int index)
                             (float_of_int index)));
                  ]
              in
              let mount = Mount.create () in
              let heights scene =
                let room = World.room scene.Scene.world 0 in
                List.init (Room.sprite_count room) (fun index ->
                    (Room.sprite_at room index).Room.pos.Vec.y)
              in
              let frame dt = { Events.still with Events.dt } in
              Alcotest.(check (list (float 1e-9)))
                "where they started" [ 0.; 1.; 2. ]
                (heights (play mount (frame 0.) (swarm ())));
              ignore (play mount (frame 0.5) (swarm ()));
              Alcotest.(check (list (float 1e-9)))
                "and each has moved by itself" [ 0.5; 1.5; 2.5 ]
                (heights (play mount (frame 0.) (swarm ()))));
          case "a keyed sprite keeps its own state when the list reorders"
            (fun () ->
              (* The reconciler's claim, asked of a world rather than of
                 strings: rewrite the list backwards and every mote is still
                 where it had got to. *)
              let mote =
                Element.declare ~name:"mote" @@ fun (start : float) ->
                let y, set_y = Hook.use_state start in
                Events.use_frame (fun ~dt -> set_y (y +. dt));
                P.sprite ~size:0.4 ~image:poster (Vec.make 0. y)
              in
              let swarm =
                Element.declare ~name:"swarm" @@ fun (backwards : bool) ->
                let motes =
                  List.init 3 (fun index ->
                      mote
                        ~key:("mote" ^ string_of_int index)
                        (float_of_int index))
                in
                world_of
                  ~spawn:("hall", Vec.make 0. 0.)
                  [
                    box ~name:"hall" ~at:0.
                      (if backwards then List.rev motes else motes);
                  ]
              in
              let mount = Mount.create () in
              let heights scene =
                let room = World.room scene.Scene.world 0 in
                List.init (Room.sprite_count room) (fun index ->
                    (Room.sprite_at room index).Room.pos.Vec.y)
              in
              ignore
                (play mount { Events.still with Events.dt = 0.5 } (swarm false));
              Alcotest.(check (list (float 1e-9)))
                "half a second on" [ 0.5; 1.5; 2.5 ]
                (heights (play mount Events.still (swarm false)));
              Alcotest.(check (list (float 1e-9)))
                "and written backwards, each is still its own" [ 2.5; 1.5; 0.5 ]
                (heights (play mount Events.still (swarm true))));
        ] );
    ]
