(* The map, drawn into a buffer with no window behind it.

   An overlay is a thing you look at, so most of what it is worth cannot be
   asserted. What can is that it draws where it says it does, that it draws
   nothing anywhere else, and that the two colours it uses to say something —
   green for a linked doorway, red for one that leads nowhere — are the ones it
   ends up putting on the buffer. *)

open Camlcast_core
open Camlcast
open Support

let width = 320
let height = 240

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let wall_height = 4.
let flat = Plane.horizontal 0.
let floor = Room.floor ~plane:flat ~material:stone
let ceiling = Room.roof ~plane:(Plane.above flat wall_height) ~material:stone

let west_side =
  P.boundary ~closed:false ~height:wall_height ~material:stone
    (P.corners
       [
         Vec.make 0. 4.;
         Vec.make (-6.) 4.;
         Vec.make (-6.) (-4.);
         Vec.make 0. (-4.);
       ])

let east_side =
  P.boundary ~closed:false ~height:wall_height ~material:stone
    (P.corners
       [ Vec.make 0. (-4.); Vec.make 6. (-4.); Vec.make 6. 4.; Vec.make 0. 4. ])

let west_door =
  P.doorway ~name:"east" ~width:2. ~opening:2.5 ~height:wall_height
    ~material:stone (Vec.make 0. (-4.)) (Vec.make 0. 4.)

let east_door =
  P.doorway ~name:"west" ~width:2. ~opening:2.5 ~height:wall_height
    ~material:stone (Vec.make 0. 4.) (Vec.make 0. (-4.))

let joined =
  P.world ~atmosphere:Atmosphere.default
    ~spawn:("west", Vec.make (-3.) 0.)
    [
      P.room ~name:"west" ~floor ~ceiling [ west_side; west_door ];
      P.room ~name:"east" ~floor ~ceiling [ east_side; east_door ];
      P.link ("west", "east") ("east", "west");
    ]

let unlinked_world =
  let jambs, threshold =
    Room.doorway ~name:"east" ~width:2. ~opening:2.5 ~height:wall_height
      ~material:stone (Vec.make 0. (-4.)) (Vec.make 0. 4.)
  in
  let walls =
    Room.path ~closed:false ~height:wall_height ~material:stone
      [
        Vec.make 0. 4.;
        Vec.make (-6.) 4.;
        Vec.make (-6.) (-4.);
        Vec.make 0. (-4.);
      ]
    @ jambs
  in
  let shut = Room.make ~floor ~ceiling walls in
  let opened = Room.make ~thresholds:[ threshold ] ~floor ~ceiling walls in
  (* World.make refuses a threshold nothing links, which is the engine being
     right. open_doorway is the primitive that legitimately leaves one: between
     it and the link that fills it, the doorway is solid and draws as haze —
     which is the state a world grown a room at a time passes through, and the
     one the map exists to make visible. *)
  World.open_doorway
    (World.make
       ~rooms:[ ("west", shut) ]
       ~links:[] ~atmosphere:Atmosphere.default
       ~spawn:("west", Vec.make (-3.) 0.))
    ~room:0 ~opened

let drawn ?(world = (Mount.build joined).Scene.world) ?(found = []) () =
  let buffer = Framebuffer.offscreen ~width ~height in
  let player = Player.spawn world in
  Debug_map.draw buffer world player found;
  (buffer, player)

let holds buffer color =
  let x, y, w, h = Debug_map.panel buffer in
  let rec scan px py =
    if py >= y + h then false
    else if px >= x + w then scan x (py + 1)
    else if Framebuffer.pixel buffer ~x:px ~y:py = color then true
    else scan (px + 1) py
  in
  scan x y

let () =
  Alcotest.run "Debug map"
    [
      ( "where it draws",
        [
          case "inside its panel" (fun () ->
              let buffer, _ = drawn () in
              let x, y, w, h = Debug_map.panel buffer in
              let touched = ref 0 in
              for px = x to x + w - 1 do
                for py = y to y + h - 1 do
                  if Framebuffer.pixel buffer ~x:px ~y:py <> Color.rgb 0 0 0
                  then incr touched
                done
              done;
              Alcotest.(check bool)
                "the panel has something in it" true (!touched > 100));
          case "and nowhere else" (fun () ->
              (* An offscreen buffer starts black and the map is the only thing
                 that has drawn, so anything outside the panel is a leak. *)
              let buffer, _ = drawn () in
              let x, y, w, h = Debug_map.panel buffer in
              let outside = ref None in
              for px = 0 to width - 1 do
                for py = 0 to height - 1 do
                  let inside = px >= x && px < x + w && py >= y && py < y + h in
                  if
                    (not inside)
                    && Framebuffer.pixel buffer ~x:px ~y:py <> Color.rgb 0 0 0
                  then outside := Some (px, py)
                done
              done;
              match !outside with
              | None -> ()
              | Some (px, py) ->
                  Alcotest.failf "the map drew at (%d, %d), outside its panel"
                    px py);
        ] );
      ( "what it says",
        [
          case "a linked doorway is green" (fun () ->
              let buffer, _ = drawn () in
              Alcotest.(check bool)
                "green is on the buffer" true
                (holds buffer (Color.rgb 90 210 120));
              Alcotest.(check bool)
                "and red is not" false
                (holds buffer (Color.rgb 240 90 80)));
          case "a doorway that leads nowhere is red" (fun () ->
              let buffer, _ = drawn ~world:unlinked_world () in
              Alcotest.(check bool)
                "red is on the buffer" true
                (holds buffer (Color.rgb 240 90 80));
              Alcotest.(check bool)
                "and green is not" false
                (holds buffer (Color.rgb 90 210 120)));
          case "every wall carries a tick showing the way it faces" (fun () ->
              (* The one thing the map exists for: a boundary wound the wrong
                 way round is a row of these pointing out of the room. *)
              let buffer, _ = drawn () in
              Alcotest.(check bool)
                "the ticks are drawn" true
                (holds buffer (Color.rgb 90 170 255)));
          case "a diagnostic with a place is marked in it" (fun () ->
              let world = (Mount.build joined).Scene.world in
              let buffer, player = drawn ~world () in
              Alcotest.(check bool)
                "nothing to mark yet" false
                (holds buffer (Color.rgb 255 60 60));
              Debug_map.draw buffer world player
                [
                  {
                    Check.severity = Check.Error;
                    where = "west";
                    summary = "made up";
                    detail = [];
                    spot = Some (player.Player.room, player.Player.pos);
                  };
                ];
              Alcotest.(check bool)
                "and now there is" true
                (holds buffer (Color.rgb 255 60 60)));
          case "a diagnostic about another room is not" (fun () ->
              let world = (Mount.build joined).Scene.world in
              let buffer, player = drawn ~world () in
              Debug_map.draw buffer world player
                [
                  {
                    Check.severity = Check.Error;
                    where = "east";
                    summary = "made up";
                    detail = [];
                    spot = Some (1, Vec.make 3. 0.);
                  };
                ];
              Alcotest.(check bool)
                "the map draws one room, and marks only that one" false
                (holds buffer (Color.rgb 255 60 60)));
        ] );
    ]
