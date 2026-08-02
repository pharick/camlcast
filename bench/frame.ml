(* What the declarative layer costs, against what a frame costs anyway.

   Every step of this rewrite has said the same thing about assembling a whole
   World every frame: a real cost, deliberately deferred, to be settled when a
   benchmark asks. This is the benchmark, and the number that matters is not how
   long a render takes but how it compares to drawing the frame it is for. A
   layer that costs a twentieth of what the renderer costs is a layer nobody
   will ever see; one that costs as much again has to be paid for.

   The answer, on the machine this was written on, at the 512x384 buffer the
   engine's own window renders into:

     describe five rooms         20 us
     draw five rooms         14,088 us

   A seventh of one percent. For one empty room it is a hundredth of one
   percent. Whatever the declarative layer is worth arguing about, it is not
   this — and the number that {e is} worth looking at is the other one: drawing
   five rooms takes most of a sixty-frame budget on its own. Ray.cast
   intersects every wall segment in the room once per screen column and there is
   no spatial index, which is the engine as it has always been and has nothing
   to do with anything above it.

   Run it with `dune exec bench/frame.exe`, or with `--profile release`, which
   moves the renderer by about a fifth and the layer by nothing. *)

open Camlcast_core
open Camlcast
open Bechamel
open Toolkit

let stone =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 150 150 160))

let ground =
  Material.make
    ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 116 110 98))

let picture =
  Image.make ~width:16 ~height:16 (fun ~u:_ ~v:_ -> (Color.rgb 200 90 60, 255))

let height = 4.
let flat = Plane.horizontal 0.

let square ~name ~at ~reach contents =
  P.(
    room ~name
      ~floor:(floor ~plane:flat ~material:ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
      (boundary ~height ~material:stone
         (corners
            [
              Vec.make (at -. reach) (-.reach);
              Vec.make (at +. reach) (-.reach);
              Vec.make (at +. reach) reach;
              Vec.make (at -. reach) reach;
            ])
      :: contents))

(* One room and nothing in it: the floor of what a description can cost. *)
let smallest =
  P.world ~atmosphere:Atmosphere.default
    ~spawn:("room", Vec.make 0. 0.)
    [ square ~name:"room" ~at:0. ~reach:6. [] ]

(* Five rooms, pillars, sprites and decals — the shape and roughly the size of
   demo/level.ml, which is the largest world this engine has. *)
let showcase =
  let pillar ~at ~side =
    P.(
      boundary ~height ~material:stone
        (corners
           [
             Vec.make (at -. side) (-.side);
             Vec.make (at +. side) (-.side);
             Vec.make (at +. side) side;
             Vec.make (at -. side) side;
           ]))
  in
  let furnished ~name ~at =
    square ~name ~at ~reach:8.
      (List.init 6 (fun index ->
           pillar ~at:(at -. 5. +. (float_of_int index *. 2.)) ~side:0.6)
      @ List.init 4 (fun index ->
          P.sprite
            ~key:("sprite" ^ string_of_int index)
            ~size:1.2 ~image:picture
            (Vec.make (at +. float_of_int index) 2.))
      @ [
          P.wall ~height:2.5 ~material:stone
            ~decals:
              [
                P.decal ~along:1. ~z:1.4 ~half_width:0.7 ~half_height:0.7
                  picture;
              ]
            (Vec.make (at -. 3.) (-4.))
            (Vec.make (at +. 3.) (-4.));
        ])
  in
  P.world ~atmosphere:Atmosphere.default
    ~spawn:("room0", Vec.make 0. 0.)
    (List.init 5 (fun index ->
         furnished
           ~name:("room" ^ string_of_int index)
           ~at:(float_of_int index *. 20.)))

let size description =
  let scene = Mount.build description in
  let world = scene.Scene.world in
  let walls =
    List.fold_left
      (fun total room -> total + Room.wall_count (World.room world room))
      0
      (List.init (World.room_count world) Fun.id)
  in
  (World.room_count world, walls)

(* The frame this is all for. Framebuffer.offscreen has no texture behind it and
   draw_frame makes no SDL call, so the renderer can be timed with nothing open
   — at the size Renderer.internal_size gives for the engine's own window, which
   is what it would really be drawing. *)
let drawing description =
  let scene = Mount.build description in
  let width, height =
    Renderer.internal_size ~width:Config.initial_width
      ~height:Config.initial_height
  in
  let buffer = Framebuffer.offscreen ~width ~height in
  let player = Player.spawn scene.Scene.world in
  Staged.stage (fun () -> Renderer.draw_frame buffer scene.Scene.world player)

(* A mount rendered into repeatedly, which is what a running game does — not a
   fresh one each time, since the first render of anything mounts it. *)
let rendering description =
  let mount = Mount.create () in
  ignore (Mount.render mount description);
  Staged.stage (fun () -> ignore (Mount.render mount description))

let tests =
  Test.make_grouped ~name:"a frame"
    [
      Test.make ~name:"describe one room" (rendering smallest);
      Test.make ~name:"describe five rooms" (rendering showcase);
      Test.make ~name:"draw one room" (drawing smallest);
      Test.make ~name:"draw five rooms" (drawing showcase);
    ]

let () =
  let rooms, walls = size showcase in
  Printf.printf "five rooms is %d rooms and %d walls, at %dx%d\n\n" rooms walls
    (fst
       (Renderer.internal_size ~width:Config.initial_width
          ~height:Config.initial_height))
    (snd
       (Renderer.internal_size ~width:Config.initial_width
          ~height:Config.initial_height));
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~quota:(Time.second 1.0) () in
  let raw = Benchmark.all cfg instances tests in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw) instances
  in
  let merged = Analyze.merge ols instances results in
  Hashtbl.iter
    (fun _ by_name ->
      let rows =
        Hashtbl.fold
          (fun name result rows ->
            match Analyze.OLS.estimates result with
            | Some (per_run :: _) -> (name, per_run) :: rows
            | _ -> rows)
          by_name []
      in
      List.iter
        (fun (name, nanoseconds) ->
          Printf.printf "%-32s %10.1f us\n" name (nanoseconds /. 1000.))
        (List.sort compare rows))
    merged
