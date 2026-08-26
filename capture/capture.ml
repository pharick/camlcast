(** A frame of the overlay, drawn without a window.

    {b Why this exists.} Everything about how the overlay {e looks} — whether a
    plan is legible at the size the engine actually renders at, whether a
    legend fits, how fine a corner is to take hold of — was being decided by
    reasoning about numbers. This draws the real thing instead.

    Nothing here opens a window or needs one. {!Camlcast_core.Framebuffer} has
    an offscreen buffer with no SDL behind it, and
    {!Camlcast_core.Renderer.draw_frame} is pure array writes into it; that
    pairing is how every rendering test in this repository already works. What
    is added is a script of frames — where the pointer is, what is held — so
    the overlay can be driven to the state worth looking at.

    {b Run it} with [dune exec capture/capture.exe -- <name>], which writes
    [<name>.ppm] beside itself. PPM because there is no PNG encoder in this
    tree and one is not worth acquiring to look at a picture: three ASCII
    numbers a pixel, and [sips] or Python turns it into a PNG. *)

open Camlcast
open Camlcast_core

(* The size a window of 1280x800 is actually rendered at, which is the number
   that matters and is not the window's: the engine divides both axes by the
   smallest whole number bringing the height under Config.max_render_height. *)
let width, height = Renderer.internal_size ~width:1280 ~height:800

let stone =
  Material.make
    ~pattern:
      (Texture.generate (fun ~u ~v ->
           Color.level (Color.rgb 150 150 160)
             (if ((u / 8) + (v / 8)) land 1 = 0 then 235 else 175)))

let brick =
  Material.make
    ~pattern:
      (Texture.generate (fun ~u ~v ->
           Color.level (Color.rgb 190 90 80)
             (if ((u / 8) + (v / 8)) land 1 = 0 then 235 else 175)))

let ground =
  Material.make
    ~pattern:
      (Texture.generate (fun ~u ~v ->
           Color.level (Color.rgb 116 110 98)
             (if ((u / 8) + (v / 8)) land 1 = 0 then 235 else 175)))

let session = Camlcast_edit.Session.create ()
let east = P.door ~name:"east" ~width:2. ~clearance:2.4 ()
let west = P.door ~name:"west" ~width:2. ~clearance:2.4 ()

(* A wall whose height is worked out from state, so a picture of the plan shows
   both cases: something to drag, and something drawn and plainly not for
   dragging. *)
let lamp =
  Element.declare ~name:"lamp" @@ fun (at : Vec.t) ->
  let lit, _ = Hook.use_state ~show:string_of_bool false in
  P.wall ~key:"lamp"
    ~height:(if lit then 1.6 else 1.2)
    ~material:brick at
    (Vec.add at (Vec.make 1.4 0.))

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:4. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) ground)
          ~ceiling:(roof stone)
          ~outline:
            (corners
               [
                 Vec.make (-6.) (-6.);
                 Vec.make 6. (-6.);
                 Vec.make 6. 6.;
                 Vec.make (-6.) 6.;
               ])
          [
            spawn (Vec.make (-3.) 0.);
            cut east ~along:(Vec.make 6. (-6.), Vec.make 6. 6.);
            wall ~key:"bench" ~height:0.6 ~material:brick (Vec.make (-2.) 3.)
              (Vec.make 2. 3.);
            lamp (Vec.make 1. (-4.));
          ];
        room ~name:"hall" ~height:3. ~material:brick ~floor:(floor ground)
          ~ceiling:(roof stone)
          ~outline:
            (corners
               [
                 Vec.make (-4.) (-3.);
                 Vec.make 4. (-3.);
                 Vec.make 4. 3.;
                 Vec.make (-4.) 3.;
               ])
          [ cut west ~along:(Vec.make (-4.) (-3.), Vec.make (-4.) 3.) ];
        connect east west;
        Camlcast_edit.Session.pointer session;
        hud [ crosshair (); Camlcast_edit.Session.overlay session () ];
      ])

(* One frame: render the description, draw the world it came to, and put the
   overlay over it -- which is what Run does, in the order it does it. *)
let mount = Mount.create ()
let buffer = Framebuffer.offscreen ~width ~height

(* The committed forest, kept so a script can work out where a thing was drawn
   and click on it -- which is the same arithmetic the session does, through
   the same projection, because a script computing it differently would be
   clicking somewhere the picture does not show it. *)
let forest = ref []

let frame ~actions =
  let watch = Camlcast_edit.Session.watch session in
  let scene =
    Mount.render mount
      (Camlcast_loom.Element.provide Events.context
         { Events.still with Events.actions; viewport = (width, height) }
         [ level ])
      ?trace:watch.Watch.trace
      ~patch:(fun committed ->
        forest := committed;
        match watch.Watch.patch with Some p -> p committed | None -> committed)
  in
  let player =
    Option.value scene.Scene.camera ~default:(Player.spawn scene.Scene.world)
  in
  Renderer.draw_frame buffer scene.Scene.world player;
  Overlay.draw buffer scene.Scene.hud

(* Three ASCII numbers a pixel. Nothing in this tree encodes a PNG and it is
   not worth acquiring one to look at a picture; sips reads this. *)
let write path =
  Out_channel.with_open_bin path (fun out ->
      Printf.fprintf out "P3\n%d %d\n255\n" width height;
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          let c = Framebuffer.pixel buffer ~x ~y in
          Printf.fprintf out "%d %d %d " c.Color.r c.Color.g c.Color.b
        done;
        Out_channel.output_char out '\n'
      done)

let held keys buttons control =
  match control with
  | Input.Key key -> List.exists (fun k -> k = key) keys
  | Input.Button button -> List.exists (fun b -> b = button) buttons

let actions = ref Input.untouched

let step ?(keys = []) ?(buttons = []) ?(pointer = (0, 0)) () =
  actions :=
    Input.advance !actions ~down:(held keys buttons) ~mouse:(0., 0.) ~pointer
      ~dt:0.016;
  frame ~actions:!actions

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "capture" in
  (* Settle, so the tree has a frame behind it and the plan has a room. *)
  step ();
  step ();
  (match name with
  | "plan" ->
      step ~keys:[ Key.f5 ] ();
      step ()
  | "graph" ->
      step ~keys:[ Key.f6 ] ();
      step ()
  | "tree" ->
      step ~keys:[ Key.f7 ] ();
      step ()
  | "picked" ->
      step ~keys:[ Key.f5 ] ();
      step ();
      (* Where the session drew the bench's near end, worked out its way. *)
      let items = Camlcast_edit.Sheet.items !forest ~room:0 in
      let x, y, side, _ =
        Camlcast_edit.Panel.square ~across:width ~down:height
      in
      let at =
        match Camlcast_edit.Sheet.bounds items with
        | None -> (0, 0)
        | Some bounds ->
            Overhead.to_panel
              (Overhead.fit ~bounds ~x ~y ~width:side ~height:side ~inset:12)
              (Vec.make (-2.) 3.)
      in
      step ~buttons:[ Input.Left ] ~pointer:at ();
      step ~pointer:at ()
  | other ->
      prerr_endline ("capture: no picture called " ^ other);
      exit 1);
  let path = name ^ ".ppm" in
  write path;
  Printf.printf "%s  %dx%d\n" path width height
