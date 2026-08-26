(** Picking a corner up, dragging it, and letting it go.

    The loop the whole overlay is for, driven without a window. A frame is
    rendered with a chosen {!Camlcast.Events.t}, which is what a run provides
    and what a test may provide too, so the mouse can be put where the test
    wants it and the session asked what it made of that.

    In the preprocessed directory because every step after the first needs a
    position: picking a corner up is geometry, and writing it back is finding
    the line that describes it.

    {b Nothing here touches a file.} The session is given a reader that hands
    back this suite's own source — which is the fixture, since a position names
    the file it was compiled from — and a writer that records rather than
    writes. What is asserted is the text that {e would} have been written. *)

open Camlcast

let case name body = Alcotest.test_case name `Quick body

let stone =
  Material.make ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 1 1 1))

let font =
  Font.make ~fallback:'?' ~width:6 ~height:10 ~first:32
    ~atlas:
      (Image.make ~width:96 ~height:60 (fun ~u:_ ~v:_ -> (Color.rgb 0 0 0, 255)))
    ()

(* Two doorways leading nowhere, for the graph panel to join. Made once at the
   top level, because a door carries the identity a connection joins by. *)
let north = P.door ~name:"north" ~width:1.6 ~clearance:2.2 ()
let south = P.door ~name:"south" ~width:1.6 ~clearance:2.2 ()

(* The wall this suite drags. Written out, so both its ends are literals. *)
let level session =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
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
            spawn (Vec.make 0. 0.);
            wall ~key:"bench" ~height:0.6 ~material:stone (Vec.make (-2.) 3.)
              (Vec.make 2. 3.);
            cut north ~along:(Vec.make 6. 6., Vec.make (-6.) 6.);
            cut south ~along:(Vec.make (-6.) (-6.), Vec.make 6. (-6.));
          ];
        Camlcast_edit.Session.pointer session;
        hud [ Camlcast_edit.Session.overlay session ~font ];
      ])

let own_source () =
  let rec attempt = function
    | [] -> Error (`Msg "cannot find this suite's source")
    | candidate :: rest -> (
        match In_channel.with_open_bin candidate In_channel.input_all with
        | source -> Ok source
        | exception Sys_error _ -> attempt rest)
  in
  attempt [ "test_spine.ml"; "test/ppx/test_spine.ml" ]

let viewport = (640, 400)

(* A run, with somewhere to put the mouse. *)
type driver = {
  session : Camlcast_edit.Session.t;
  mount : Mount.t;
  mutable actions : Input.actions;
  written : (string * string) list ref;
}

let start () =
  let written = ref [] in
  let session =
    Camlcast_edit.Session.create
      ~read:(fun _ -> own_source ())
      ~write:(fun path text ->
        written := (path, text) :: !written;
        Ok ())
      ()
  in
  Camlcast_edit.Session.show session Camlcast_edit.Session.Plan;
  { session; mount = Mount.create (); actions = Input.untouched; written }

let frame driver ~holding ~at =
  driver.actions <-
    Input.advance driver.actions
      ~down:(fun control -> holding && control = Input.Button Input.Left)
      ~mouse:(0., 0.) ~pointer:at ~dt:0.016;
  let events =
    { Events.still with Events.actions = driver.actions; viewport }
  in
  let watch = Camlcast_edit.Session.watch driver.session in
  ignore
    (Mount.render driver.mount
       (Camlcast_loom.Element.provide Events.context events
          [ level driver.session ])
       ?trace:watch.Watch.trace ?patch:watch.Watch.patch)

let stop driver = Mount.destroy driver.mount

(* Where the session itself puts a point on the panel. Worked out the same way
   it does -- the same square, the same bounds, the same projection -- because
   a test that computed it differently would be testing its own arithmetic. *)
let projection driver =
  let items =
    Camlcast_edit.Sheet.items
      (let taken = ref [] in
       let watch = Camlcast_edit.Session.watch driver.session in
       ignore
         (Mount.render driver.mount
            (Camlcast_loom.Element.provide Events.context
               { Events.still with Events.viewport }
               [ level driver.session ])
            ?trace:watch.Watch.trace
            ~patch:(fun forest ->
              taken := forest;
              match watch.Watch.patch with Some p -> p forest | None -> forest));
       !taken)
      ~room:0
  in
  let across, down = viewport in
  let x, y, side, _ = Camlcast_edit.Panel.square ~across ~down in
  match Camlcast_edit.Sheet.bounds items with
  | None -> Alcotest.fail "the room has no extent"
  | Some bounds ->
      Camlcast_core.Overhead.fit ~bounds ~x ~y ~width:side ~height:side
        ~inset:12

let forest_of driver =
  let taken = ref [] in
  let watch = Camlcast_edit.Session.watch driver.session in
  ignore
    (Mount.render driver.mount
       (Camlcast_loom.Element.provide Events.context
          { Events.still with Events.viewport }
          [ level driver.session ])
       ?trace:watch.Watch.trace
       ~patch:(fun forest ->
         taken := forest;
         match watch.Watch.patch with Some p -> p forest | None -> forest));
  !taken

let bench_end = Vec.make (-2.) 3.

(* Whether this build carries positions, from dune rather than from the
   rewriter -- see test/ppx/dune. Picking a corner up is geometry and works
   either way; dragging one needs the line that describes it, so a build
   without positions is a plan you can look at and not touch. That is the
   behaviour to assert there, not a case to skip. *)
let positioned =
  match Sys.getenv_opt "CAMLCAST_PROFILE" with
  | Some "release" -> false
  | Some _ | None -> true

let a_corner_is_picked_up () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view bench_end);
  (match Camlcast_edit.Session.selected driver.session with
  | Camlcast_edit.Sheet.Corner { item; which } ->
      Alcotest.(check string)
        "the bench, by the end that was clicked" "wall (-2,3)-(2,3)"
        (Prim.describe item.Camlcast_edit.Sheet.what);
      Alcotest.(check int) "its first end" 0 which
  | Camlcast_edit.Sheet.Body _ ->
      Alcotest.fail "a corner should win over the body it is on"
  | Camlcast_edit.Sheet.Nothing ->
      Alcotest.fail "nothing picked where the corner was drawn");
  stop driver

let dragging_moves_it_in_the_next_frame () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view bench_end);
  let moved_to = Vec.make (-2.) 1. in
  frame driver ~holding:true ~at:(Camlcast_core.Overhead.to_panel view moved_to);
  Alcotest.(check int)
    (if positioned then "one edit outstanding while the mouse is down"
     else "without a position there is nothing to drag")
    (if positioned then 1 else 0)
    (Camlcast_edit.Session.pending driver.session);
  Alcotest.(check (list string))
    "and nothing written yet" []
    (List.map fst !(driver.written));
  stop driver

let letting_go_writes_the_file () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view bench_end);
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view (Vec.make (-2.) 1.));
  frame driver ~holding:false
    ~at:(Camlcast_core.Overhead.to_panel view (Vec.make (-2.) 1.));
  (match !(driver.written) with
  | [] when not positioned -> ()
  | [] ->
      Alcotest.failf "nothing written; the session said: %s"
        (Option.value
           (Camlcast_edit.Session.said driver.session)
           ~default:"nothing")
  | (path, text) :: _ ->
      Alcotest.(check bool)
        "the file the wall was written in" true
        (Filename.check_suffix path "test_spine.ml");
      (* The claim: this file, with one point moved and everything else exactly
         as it was. Asserted against the source it was read from. *)
      let before =
        match own_source () with
        | Ok source -> source
        | Error (`Msg m) -> Alcotest.fail m
      in
      Alcotest.(check bool)
        "it is not what was there before" false (text = before);
      Alcotest.(check bool)
        "it still parses" true
        (match Camlcast_edit.Span.parse ~path text with
        | Ok _ -> true
        | Error _ -> false);
      Alcotest.(check int)
        "exactly one line differs" 1
        (let split = String.split_on_char '\n' in
         List.length
           (List.filter
              (fun (a, b) -> a <> b)
              (List.combine (split before) (split text)))));
  Alcotest.(check int)
    "and the preview is dropped, the description now saying it" 0
    (Camlcast_edit.Session.pending driver.session);
  stop driver

(* {1 The graph} *)

(* Two clicks and no key: picking a doorway that leads nowhere and then another
   is the gesture, and the first has to be remembered between them. *)
let joining_two_doorways_takes_two_clicks () =
  let driver = start () in
  Camlcast_edit.Session.show driver.session Camlcast_edit.Session.Graph;
  frame driver ~holding:false ~at:(0, 0);
  let graph = Camlcast_edit.Graph.read (forest_of driver) in
  let across, down = viewport in
  let x, y, side, _ = Camlcast_edit.Panel.square ~across ~down in
  let view = Camlcast_edit.Graph.place graph ~x ~y ~width:side ~height:side in
  let stub which =
    let placed = List.hd view in
    let doors = placed.Camlcast_edit.Graph.room.Camlcast_edit.Graph.doors in
    let door = List.nth doors which in
    let rec find px py =
      if py > y + side then Alcotest.failf "no stub %d found on the panel" which
      else if px > x + side then find x (py + 1)
      else
        match Camlcast_edit.Graph.hit view (px, py) with
        | Camlcast_edit.Graph.Door { door = d; _ }
          when d.Camlcast_edit.Graph.id = door.Camlcast_edit.Graph.id ->
            (px, py)
        | _ -> find (px + 1) py
    in
    find x y
  in
  driver.written := [];
  frame driver ~holding:true ~at:(stub 0);
  frame driver ~holding:false ~at:(stub 0);
  Alcotest.(check (list string))
    "one doorway picked, and nothing written for it" []
    (List.map fst !(driver.written));
  frame driver ~holding:true ~at:(stub 1);
  frame driver ~holding:false ~at:(stub 1);
  (match !(driver.written) with
  | [] when not positioned -> ()
  | [] ->
      Alcotest.failf "nothing written; the session said: %s"
        (Option.value
           (Camlcast_edit.Session.said driver.session)
           ~default:"nothing")
  | (_, text) :: _ ->
      Alcotest.(check bool)
        "a connection naming the two bindings, not the two names" true
        (let needle = "connect north south" in
         let rec holds i =
           i + String.length needle <= String.length text
           && (String.sub text i (String.length needle) = needle
              || holds (i + 1))
         in
         holds 0);
      Alcotest.(check bool)
        "and it still parses" true
        (match Camlcast_edit.Span.parse ~path:"test_spine.ml" text with
        | Ok _ -> true
        | Error _ -> false));
  stop driver

(* {1 The keys} *)

let press driver key =
  driver.actions <-
    Input.advance driver.actions
      ~down:(fun control -> control = Input.Key key)
      ~mouse:(0., 0.) ~pointer:(0, 0) ~dt:0.016;
  let events =
    { Events.still with Events.actions = driver.actions; viewport }
  in
  let watch = Camlcast_edit.Session.watch driver.session in
  ignore
    (Mount.render driver.mount
       (Camlcast_loom.Element.provide Events.context events
          [ level driver.session ])
       ?trace:watch.Watch.trace ?patch:watch.Watch.patch);
  (* Let go, so the next press is a press. *)
  frame driver ~holding:false ~at:(0, 0)

let pick_the_bench driver =
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view bench_end);
  frame driver ~holding:false
    ~at:(Camlcast_core.Overhead.to_panel view bench_end)

(* A height is not a coordinate: it cannot be dragged on a plan, which is the
   whole reason the keys exist. Nudging it writes the same way a drag does. *)
let a_field_can_be_nudged () =
  let driver = start () in
  pick_the_bench driver;
  driver.written := [];
  (* The bench's fields are its height, its material and its two ends; the
     first is the height. *)
  press driver Camlcast_core.Key.equals;
  (match !(driver.written) with
  | [] when not positioned -> ()
  | [] ->
      Alcotest.failf "nothing written; the session said: %s"
        (Option.value
           (Camlcast_edit.Session.said driver.session)
           ~default:"nothing")
  | (_, text) :: _ ->
      Alcotest.(check bool)
        "the height went up by a tenth" true
        (let rec holds i =
           i + 12 <= String.length text
           && (String.sub text i 12 = "~height:0.7 " || holds (i + 1))
         in
         holds 0));
  stop driver

let taking_it_back_is_offered () =
  let driver = start () in
  pick_the_bench driver;
  press driver Camlcast_core.Key.equals;
  driver.written := [];
  press driver Camlcast_core.Key.u;
  if positioned then
    Alcotest.(check bool)
      "the file is put back the way it was" true
      (List.length !(driver.written) = 1)
  else
    Alcotest.(check (option string))
      "with nothing written there is nothing to take back"
      (Some "nothing left to take back")
      (Camlcast_edit.Session.said driver.session);
  stop driver

let () =
  Alcotest.run "Spine"
    [
      ( "the loop",
        [
          case "a corner is picked up" a_corner_is_picked_up;
          case "dragging moves it in the next frame"
            dragging_moves_it_in_the_next_frame;
          case "letting go writes the file" letting_go_writes_the_file;
        ] );
      ( "the graph",
        [
          case "joining two doorways takes two clicks"
            joining_two_doorways_takes_two_clicks;
        ] );
      ( "the keys",
        [
          case "a field can be nudged" a_field_can_be_nudged;
          case "taking it back is offered" taking_it_back_is_offered;
        ] );
    ]
