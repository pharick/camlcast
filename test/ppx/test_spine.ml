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

(* A wall whose ends are worked out rather than written down. It carries a
   position like any other -- it is a P.wall in a preprocessed file -- and
   there is still no line to write a new coordinate into, because what is
   written there is a rule. This is the case the whole predicate is about, and
   the one a reader meets as a surprise if nothing says so. *)
let spoke k =
  let angle = float_of_int k *. Float.pi /. 3. in
  Vec.make (4.5 *. cos angle) (4.5 *. sin angle)

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
            wall ~key:"spoke" ~height:1. ~material:stone (spoke 0) (spoke 1);
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

let pick_the_bench driver =
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view bench_end);
  frame driver ~holding:false
    ~at:(Camlcast_core.Overhead.to_panel view bench_end)

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

(* A press and a release in the same place is a click, and a click selects.
   Writing on one would put the pointer's position -- through a projection and
   back, so not quite what was there -- over a file nobody asked to change. *)
let a_click_selects_without_writing () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  let at = Camlcast_core.Overhead.to_panel view bench_end in
  driver.written := [];
  frame driver ~holding:true ~at;
  frame driver ~holding:false ~at;
  Alcotest.(check bool)
    "something is picked" true
    (Camlcast_edit.Session.selected driver.session
    <> Camlcast_edit.Sheet.Nothing);
  Alcotest.(check (list string))
    "and the file is untouched" []
    (List.map fst !(driver.written));
  Alcotest.(check int)
    "with no preview left over" 0
    (Camlcast_edit.Session.pending driver.session);
  stop driver

(* {1 The panel keys}

   Read by the overlay itself, so a game places it and has an editor rather
   than an editor's parts. These were hand-rolled in studio/ and untestable
   there, which is most of why they moved. *)

let closed_start () =
  let driver = start () in
  Camlcast_edit.Session.close driver.session;
  driver

(* The one that has to work while nothing is showing: an overlay whose opening
   key is read only when it is open cannot be opened. *)
let a_key_opens_a_closed_overlay () =
  let driver = closed_start () in
  Alcotest.(check bool)
    "closed to begin with" false
    (Camlcast_edit.Session.is_open driver.session);
  press driver Camlcast_core.Key.f2;
  Alcotest.(check bool)
    "and F2 opens it" true
    (Camlcast_edit.Session.is_open driver.session);
  Alcotest.(check bool)
    "on the panel it names" true
    (Camlcast_edit.Session.panel driver.session = Camlcast_edit.Session.Graph);
  stop driver

let the_keys_choose_between_panels () =
  let driver = start () in
  List.iter
    (fun (key, expected) ->
      press driver key;
      Alcotest.(check bool)
        "the panel the key names" true
        (Camlcast_edit.Session.panel driver.session = expected))
    [
      (Camlcast_core.Key.f3, Camlcast_edit.Session.Tree);
      (Camlcast_core.Key.f1, Camlcast_edit.Session.Plan);
      (Camlcast_core.Key.f2, Camlcast_edit.Session.Graph);
    ];
  press driver Camlcast_core.Key.f4;
  Alcotest.(check bool)
    "and one closes it" false
    (Camlcast_edit.Session.is_open driver.session);
  stop driver

(* One room in this fixture, so the next room is the same room -- which is the
   case that would divide by nothing if it counted rooms wrongly. *)
let tab_moves_to_the_next_room () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  press driver Camlcast_core.Key.tab;
  Alcotest.(check int)
    "wrapped rather than running off the end" 0
    (Camlcast_edit.Session.room driver.session);
  stop driver

(* {1 What cannot be dragged} *)

let spoke_end = spoke 0

(* Picked like anything else -- it is a wall and it is there -- and then
   nothing happens, because there is nowhere to write the answer. What matters
   is that it is drawn and pickable rather than hidden or refused. *)
let a_computed_wall_is_picked_but_not_dragged () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view spoke_end);
  (match Camlcast_edit.Session.selected driver.session with
  | Camlcast_edit.Sheet.Corner { item; _ } ->
      Alcotest.(check bool)
        "the computed wall, picked up like any other" true
        (Prim.describe item.Camlcast_edit.Sheet.what <> "")
  | _ -> Alcotest.fail "a computed wall should still be pickable");
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view (Vec.make 0. 0.));
  Alcotest.(check int)
    "and nothing is outstanding: there is no line to write it into" 0
    (Camlcast_edit.Session.pending driver.session);
  frame driver ~holding:false
    ~at:(Camlcast_core.Overhead.to_panel view (Vec.make 0. 0.));
  Alcotest.(check (list string))
    "nor is anything written" []
    (List.map fst !(driver.written));
  stop driver

(* Its arguments say why, one at a time. The height is a number written down
   and can be nudged; the two ends are worked out and cannot be touched. *)
let its_arguments_say_which_is_which () =
  let driver = start () in
  frame driver ~holding:false ~at:(0, 0);
  let view = projection driver in
  frame driver ~holding:true
    ~at:(Camlcast_core.Overhead.to_panel view spoke_end);
  frame driver ~holding:false
    ~at:(Camlcast_core.Overhead.to_panel view spoke_end);
  let items = Camlcast_edit.Sheet.items (forest_of driver) ~room:0 in
  let spoke_item =
    List.find
      (fun (item : Camlcast_edit.Sheet.item) ->
        Filename.check_suffix
          (Camlcast_loom.Path.to_debug_string item.Camlcast_edit.Sheet.path)
          "[spoke]")
      items
  in
  match
    Camlcast_edit.Link.locate
      (Camlcast_edit.Link.create (fun _ -> own_source ()))
      spoke_item.Camlcast_edit.Sheet.at
  with
  | Camlcast_edit.Link.Found source when positioned ->
      Alcotest.(check (list string))
        "a height written down, two ends worked out"
        [ "height numbers"; "material name"; "_ computed"; "_ computed" ]
        (List.map
           (fun (a : Camlcast_edit.Span.argument) ->
             Printf.sprintf "%s %s"
               (Option.value a.label ~default:"_")
               (match a.value with
               | Numbers _ -> "numbers"
               | Name _ -> "name"
               | Computed -> "computed"))
           (List.tl source.Camlcast_edit.Link.call.arguments));
      Alcotest.(check bool)
        "so something about it can still be changed" true
        (Camlcast_edit.Link.editable source)
  | _ when not positioned -> ()
  | _ -> Alcotest.fail "the spoke should have been found"

(* {1 Moving something into a file of its own}

   Named after the element's own key rather than typed. The engine reports keys
   as places on a keyboard and has no text input, so a name asked for would
   have to be spelled out of scancodes -- and a ~key is already there, already
   unique among its siblings, and already what the author called the thing. *)
let extracting_uses_the_key_as_the_name () =
  let driver = start () in
  pick_the_bench driver;
  driver.written := [];
  press driver Camlcast_core.Key.x;
  match !(driver.written) with
  | [] when not positioned -> ()
  | [] ->
      Alcotest.failf "nothing written; the session said: %s"
        (Option.value
           (Camlcast_edit.Session.said driver.session)
           ~default:"nothing")
  | written ->
      let paths = List.sort compare (List.map fst written) in
      Alcotest.(check bool)
        "a file named for the key" true
        (List.exists (fun p -> Filename.basename p = "bench.ml") paths);
      let component =
        List.assoc_opt "bench.ml" written
        |> Option.value ~default:(snd (List.hd written))
      in
      Alcotest.(check bool)
        "holding the call as it was written" true
        (let needle = "~height:0.6" in
         let rec holds i =
           i + String.length needle <= String.length component
           && (String.sub component i (String.length needle) = needle
              || holds (i + 1))
         in
         holds 0);
      Alcotest.(check bool)
        "declared at the top level, as the rule wants" true
        (let needle = "Element.declare ~name:\"bench\" @@ fun () ->" in
         let rec holds i =
           i + String.length needle <= String.length component
           && (String.sub component i (String.length needle) = needle
              || holds (i + 1))
         in
         holds 0);
      Alcotest.(check bool)
        "and both files parse" true
        (List.for_all
           (fun (_, text) ->
             match Camlcast_edit.Span.parse ~path:"x.ml" text with
             | Ok _ -> true
             | Error _ -> false)
           written);
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
      ( "clicking",
        [
          case "a click selects without writing" a_click_selects_without_writing;
        ] );
      ( "the panel keys",
        [
          case "a key opens a closed overlay" a_key_opens_a_closed_overlay;
          case "the keys choose between panels" the_keys_choose_between_panels;
          case "tab moves to the next room" tab_moves_to_the_next_room;
        ] );
      ( "what cannot be dragged",
        [
          case "a computed wall is picked but not dragged"
            a_computed_wall_is_picked_but_not_dragged;
          case "its arguments say which is which"
            its_arguments_say_which_is_which;
        ] );
      ( "moving it out",
        [
          case "extracting uses the key as the name"
            extracting_uses_the_key_as_the_name;
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
