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
    ]
