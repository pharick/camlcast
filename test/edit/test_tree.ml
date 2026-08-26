(** The component tree, folded back out of a trace.

    Two halves. The fold itself is driven with events written by hand, because
    the cases worth pinning — a refused frame, an unmount, the order rows come
    back in — are awkward to provoke through a real world and trivial to state
    directly. The remount count is driven through a real mount, because the
    mistake it exists to catch is a mistake in how a game is written and only
    the reconciler can decide it happened. *)

open Camlcast
open Camlcast_loom

let case name body = Alcotest.test_case name `Quick body

(* {1 The fold} *)

let at ?key ?name index parent = Path.child parent ?key ?name index

let labels tree =
  List.map
    (fun (row : Camlcast_edit.Tree.row) ->
      String.make (row.depth * 2) ' ' ^ row.label)
    (Camlcast_edit.Tree.rows tree)

let component name = Trace.Component name

let a_frame_is_shown_only_once_it_stands () =
  let tree = Camlcast_edit.Tree.create () in
  let root = at ~name:"level" 0 Path.root in
  Camlcast_edit.Tree.watch tree (Trace.Mounted (root, component "level"));
  Alcotest.(check (list string))
    "nothing yet: the frame has not been committed" [] (labels tree);
  Camlcast_edit.Tree.commit tree;
  Alcotest.(check (list string)) "and now it stands" [ "level" ] (labels tree)

(* A render can be refused after events have been reported. Those events
   describe the walk and not any tree, so what they said is dropped and the
   frame before is still what stands. *)
let a_refused_frame_is_not_shown () =
  let tree = Camlcast_edit.Tree.create () in
  let root = at ~name:"level" 0 Path.root in
  Camlcast_edit.Tree.watch tree (Trace.Mounted (root, component "level"));
  Camlcast_edit.Tree.commit tree;
  Camlcast_edit.Tree.watch tree
    (Trace.Mounted (at ~name:"torch" 0 root, component "torch"));
  Camlcast_edit.Tree.watch tree Trace.Refused;
  Camlcast_edit.Tree.commit tree;
  Alcotest.(check (list string))
    "the frame that stood, not the walk that did not" [ "level" ] (labels tree)

let an_unmount_takes_its_place_away () =
  let tree = Camlcast_edit.Tree.create () in
  let root = at ~name:"level" 0 Path.root in
  let torch = at ~name:"torch" 0 root in
  Camlcast_edit.Tree.watch tree (Trace.Mounted (root, component "level"));
  Camlcast_edit.Tree.watch tree (Trace.Mounted (torch, component "torch"));
  Camlcast_edit.Tree.commit tree;
  Alcotest.(check (list string))
    "outermost first, each before its children" [ "level"; "  torch" ]
    (labels tree);
  Camlcast_edit.Tree.watch tree (Trace.Updated (root, component "level"));
  Camlcast_edit.Tree.watch tree (Trace.Unmounted (torch, component "torch"));
  Camlcast_edit.Tree.commit tree;
  Alcotest.(check (list string)) "and gone" [ "level" ] (labels tree)

(* {1 The remount count, through a real reconciler} *)

let stone =
  Material.make ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 1 1 1))

let boundary =
  P.corners
    [
      Vec.make (-4.) (-4.); Vec.make 4. (-4.); Vec.make 4. 4.; Vec.make (-4.) 4.;
    ]

let level inside =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:boundary
          [ spawn (Vec.make 0. 0.); inside ];
      ])

let wall =
  P.wall ~key:"w" ~height:2. ~material:stone (Vec.make (-1.) 0.)
    (Vec.make 1. 0.)

(* Declared where it should be: once, when this module is initialised. *)
let steady = Element.declare ~name:"steady" @@ fun () -> wall

(* And declared where it should not be: inside another component's render, so
   that every frame builds a fresh closure which is therefore never the same
   component as last frame's. This is the mistake -- the frame looks right and
   the state silently resets.

   [inner] has to capture something for that to happen, which is worth knowing
   before writing one of these off as harmless. A closure over nothing is
   lifted to a constant and so is physically the same every render, and a
   component built from it is accidentally correct; capture one value from the
   render around it and the accident stops. Both spellings look equally wrong
   on the page. *)
let shifting =
  Element.declare ~name:"shifting" @@ fun (height : float) ->
  let inner =
    Element.declare ~name:"inner" @@ fun () ->
    P.wall ~key:"w" ~height ~material:stone (Vec.make (-1.) 0.) (Vec.make 1. 0.)
  in
  inner ()

let remounts_after ~frames description =
  let tree = Camlcast_edit.Tree.create () in
  let mount = Mount.create () in
  for _ = 1 to frames do
    ignore
      (Mount.render ~trace:(Camlcast_edit.Tree.watch tree) mount description);
    Camlcast_edit.Tree.commit tree
  done;
  Mount.destroy mount;
  List.map
    (fun (row : Camlcast_edit.Tree.row) -> (row.label, row.remounts))
    (Camlcast_edit.Tree.remounting tree)

let a_component_declared_once_never_remounts () =
  Alcotest.(check (list (pair string int)))
    "nothing to report over four frames" []
    (remounts_after ~frames:4 (level (steady ())))

let a_component_rebuilt_every_render_remounts_every_frame () =
  Alcotest.(check (list (pair string int)))
    "the component, once per frame after the first"
    [ ("inner", 3) ]
    (remounts_after ~frames:4 (level (shifting 2.)))

(* The wall under it is rebuilt just as often, and saying so twice would bury
   the one line worth reading. It is counted all the same, so a reader who
   wants the consequences can have them. *)
let the_consequences_are_counted_but_not_reported () =
  let tree = Camlcast_edit.Tree.create () in
  let mount = Mount.create () in
  for _ = 1 to 4 do
    ignore
      (Mount.render
         ~trace:(Camlcast_edit.Tree.watch tree)
         mount
         (level (shifting 2.)));
    Camlcast_edit.Tree.commit tree
  done;
  Mount.destroy mount;
  Alcotest.(check (list (pair string int)))
    "every place that remounted, component or not"
    [ ("inner", 3); ("wall (-1,0)-(1,0)", 3) ]
    (Camlcast_edit.Tree.rows tree
    |> List.filter (fun (row : Camlcast_edit.Tree.row) -> row.remounts > 0)
    |> List.map (fun (row : Camlcast_edit.Tree.row) ->
        (row.label, row.remounts)))

let () =
  Alcotest.run "Tree"
    [
      ( "the fold",
        [
          case "a frame is shown only once it stands"
            a_frame_is_shown_only_once_it_stands;
          case "a refused frame is not shown" a_refused_frame_is_not_shown;
          case "an unmount takes its place away" an_unmount_takes_its_place_away;
        ] );
      ( "remounting",
        [
          case "a component declared once never remounts"
            a_component_declared_once_never_remounts;
          case "a component rebuilt every render remounts every frame"
            a_component_rebuilt_every_render_remounts_every_frame;
          case "the consequences are counted but not reported"
            the_consequences_are_counted_but_not_reported;
        ] );
    ]
