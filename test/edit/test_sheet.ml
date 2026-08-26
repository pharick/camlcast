(** A room from above: what is in it, and what is under the pointer.

    Drawn from the committed frame rather than from the world, so that
    everything on the plan carries the path an edit is named by. These check
    that it finds the right things, in the room asked for, and that picking one
    up agrees with where it was drawn. *)

open Camlcast
open Camlcast_core

let case name body = Alcotest.test_case name `Quick body

let stone =
  Material.make ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 1 1 1))

let square half =
  P.corners
    [
      Vec.make (-.half) (-.half);
      Vec.make half (-.half);
      Vec.make half half;
      Vec.make (-.half) half;
    ]

let flame = Image.make ~width:4 (fun ~u:_ ~v:_ -> (Color.rgb 255 200 100, 255))

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:(square 4.)
          [
            spawn (Vec.make 0. 0.);
            wall ~key:"bench" ~height:1. ~material:stone (Vec.make (-2.) 2.)
              (Vec.make 2. 2.);
            sprite ~key:"torch" ~size:1. ~image:flame (Vec.make 3. (-3.));
          ];
        room ~name:"cellar" ~height:2. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:(square 2.) [];
      ])

let forest () =
  let taken = ref [] in
  let mount = Mount.create () in
  ignore
    (Mount.render mount level ~patch:(fun forest ->
         taken := forest;
         forest));
  Mount.destroy mount;
  !taken

let view items =
  (* A panel like one drawn down the side of a frame. Bounds taken from the
     items themselves so the test does not depend on a Room. *)
  let points =
    List.concat_map
      (fun i ->
        match i.Camlcast_edit.Sheet.what with
        | Prim.Wall { a; b; _ } -> [ a; b ]
        | Prim.Sprite (s, _) -> [ s.Room.pos ]
        | Prim.Spawn at -> [ at ]
        | _ -> [])
      items
  in
  let bounds =
    List.fold_left
      (fun (x0, y0, x1, y1) (v : Vec.t) ->
        (Float.min x0 v.x, Float.min y0 v.y, Float.max x1 v.x, Float.max y1 v.y))
      (0., 0., 0., 0.) points
  in
  Overhead.fit ~bounds ~x:0 ~y:0 ~width:200 ~height:200 ~inset:10

let describe (item : Camlcast_edit.Sheet.item) = Prim.describe item.what

let the_plan_holds_what_stands_in_the_room () =
  let items = Camlcast_edit.Sheet.items (forest ()) ~room:0 in
  Alcotest.(check (list string))
    "the outline's four legs, the spawn, the bench and the torch"
    [
      "wall (-4,-4)-(4,-4)";
      "wall (4,-4)-(4,4)";
      "wall (4,4)-(-4,4)";
      "wall (-4,4)-(-4,-4)";
      "spawn at (0,0)";
      "wall (-2,2)-(2,2)";
      "sprite (3,-3)";
    ]
    (List.map describe items)

(* A world has no shared frame to draw two rooms in, so a plan is of one room.
   Asking for another gives that one and not a mixture. *)
let each_room_is_its_own_plan () =
  let items = Camlcast_edit.Sheet.items (forest ()) ~room:1 in
  Alcotest.(check (list string))
    "the cellar's four legs, and nothing of the plaza"
    [
      "wall (-2,-2)-(2,-2)";
      "wall (2,-2)-(2,2)";
      "wall (2,2)-(-2,2)";
      "wall (-2,2)-(-2,-2)";
    ]
    (List.map describe items);
  Alcotest.(check (list string))
    "and a room that is not there is empty" []
    (List.map describe (Camlcast_edit.Sheet.items (forest ()) ~room:9))

(* Picking up agrees with drawing. Both go through Overhead, which is why they
   can: the corner is looked for at the pixel the corner was drawn at. *)
let a_corner_is_found_where_it_was_drawn () =
  let items = Camlcast_edit.Sheet.items (forest ()) ~room:0 in
  let view = view items in
  let bench = List.find (fun i -> describe i = "wall (-2,2)-(2,2)") items in
  let at = Overhead.to_panel view (Vec.make (-2.) 2.) in
  match Camlcast_edit.Sheet.hit view [ bench ] at with
  | Corner { item; which } ->
      Alcotest.(check string) "the bench" "wall (-2,2)-(2,2)" (describe item);
      Alcotest.(check int) "by its first end" 0 which
  | Body _ -> Alcotest.fail "a corner should win over the body it is on"
  | Nothing -> Alcotest.fail "nothing found where the corner was drawn"

let the_body_is_found_between_the_corners () =
  let items = Camlcast_edit.Sheet.items (forest ()) ~room:0 in
  let view = view items in
  let bench = List.find (fun i -> describe i = "wall (-2,2)-(2,2)") items in
  let at = Overhead.to_panel view (Vec.make 0. 2.) in
  match Camlcast_edit.Sheet.hit view [ bench ] at with
  | Body item ->
      Alcotest.(check string)
        "the bench, by its middle" "wall (-2,2)-(2,2)" (describe item)
  | Corner _ -> Alcotest.fail "the middle of a wall is not a corner"
  | Nothing -> Alcotest.fail "nothing found on the wall itself"

let empty_space_is_nothing () =
  let items = Camlcast_edit.Sheet.items (forest ()) ~room:0 in
  let view = view items in
  let bench = List.find (fun i -> describe i = "wall (-2,2)-(2,2)") items in
  let at = Overhead.to_panel view (Vec.make (-3.5) (-3.5)) in
  Alcotest.(check bool)
    "well away from the one thing offered" true
    (match Camlcast_edit.Sheet.hit view [ bench ] at with
    | Nothing -> true
    | _ -> false)

(* The one predicate, made visible: what cannot be dragged is drawn, and drawn
   differently. Nothing here hides it. *)
let what_cannot_be_dragged_is_still_drawn () =
  let items = Camlcast_edit.Sheet.items (forest ()) ~room:0 in
  let view = view items in
  let ink = Color.rgb 255 255 255
  and computed = Color.rgb 90 90 90
  and picked = Color.rgb 255 0 0 in
  let drawn ~draggable =
    match
      Camlcast_edit.Sheet.draw ~view ~draggable ~ink ~computed ~picked
        ~selected:Camlcast_edit.Sheet.Nothing items
    with
    | Camlcast_loom.Element.Fragment { children; _ } -> List.length children
    | _ -> Alcotest.fail "a plan is a fragment"
  in
  Alcotest.(check int)
    "one element for each thing, whether or not it can be dragged"
    (List.length items)
    (drawn ~draggable:(fun _ -> false));
  Alcotest.(check int)
    "and the same when everything can" (List.length items)
    (drawn ~draggable:(fun _ -> true))

let () =
  Alcotest.run "Sheet"
    [
      ( "what is on the plan",
        [
          case "the plan holds what stands in the room"
            the_plan_holds_what_stands_in_the_room;
          case "each room is its own plan" each_room_is_its_own_plan;
        ] );
      ( "what is under the pointer",
        [
          case "a corner is found where it was drawn"
            a_corner_is_found_where_it_was_drawn;
          case "the body is found between the corners"
            the_body_is_found_between_the_corners;
          case "empty space is nothing" empty_space_is_nothing;
        ] );
      ( "what it draws",
        [
          case "what cannot be dragged is still drawn"
            what_cannot_be_dragged_is_still_drawn;
        ] );
    ]
