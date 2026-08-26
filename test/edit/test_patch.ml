(** Edits held against a frame on its way to being built.

    Two claims. What is edited changes, and what is not comes back untouched —
    literally untouched, the same value rather than an equal one, since a
    forest rebuilt wholesale every frame for the sake of one dragged wall is a
    cost paid by a game that is not being edited at all. *)

open Camlcast
open Camlcast_core

let case name body = Alcotest.test_case name `Quick body

let stone =
  Material.make ~pattern:(Texture.generate (fun ~u:_ ~v:_ -> Color.rgb 1 1 1))

let boundary =
  P.corners
    [
      Vec.make (-4.) (-4.); Vec.make 4. (-4.); Vec.make 4. 4.; Vec.make (-4.) 4.;
    ]

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:3. ~material:stone
          ~floor:(floor ~plane:(Plane.horizontal 0.) stone)
          ~ceiling:(roof stone) ~outline:boundary
          [
            spawn (Vec.make 0. 0.);
            wall ~key:"north" ~height:2.5 ~material:stone (Vec.make (-3.) 1.)
              (Vec.make 3. 1.);
          ];
      ])

(* The committed forest, and the frame that comes of it. A patch is handed the
   first and the world is made from the second, so both are needed to say that
   an edit reached the world rather than only the forest. *)
let render ?patch () =
  let taken = ref [] in
  let mount = Mount.create () in
  let scene =
    Mount.render mount level ~patch:(fun forest ->
        taken := forest;
        match patch with None -> forest | Some apply -> apply forest)
  in
  Mount.destroy mount;
  (!taken, scene)

let rec flatten (node : Watch.node) =
  node :: List.concat_map flatten node.Camlcast_loom.Host.children

let nodes forest = List.concat_map flatten forest

let keyed forest key =
  match
    List.find_opt
      (fun (node : Watch.node) ->
        Filename.check_suffix
          (Camlcast_loom.Path.to_debug_string node.Camlcast_loom.Host.path)
          ("[" ^ key ^ "]"))
      (nodes forest)
  with
  | Some node -> node
  | None -> Alcotest.failf "no node keyed %S" key

(* Found by its span across, which the test never changes, rather than by where
   it sits, which is the whole thing under test. A Room has no keys -- keys are
   the description's, and by the time there is a Room there is no description
   left -- so identifying it by geometry is what is available. *)
let north_wall_of (world : World.t) =
  let room = World.room world 0 in
  let rec go index =
    if index >= Room.wall_count room then
      Alcotest.fail "the room should have a wall spanning -3 to 3"
    else
      let wall = Room.wall_at room index in
      if
        Float.abs (wall.Room.a.Vec.x +. 3.) < 1e-9
        && Float.abs (wall.Room.b.Vec.x -. 3.) < 1e-9
      then wall
      else go (index + 1)
  in
  go 0

let an_edit_reaches_the_world () =
  let forest, _ = render () in
  let path = (keyed forest "north").Camlcast_loom.Host.path in
  let patch = Camlcast_edit.Patch.create () in
  Camlcast_edit.Patch.move_wall patch path ~a:(Vec.make (-3.) 2.)
    ~b:(Vec.make 3. 2.);
  Alcotest.(check int)
    "one edit outstanding" 1
    (Camlcast_edit.Patch.count patch);
  let _, scene = render ~patch:(Camlcast_edit.Patch.apply patch) () in
  Alcotest.(check (float 1e-9))
    "the wall is where the patch put it, in the world that was built" 2.
    (north_wall_of scene.Scene.world).Room.a.Vec.y

(* The description never moved. Dropping the edits puts the world back, which
   is what makes a patch something the runtime can live with. *)
let dropping_the_edits_puts_the_world_back () =
  let forest, _ = render () in
  let path = (keyed forest "north").Camlcast_loom.Host.path in
  let patch = Camlcast_edit.Patch.create () in
  Camlcast_edit.Patch.move_wall patch path ~a:(Vec.make (-3.) 2.)
    ~b:(Vec.make 3. 2.);
  let _, moved = render ~patch:(Camlcast_edit.Patch.apply patch) () in
  Alcotest.(check (float 1e-9))
    "moved" 2. (north_wall_of moved.Scene.world).Room.a.Vec.y;
  Camlcast_edit.Patch.clear patch;
  Alcotest.(check int) "nothing outstanding" 0 (Camlcast_edit.Patch.count patch);
  let _, back = render ~patch:(Camlcast_edit.Patch.apply patch) () in
  Alcotest.(check (float 1e-9))
    "and back where the description says" 1.
    (north_wall_of back.Scene.world).Room.a.Vec.y

let an_untouched_node_is_the_value_that_was_there () =
  let forest, _ = render () in
  let patch = Camlcast_edit.Patch.create () in
  Alcotest.(check bool)
    "with nothing outstanding, the forest itself comes back" true
    (Camlcast_edit.Patch.apply patch forest == forest);
  let path = (keyed forest "north").Camlcast_loom.Host.path in
  Camlcast_edit.Patch.move_wall patch path ~a:(Vec.make 0. 0.)
    ~b:(Vec.make 1. 0.);
  let patched = Camlcast_edit.Patch.apply patch forest in
  (* A node holding an edited descendant has to be rebuilt to hold it, so what
     can be shared is everything off that line. The spawn is a sibling of the
     edited wall: if it is not the same value, the walk is copying the forest
     rather than threading a change through it. *)
  let spawn nodes =
    List.find
      (fun (node : Watch.node) ->
        match node.Camlcast_loom.Host.prim with
        | Prim.Spawn _ -> true
        | _ -> false)
      nodes
  in
  Alcotest.(check bool)
    "a sibling of the edited node is the value that was there" true
    (spawn (nodes forest) == spawn (nodes patched));
  Alcotest.(check bool)
    "and the edited node is not" false
    (keyed forest "north" == keyed patched "north")

(* A description rebuilt into a different shape can leave an edit pointing at a
   path that now holds something else. That is staleness, not a mistake, and it
   is dropped rather than forced. *)
let an_edit_against_the_wrong_kind_is_ignored () =
  let forest, _ = render () in
  let path = (keyed forest "north").Camlcast_loom.Host.path in
  let patch = Camlcast_edit.Patch.create () in
  Camlcast_edit.Patch.move_sprite patch path (Vec.make 9. 9.);
  let _, scene = render ~patch:(Camlcast_edit.Patch.apply patch) () in
  Alcotest.(check (float 1e-9))
    "a sprite edit against a wall changes nothing" 1.
    (north_wall_of scene.Scene.world).Room.a.Vec.y

let () =
  Alcotest.run "Patch"
    [
      ( "what it changes",
        [
          case "an edit reaches the world" an_edit_reaches_the_world;
          case "dropping the edits puts the world back"
            dropping_the_edits_puts_the_world_back;
        ] );
      ( "what it leaves alone",
        [
          case "an untouched node is the value that was there"
            an_untouched_node_is_the_value_that_was_there;
          case "an edit against the wrong kind is ignored"
            an_edit_against_the_wrong_kind_is_ignored;
        ] );
    ]
