(* The reconciler, against a host made of strings.

   This suite links camlcast_loom and nothing else — no camlcast, no SDL, not
   even Support, which is built on the engine's types. That is the point of the
   split stated as a test: if the runtime can be exercised this thoroughly with
   the engine absent, then the engine is genuinely absent from it.

   A host is two types and one function, so here it is: a primitive is a string
   and a scene is the forest drawn as an indented tree. Everything the
   reconciler decides is then readable as text — the scene says what was built,
   and the trace says what was kept, made and destroyed to get there, which is
   the claim that cannot be checked by looking at a picture. *)

open Camlcast_loom

let case name body = Alcotest.test_case name `Quick body

module Mock = struct
  type prim = string
  type scene = string

  let rec lines ~indent (node : prim Host.node) =
    (String.make indent ' ' ^ node.Host.prim)
    :: List.concat_map (lines ~indent:(indent + 2)) node.Host.children

  let assemble nodes =
    String.concat "\n" (List.concat_map (lines ~indent:0) nodes)
end

module R = Reconcile.Make (Mock)

(* Render, and keep what the reconciler said it was doing while it did it. *)
let run root element =
  let log = ref [] in
  let trace event = log := Trace.to_string Fun.id event :: !log in
  let scene = R.render ~trace root element in
  (scene, List.rev !log)

let scene_of root element = fst (run root element)
let log_of root element = snd (run root element)
let scene = Alcotest.string
let log = Alcotest.(list string)

(* Two components that differ in nothing but identity, which is the only thing
   the reconciler judges them by. *)
let torch = Element.declare ~name:"torch" (fun () -> Element.prim "flame")
let lamp = Element.declare ~name:"lamp" (fun () -> Element.prim "glow")
let room children = Element.prim ~children "room"
let wall = Element.prim "wall"

let mounting =
  [
    case "a tree of primitives becomes a forest" (fun () ->
        Alcotest.check scene "the room and its walls" "room\n  wall\n  wall"
          (scene_of (R.create ()) (room [ wall; wall ])));
    case "everything mounts, deepest last" (fun () ->
        Alcotest.check log "one line each"
          [
            "mount    #0 : room";
            "mount    #0/#0 : wall";
            "mount    #0/#1 : wall";
          ]
          (log_of (R.create ()) (room [ wall; wall ])));
    case "fragments flatten away" (fun () ->
        Alcotest.check scene "no trace of the fragment" "room\n  wall\n  wall"
          (scene_of (R.create ()) (room [ Element.fragment [ wall; wall ] ])));
    case "empty describes nothing and builds nothing" (fun () ->
        Alcotest.check scene "the middle child leaves no gap" "room\n  wall"
          (scene_of (R.create ()) (room [ Element.empty; wall ])));
  ]

let keeping =
  [
    case "the same description again keeps everything" (fun () ->
        let root = R.create () in
        ignore (run root (room [ wall; wall ]));
        Alcotest.check log "updates, and not one mount"
          [
            "update   #0 : room";
            "update   #0/#0 : wall";
            "update   #0/#1 : wall";
          ]
          (log_of root (room [ wall; wall ])));
    case "a component is kept across a re-render" (fun () ->
        let root = R.create () in
        ignore (run root (torch ()));
        Alcotest.check log "the component and its flame both survive"
          [ "update   torch"; "update   torch/#0 : flame" ]
          (log_of root (torch ())));
    case "a different component in the same place is a replacement" (fun () ->
        let root = R.create () in
        ignore (run root (torch ()));
        Alcotest.check log "the old goes deepest-first, then the new arrives"
          [
            "unmount  torch/#0 : flame";
            "unmount  torch";
            "mount    lamp";
            "mount    lamp/#0 : glow";
          ]
          (log_of root (lamp ())));
  ]

(* The reason keys exist. Each of these rearranges a list and asks whether the
   things in it were recognised where they ended up. *)
let keys =
  let keyed key = Element.prim ~key key in
  let unkeyed = Element.prim in
  [
    case "keyed children are found wherever they moved to" (fun () ->
        let root = R.create () in
        ignore (run root (room [ keyed "a"; keyed "b"; keyed "c" ]));
        Alcotest.check log "reordered, and every one of them kept"
          [
            "update   #0 : room";
            "update   #0/[c] : c";
            "update   #0/[a] : a";
            "update   #0/[b] : b";
          ]
          (log_of root (room [ keyed "c"; keyed "a"; keyed "b" ])));
    case "an unkeyed list only matches by position" (fun () ->
        let root = R.create () in
        ignore (run root (room [ unkeyed "a"; unkeyed "b" ]));
        (* Nothing is unmounted — but "b" has been updated into the place "a"
           held, which is exactly the state-follows-the-wrong-row bug that keys
           are there to prevent. *)
        Alcotest.check log "matched where they stand, not by what they are"
          [ "update   #0 : room"; "update   #0/#0 : b"; "update   #0/#1 : a" ]
          (log_of root (room [ unkeyed "b"; unkeyed "a" ])));
    case "a removed key unmounts exactly that one" (fun () ->
        let root = R.create () in
        ignore (run root (room [ keyed "a"; keyed "b"; keyed "c" ]));
        Alcotest.check log "b goes, a and c stay"
          [
            "update   #0 : room";
            "update   #0/[a] : a";
            "update   #0/[c] : c";
            "unmount  #0/[b] : b";
          ]
          (log_of root (room [ keyed "a"; keyed "c" ])));
    case "leftovers are unmounted in the order they were declared" (fun () ->
        let root = R.create () in
        ignore (run root (room [ keyed "a"; keyed "b"; keyed "c" ]));
        Alcotest.check log "a then c, never c then a"
          [
            "update   #0 : room";
            "update   #0/[b] : b";
            "unmount  #0/[a] : a";
            "unmount  #0/[c] : c";
          ]
          (log_of root (room [ keyed "b" ])));
    case "a shorter unkeyed list drops the last" (fun () ->
        let root = R.create () in
        ignore (run root (room [ unkeyed "a"; unkeyed "b" ]));
        Alcotest.check log "the second is the one that goes"
          [ "update   #0 : room"; "update   #0/#0 : a"; "unmount  #0/#1 : b" ]
          (log_of root (room [ unkeyed "a" ])));
    case "state follows a keyed component, not its position" (fun () ->
        let root = R.create () in
        let one = torch ~key:"one" and two = torch ~key:"two" in
        ignore (run root (room [ one (); two () ]));
        Alcotest.check log "both kept, neither remounted"
          [
            "update   #0 : room";
            "update   #0/torch[two]";
            "update   #0/torch[two]/#0 : flame";
            "update   #0/torch[one]";
            "update   #0/torch[one]/#0 : flame";
          ]
          (log_of root (room [ two (); one () ])));
  ]

(* The property the whole design rests on: reconciling is an optimisation and
   never a change of meaning. Whatever a root has been through, rendering a
   description into it has to leave the same scene as rendering that
   description into a root that has been through nothing. *)
let history_does_not_show =
  let open QCheck2 in
  let element =
    Gen.sized_size (Gen.int_bound 3)
    @@ Gen.fix (fun self depth ->
        let leaf =
          Gen.map
            (fun n -> Element.prim ("p" ^ string_of_int n))
            (Gen.int_bound 3)
        in
        if depth <= 0 then leaf
        else
          Gen.oneof_weighted
            [
              (3, leaf);
              (1, Gen.return Element.empty);
              ( 2,
                Gen.map
                  (fun children -> Element.prim ~children "box")
                  (Gen.list_size (Gen.int_bound 3) (self (depth - 1))) );
              ( 2,
                Gen.map2
                  (fun key children -> Element.prim ~key ~children "box")
                  (Gen.map (fun n -> "k" ^ string_of_int n) (Gen.int_bound 3))
                  (Gen.list_size (Gen.int_bound 3) (self (depth - 1))) );
              (1, Gen.map (fun () -> torch ()) (Gen.return ()));
              (1, Gen.map (fun () -> lamp ()) (Gen.return ()));
              ( 1,
                Gen.map
                  (fun children -> Element.fragment children)
                  (Gen.list_size (Gen.int_bound 3) (self (depth - 1))) );
            ])
  in
  QCheck2.Test.make ~count:500
    ~name:"a root's history does not show in its scene"
    (Gen.pair element element) (fun (first, second) ->
      let used = R.create () in
      ignore (R.render used first);
      let after_history = R.render used second in
      let from_scratch = R.render (R.create ()) second in
      String.equal after_history from_scratch)

let () =
  Alcotest.run "Loom"
    [
      ("mounting", mounting);
      ("keeping", keeping);
      ("keys", keys);
      ( "properties",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick history_does_not_show;
        ] );
    ]
