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

(* The property the whole design rests on: for a description that keeps no
   state, reconciling is an optimisation and never a change of meaning. However
   much a root has been through, rendering a stateless description into it
   leaves the same scene as rendering it into a root that has been through
   nothing.

   The qualifier is the point and not a weakness. With state the property is
   deliberately false — a root that has been counted up to seven is supposed to
   differ from a fresh one, and that difference is the whole reason instances
   outlive descriptions. So the generator below builds from stateless parts,
   and what is being checked is that reconciling adds nothing of its own on
   top: no leftover child, no dropped sibling, no order the history invented. *)
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

(* A counter that hands its setter out through its props, since a test has no
   input layer to press a key on yet. *)
let counter =
  Element.declare ~name:"counter" @@ fun (latch : (int -> unit) ref) ->
  let count, set = Hook.use_state 0 in
  latch := set;
  Element.prim ("n=" ^ string_of_int count)

let state =
  [
    case "a value survives a re-render" (fun () ->
        let root = R.create () and latch = ref ignore in
        ignore (run root (counter latch));
        !latch 7;
        Alcotest.check scene "the slot kept what the setter put in it" "n=7"
          (scene_of root (counter latch)));
    case "a setter does not disturb the render that read the old value"
      (fun () ->
        let root = R.create () and latch = ref ignore in
        Alcotest.check scene "mounts at the initial value" "n=0"
          (scene_of root (counter latch));
        !latch 3;
        (* The scene on screen is still the old one. Nothing re-enters a render
           that has already answered; the change shows up next frame. *)
        Alcotest.check scene "and the next frame is where it shows" "n=3"
          (scene_of root (counter latch)));
    case "the root reports having work to do" (fun () ->
        let root = R.create () and latch = ref ignore in
        ignore (run root (counter latch));
        Alcotest.(check bool) "clean once rendered" false (R.dirty root);
        !latch 1;
        Alcotest.(check bool) "and dirty once set" true (R.dirty root);
        ignore (run root (counter latch));
        Alcotest.(check bool) "clean again" false (R.dirty root));
    case "state follows the key, not the position" (fun () ->
        let root = R.create () in
        let first = ref ignore and second = ref ignore in
        let pair a b =
          room [ counter ~key:"first" a; counter ~key:"second" b ]
        in
        ignore (run root (pair first second));
        !first 11;
        !second 22;
        ignore (run root (pair first second));
        (* Swap the order they are written in. Each count goes with its key. *)
        Alcotest.check scene "eleven is still first's" "room\n  n=22\n  n=11"
          (scene_of root
             (room [ counter ~key:"second" second; counter ~key:"first" first ])));
    case "a ref is the same box every render" (fun () ->
        let root = R.create () and seen = ref [] in
        let holder =
          Element.declare ~name:"holder" @@ fun () ->
          let box = Hook.use_ref 0 in
          seen := box :: !seen;
          incr box;
          Element.prim (string_of_int !box)
        in
        ignore (run root (holder ()));
        Alcotest.check scene "counted up in the same box" "2"
          (scene_of root (holder ()));
        match !seen with
        | [ second; first ] ->
            Alcotest.(check bool)
              "physically the same ref" true (first == second)
        | _ -> Alcotest.fail "expected exactly two renders");
  ]

let memo =
  let computed = ref 0 in
  let doubler =
    Element.declare ~name:"doubler" @@ fun (n : int) ->
    let value =
      Hook.use_memo ~deps:n (fun () ->
          incr computed;
          n * 2)
    in
    Element.prim (string_of_int value)
  in
  [
    case "computed once while the deps hold still" (fun () ->
        let root = R.create () in
        computed := 0;
        ignore (run root (doubler 5));
        ignore (run root (doubler 5));
        ignore (run root (doubler 5));
        Alcotest.(check int) "three renders, one computation" 1 !computed);
    case "recomputed when they change" (fun () ->
        let root = R.create () in
        computed := 0;
        Alcotest.check scene "five doubled" "10" (scene_of root (doubler 5));
        Alcotest.check scene "six doubled" "12" (scene_of root (doubler 6));
        Alcotest.(check int) "once per distinct dep" 2 !computed);
  ]

(* Effects are the seam where a component may reach outside itself, and the
   whole of what makes them safe is when they run and in what order. *)
let effects =
  let journal = ref [] in
  let note line = journal := line :: !journal in
  let read () = List.rev !journal in
  let watcher =
    Element.declare ~name:"watcher" @@ fun (tag : string) ->
    Hook.use_effect ~deps:tag (fun () ->
        note ("start " ^ tag);
        Some (fun () -> note ("stop " ^ tag)));
    Element.prim tag
  in
  [
    case "an effect runs after the frame it was described in" (fun () ->
        let root = R.create () in
        journal := [];
        let recorder =
          Element.declare ~name:"recorder" @@ fun () ->
          Hook.use_effect ~deps:() (fun () ->
              note "effect";
              None);
          note "render";
          Element.prim "x"
        in
        ignore (scene_of root (recorder ()));
        Alcotest.check
          Alcotest.(list string)
          "render first, then the effect" [ "render"; "effect" ] (read ()));
    case "cleanup runs before the next setup" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (watcher "a"));
        ignore (run root (watcher "b"));
        Alcotest.check
          Alcotest.(list string)
          "a stops before b starts"
          [ "start a"; "stop a"; "start b" ]
          (read ()));
    case "unchanged deps neither stop nor start anything" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (watcher "a"));
        ignore (run root (watcher "a"));
        ignore (run root (watcher "a"));
        Alcotest.check
          Alcotest.(list string)
          "once, and only once" [ "start a" ] (read ()));
    case "unmounting runs the outstanding cleanup" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (watcher "a"));
        ignore (run root Element.empty);
        Alcotest.check
          Alcotest.(list string)
          "stopped on the way out" [ "start a"; "stop a" ] (read ()));
    case "a component leaving and one arriving do not overlap" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (watcher ~key:"old" "a"));
        (* A different key, so this is a teardown and a mount rather than an
           update — and the cleanup still lands before the setup. *)
        ignore (run root (watcher ~key:"new" "b"));
        Alcotest.check
          Alcotest.(list string)
          "every cleanup before any setup"
          [ "start a"; "stop a"; "start b" ]
          (read ()));
  ]

(* The rule the whole slot mechanism rests on, and what happens when it is
   broken. See hook.mli for the one violation that still gets through. *)
let hook_order =
  [
    case "swapping one hook for another is caught" (fun () ->
        let root = R.create () in
        let swapper =
          Element.declare ~name:"swapper" @@ fun (as_ref : bool) ->
          if as_ref then ignore (Hook.use_ref 0) else ignore (Hook.use_state 0);
          Element.prim "x"
        in
        ignore (run root (swapper false));
        Alcotest.check_raises "the slot remembers what made it"
          (Hook.Hook_order_changed
             { at = "swapper"; expected = "use_state"; found = "use_ref" })
          (fun () -> ignore (run root (swapper true))));
    case "asking for more hooks than last time is caught" (fun () ->
        let root = R.create () in
        let grower =
          Element.declare ~name:"grower" @@ fun (extra : bool) ->
          ignore (Hook.use_state 0);
          if extra then ignore (Hook.use_state 1);
          Element.prim "x"
        in
        ignore (run root (grower false));
        Alcotest.check_raises "there was no second slot to claim"
          (Hook.Hook_order_changed
             { at = "grower"; expected = "nothing"; found = "use_state" })
          (fun () -> ignore (run root (grower true))));
    case "stopping early and asking for fewer is caught" (fun () ->
        let root = R.create () in
        let shrinker =
          Element.declare ~name:"shrinker" @@ fun (both : bool) ->
          ignore (Hook.use_state 0);
          if both then ignore (Hook.use_ref 1);
          Element.prim "x"
        in
        ignore (run root (shrinker true));
        Alcotest.check_raises "a slot was left unclaimed"
          (Hook.Hook_order_changed
             { at = "shrinker"; expected = "use_ref"; found = "nothing" })
          (fun () -> ignore (run root (shrinker false))));
    case "a hook outside a render says so" (fun () ->
        Alcotest.check_raises "nothing is handling the effect"
          Hook.Hook_outside_render (fun () -> ignore (Hook.use_state 0)));
  ]

let () =
  Alcotest.run "Loom"
    [
      ("mounting", mounting);
      ("keeping", keeping);
      ("keys", keys);
      ("state", state);
      ("memo", memo);
      ("effects", effects);
      ("hook order", hook_order);
      ( "properties",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick history_does_not_show;
        ] );
    ]
