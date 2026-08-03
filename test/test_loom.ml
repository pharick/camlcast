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

exception Refused

(* The same host, given the one thing every real host has and Mock does not: a
   description it will not build. Assembling is the last thing that can refuse a
   frame, and what a refusal leaves behind is only visible through one. *)
module Fragile = struct
  include Mock

  let rec unbuildable (node : prim Host.node) =
    node.Host.prim = "bad" || List.exists unbuildable node.Host.children

  let assemble nodes =
    if List.exists unbuildable nodes then raise Refused else Mock.assemble nodes
end

module F = Reconcile.Make (Fragile)

(* Render, and keep what the reconciler said it was doing while it did it. *)
let run root element =
  let log = ref [] in
  let trace event = log := Trace.to_string Fun.id event :: !log in
  let scene = R.render ~trace root element in
  (scene, List.rev !log)

let scene_of root element = fst (run root element)
let log_of root element = snd (run root element)

(* The same, for a root taken down rather than rendered. *)
let taking_down root =
  let log = ref [] in
  R.destroy
    ~trace:(fun event -> log := Trace.to_string Fun.id event :: !log)
    root;
  List.rev !log

let scene = Alcotest.string
let log = Alcotest.(list string)

(* Two components that differ in nothing but identity, which is the only thing
   the reconciler judges them by. *)
let torch = Element.declare ~name:"torch" (fun () -> Element.prim "flame")
let lamp = Element.declare ~name:"lamp" (fun () -> Element.prim "glow")
let room children = Element.prim ~children "room"
let wall = Element.prim "wall"

exception Broken

(* A component that will not let go quietly. Its cleanup raises, which is the
   one thing a flush has to carry on past: the work queued behind it is owed by
   a tree that is already standing. *)
let brittle =
  Element.declare ~name:"brittle" @@ fun () ->
  Hook.use_effect ~deps:() (fun () -> Some (fun () -> raise Broken));
  Element.prim "brittle"

exception Balked

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
          [ "update   torch#0"; "update   torch#0/#0 : flame" ]
          (log_of root (torch ())));
    case "a different component in the same place is a replacement" (fun () ->
        let root = R.create () in
        ignore (run root (torch ()));
        Alcotest.check log "the old goes deepest-first, then the new arrives"
          [
            "unmount  torch#0/#0 : flame";
            "unmount  torch#0";
            "mount    lamp#0";
            "mount    lamp#0/#0 : glow";
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
    (* An unkeyed child is its position and nothing else, so it is claimed at
       its own index rather than from what is left over after the keyed ones
       have been dealt with. Compacting instead would keep this wall's state
       across a move its path did not survive. *)
    case "an unkeyed child is claimed where it stands" (fun () ->
        let root = R.create () in
        ignore (run root (room [ keyed "a"; unkeyed "b" ]));
        Alcotest.check log "built afresh at the index it landed on"
          [
            "update   #0 : room";
            "mount    #0/#0 : b";
            "unmount  #0/[a] : a";
            "unmount  #0/#1 : b";
          ]
          (log_of root (room [ unkeyed "b" ])));
    case "a keyed fragment carries everything under it" (fun () ->
        let root = R.create () in
        (* Neither the torch nor the wall can be keyed usefully here: what moves
           is the pair, and the pair is a fragment. *)
        let pair key = Element.fragment ~key [ torch ~key:"lit" (); wall ] in
        ignore (run root (room [ pair "left"; pair "right" ]));
        Alcotest.check log "both found where they moved to, nothing rebuilt"
          [
            "update   #0 : room";
            "update   #0/[right]/torch[lit]";
            "update   #0/[right]/torch[lit]/#0 : flame";
            "update   #0/[right]/#1 : wall";
            "update   #0/[left]/torch[lit]";
            "update   #0/[left]/torch[lit]/#0 : flame";
            "update   #0/[left]/#1 : wall";
          ]
          (log_of root (room [ pair "right"; pair "left" ])));
    case "two children under one key is refused" (fun () ->
        let root = R.create () in
        Alcotest.check_raises "they would share a path"
          (Element.Duplicate_key { at = "#0"; key = "a" })
          (fun () -> ignore (run root (room [ keyed "a"; keyed "a" ]))));
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
(* Sibling keys have to be unique, and a generator drawing them from a pool of
   four will repeat one soon enough. The repeat is dropped rather than re-keyed,
   because what this property is about is the shapes reconciling can be handed
   and not which keys they were built with. *)
let distinct_keys children =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun child ->
      match Element.key child with
      | None -> true
      | Some key when Hashtbl.mem seen key -> false
      | Some key ->
          Hashtbl.add seen key ();
          true)
    children

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
          let children =
            Gen.map distinct_keys
              (Gen.list_size (Gen.int_bound 3) (self (depth - 1)))
          in
          let key =
            Gen.map (fun n -> "k" ^ string_of_int n) (Gen.int_bound 3)
          in
          Gen.oneof_weighted
            [
              (3, leaf);
              (1, Gen.return Element.empty);
              ( 2,
                Gen.map (fun children -> Element.prim ~children "box") children
              );
              ( 2,
                Gen.map2
                  (fun key children -> Element.prim ~key ~children "box")
                  key children );
              (1, Gen.map (fun () -> torch ()) (Gen.return ()));
              (1, Gen.map (fun () -> lamp ()) (Gen.return ()));
              (1, Gen.map (fun children -> Element.fragment children) children);
              ( 1,
                Gen.map2
                  (fun key children -> Element.fragment ~key children)
                  key children );
            ])
  in
  QCheck2.Test.make ~count:500
    ~name:"a stateless description does not remember its root's history"
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

(* A component that keeps a child while its own state says so, and a child that
   calls whatever it was handed on its way out. Between them: a cleanup reaching
   a setter that belongs to somebody else, which is the case a liveness flag
   kept on the root rather than on the component would get wrong. *)
let farewell =
  Element.declare ~name:"farewell" @@ fun (tell : unit -> unit) ->
  Hook.use_effect ~deps:() (fun () -> Some tell);
  Element.prim "child"

let nest =
  Element.declare ~name:"nest" @@ fun (latch : (bool -> unit) ref) ->
  let keeping, set_keeping = Hook.use_state true in
  latch := set_keeping;
  room (if keeping then [ farewell (fun () -> set_keeping true) ] else [])

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
    case "a setter kept past its component asks for nothing" (fun () ->
        (* A setter is a value, and a game may hand one to a timer or a
           subscription that lets go of it late or never. Called then, it writes
           a slot nothing will read again — and must not ask for a frame on its
           way, because a loop driven by {!R.dirty} would render one for a
           component that is not there. *)
        let root = R.create () and latch = ref ignore in
        ignore (run root (room [ counter latch ]));
        ignore (run root (room []));
        Alcotest.(check bool)
          "settled once the counter has gone" false (R.dirty root);
        !latch 1;
        Alcotest.(check bool)
          "and the stale setter did not stir it" false (R.dirty root));
    case "nor does one kept past the root itself" (fun () ->
        (* The same fact where it has teeth. A root that has been destroyed
           renders no more frames on its own, so a setter that marked it would
           mark it for good: {!R.dirty} answering yes for ever, and a loop
           polling it rebuilding a description it had just let go of. *)
        let root = R.create () and latch = ref ignore in
        ignore (run root (counter latch));
        R.destroy root;
        !latch 1;
        !latch 2;
        Alcotest.(check bool)
          "a destroyed root stays clean" false (R.dirty root);
        (* And it is empty rather than spent, so the setter the fresh mount
           hands out is a live one again. *)
        ignore (run root (counter latch));
        Alcotest.(check bool) "clean after the remount" false (R.dirty root);
        !latch 3;
        Alcotest.(check bool) "and this one is heard" true (R.dirty root));
    case "a parent's setter still wakes it, called on a child's way out"
      (fun () ->
        (* Which is why liveness is the component's and not the root's. A child
           leaving may hand something back to a parent that is staying, and the
           frame that asks for is a frame the parent will be in. *)
        let root = R.create () and latch = ref ignore in
        ignore (run root (nest latch));
        Alcotest.(check bool) "settled" false (R.dirty root);
        !latch false;
        (* The render that removes the child, which clears the flag on its way
           in and runs the child's cleanup on its way out. *)
        ignore (run root (nest latch));
        Alcotest.(check bool)
          "the cleanup reached a setter whose component is still here" true
          (R.dirty root));
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
    (* A hook's work is done in the effect handler, which runs outside the fiber
       the component is suspended in — so a raise from there is not, without
       care, a raise from the component. These three are that care: the failure
       is walked back to the point the hook was written, and everything that
       follows from being an ordinary expression follows from that. *)
    case "a compute that raises raises where it was called" (fun () ->
        let guarded =
          Element.declare ~name:"guarded" @@ fun () ->
          let value =
            try Hook.use_memo ~deps:0 (fun () -> raise Balked)
            with Balked -> "caught"
          in
          Element.prim value
        in
        Alcotest.check scene "the component's own try took it" "caught"
          (scene_of (R.create ()) (guarded ())));
    case "and a finaliser in the render body still runs" (fun () ->
        (* The one a raise past the fiber cannot do anything about: an abandoned
           fiber is not an unwound one, so a component holding something while
           it describes itself would never give it back. *)
        let released = ref false in
        let protecting =
          Element.declare ~name:"protecting" @@ fun () ->
          Fun.protect ~finally:(fun () -> released := true) @@ fun () ->
          Element.prim (Hook.use_memo ~deps:0 (fun () -> raise Balked))
        in
        Alcotest.check_raises "the failure still comes out" Balked (fun () ->
            ignore (scene_of (R.create ()) (protecting ())));
        Alcotest.(check bool)
          "and what it was holding was given back" true !released);
    case "and a refusal from inside one names its component" (fun () ->
        (* {!Element.Render_refused} is put on by a [try] around the component's
           own call, which is inside the fiber — so a refusal arriving past it
           would be an [Invalid_argument] with nothing to attach it to. The two
           spellings below are the same mistake written in two places and have
           to read alike. *)
        let named describe =
          match scene_of (R.create ()) (describe ()) with
          | scene -> "built " ^ scene
          | exception Element.Render_refused { at; message } ->
              at ^ ": " ^ message
        in
        let in_a_memo =
          Element.declare ~name:"choosy" @@ fun () ->
          Element.prim (Hook.use_memo ~deps:0 (fun () -> invalid_arg "no"))
        in
        let in_the_body =
          Element.declare ~name:"choosy" @@ fun () ->
          Element.prim (invalid_arg "no")
        in
        Alcotest.(check string)
          "the memo names it" "choosy#0: no" (named in_a_memo);
        Alcotest.(check string)
          "and so does the body, the same way" "choosy#0: no"
          (named in_the_body));
    (* Being an ordinary expression includes being caught, and a catch on the
       {e mount} render is the case worth pinning: the slot the compute was
       filling has to exist afterwards, empty, or the row settles one short of
       the component's own hook calls and every later render is one hook too
       many. *)
    case "a compute caught on the mount render is asked again" (fun () ->
        let tries = ref 0 in
        let flaky =
          Element.declare ~name:"flaky" @@ fun () ->
          let value =
            try
              string_of_int
                (Hook.use_memo ~deps:0 (fun () ->
                     incr tries;
                     if !tries = 1 then raise Balked else 7))
            with Balked -> "fallback"
          in
          Element.prim value
        in
        let root = R.create () in
        Alcotest.check scene "the first render caught it" "fallback"
          (scene_of root (flaky ()));
        Alcotest.check scene "the next one computes" "7"
          (scene_of root (flaky ()));
        Alcotest.check scene "and the one after remembers" "7"
          (scene_of root (flaky ()));
        Alcotest.(check int) "asked exactly twice" 2 !tries);
    case "and does not shift its neighbour's slot" (fun () ->
        (* The sharper half of the same claim: with a second memo after the
           caught one, a row settled one slot short would hand the second
           memo's value back to the first — at the first's type. *)
        let tries = ref 0 in
        let pair =
          Element.declare ~name:"pair" @@ fun () ->
          let first =
            try
              Hook.use_memo ~deps:0 (fun () ->
                  incr tries;
                  if !tries = 1 then raise Balked else 41)
            with Balked -> 0
          in
          let second = Hook.use_memo ~deps:0 (fun () -> "own") in
          Element.prim (string_of_int first ^ "/" ^ second)
        in
        let root = R.create () in
        Alcotest.check scene "caught on the mount" "0/own"
          (scene_of root (pair ()));
        Alcotest.check scene "each hook kept its own slot" "41/own"
          (scene_of root (pair ())));
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
  (* The same, except that it will not start under one particular tag. A setup
     that raises is the awkward case because of when it raises: the cleanup of
     the run before it has already been called, at the top of this same flush,
     so what the slot is holding at that moment is a cleanup nobody owes. *)
  let balky =
    Element.declare ~name:"balky" @@ fun (tag : string) ->
    Hook.use_effect ~deps:tag (fun () ->
        if tag = "no" then raise Balked;
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
    (* The queue is emptied before any of it runs, so a flush that stopped at
       the first raise would leave the rest owed with nothing holding them. The
       tree these belong to is already committed, which is what makes them owed
       rather than optional. *)
    case "a cleanup that raises does not cancel what is queued behind it"
      (fun () ->
        let root = R.create () in
        journal := [];
        ignore
          (run root
             (room
                [
                  watcher ~key:"a" "a";
                  brittle ~key:"b" ();
                  watcher ~key:"c" "c";
                ]));
        journal := [];
        Alcotest.check_raises "the first one to go wrong comes back out" Broken
          (fun () -> ignore (run root (room [ watcher ~key:"a" "d" ])));
        Alcotest.check
          Alcotest.(list string)
          "everything owed ran: the cleanups either side, then the new setup"
          [ "stop a"; "stop c"; "start d" ]
          (read ()));
    (* A setup can go wrong too, and it leaves a subtler mess than a cleanup
       does. The cleanup it replaces has already run by then, so a slot still
       holding it is holding something owed to nobody. *)
    case "a setup that raises leaves no cleanup behind it" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (balky "a"));
        Alcotest.check_raises "the one that would not start says so" Balked
          (fun () -> ignore (run root (balky "no")));
        (* "stop a" ran on the way in to that flush. Taking the root down must
           not run it again — it would be a second close of one open thing. *)
        R.destroy root;
        Alcotest.check
          Alcotest.(list string)
          "started once, stopped once" [ "start a"; "stop a" ] (read ()));
    case "nor a claim on the deps it failed under" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (balky "a"));
        Alcotest.check_raises "once" Balked (fun () ->
            ignore (run root (balky "no")));
        (* The same deps, so there is nothing new to try: the raise came back
           out of the render that flushed it, and that was the report. A slot
           left holding the old deps would read this as a change and take the
           spent cleanup out again. *)
        ignore (run root (balky "no"));
        Alcotest.check
          Alcotest.(list string)
          "the second attempt is not made" [ "start a"; "stop a" ] (read ()));
    case "and a setup that raises on mount takes nothing" (fun () ->
        let root = R.create () in
        journal := [];
        Alcotest.check_raises "the raise comes back out of the render" Balked
          (fun () -> ignore (run root (balky "no")));
        R.destroy root;
        Alcotest.check
          Alcotest.(list string)
          "so there is nothing to give back" [] (read ()));
  ]

(* The other end of a mount. A root that is only ever rendered into runs a
   cleanup when the component it belongs to goes away — and a root that is let
   go of while everything is still in it used to run none at all. *)
let destroying =
  let journal = ref [] in
  let note line = journal := line :: !journal in
  let read () = List.rev !journal in
  let beacon =
    Element.declare ~name:"beacon" @@ fun (tag : string) ->
    Hook.use_effect ~deps:tag (fun () ->
        note ("lit " ^ tag);
        Some (fun () -> note ("out " ^ tag)));
    Element.prim tag
  in
  [
    case "destroying a root runs every cleanup it was owed" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (room [ beacon "a"; beacon "b" ]));
        Alcotest.check
          Alcotest.(list string)
          "both alight" [ "lit a"; "lit b" ] (read ());
        R.destroy root;
        Alcotest.check
          Alcotest.(list string)
          "and both out, in the order they were lit"
          [ "lit a"; "lit b"; "out a"; "out b" ]
          (read ()));
    case "and reports what it took down, deepest first" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (room [ beacon "a" ]));
        Alcotest.check log "the same order an unmount is reported in"
          [
            "unmount  #0/beacon#0/#0 : a";
            "unmount  #0/beacon#0";
            "unmount  #0 : room";
          ]
          (taking_down root));
    case "destroying it twice owes nothing the second time" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (beacon "a"));
        R.destroy root;
        R.destroy root;
        (* The cleanup is read out of its slot without being cleared, so a row
           walked twice would put it out twice. *)
        Alcotest.check
          Alcotest.(list string)
          "out once" [ "lit a"; "out a" ] (read ()));
    case "a destroyed root is empty, not spent" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (beacon "a"));
        R.destroy root;
        Alcotest.check scene "renders again" "a" (scene_of root (beacon "a"));
        Alcotest.check
          Alcotest.(list string)
          "as a mount and not as an update"
          [ "lit a"; "out a"; "lit a" ]
          (read ()));
    case "one cleanup raising does not keep the others in" (fun () ->
        let root = R.create () in
        journal := [];
        ignore (run root (room [ beacon "a"; brittle (); beacon "c" ]));
        journal := [];
        Alcotest.check_raises "the teardown says what went wrong" Broken
          (fun () -> R.destroy root);
        Alcotest.check
          Alcotest.(list string)
          "and everything either side of it was still given back"
          [ "out a"; "out c" ] (read ()));
    case "and does not leave a torn-down root asking for a frame" (fun () ->
        (* The flush re-raises what a cleanup raised, and clearing [dirty] on
           the line after would be skipped on that path — a root stuck
           answering yes with every row silenced and nothing left to render. *)
        let root = R.create () in
        let ask = ref (fun () -> ()) in
        let keeper =
          Element.declare ~name:"keeper" @@ fun () ->
          let _, set = Hook.use_state 0 in
          ask := (fun () -> set 1);
          Element.prim "kept"
        in
        ignore (run root (room [ keeper (); brittle () ]));
        !ask ();
        Alcotest.(check bool) "a frame was asked for" true (R.dirty root);
        Alcotest.check_raises "the teardown still reports the raise" Broken
          (fun () -> R.destroy root);
        Alcotest.(check bool)
          "and the root is quiet, not stuck" false (R.dirty root));
  ]

(* Assembling is the last thing that can refuse a frame, and it happens after
   every component in it has run and queued whatever it wanted done. What a
   refusal must not do is leave any of that lying about for the next frame. *)
let refusing =
  let journal = ref [] in
  let note line = journal := line :: !journal in
  let read () = List.rev !journal in
  let flare =
    Element.declare ~name:"flare" @@ fun (tag : string) ->
    Hook.use_effect ~deps:tag (fun () ->
        note ("start " ^ tag);
        None);
    Element.prim tag
  in
  let lantern =
    Element.declare ~name:"lantern"
    @@ fun ((latch, broken) : (int -> unit) ref * bool) ->
    let count, set = Hook.use_state 0 in
    latch := set;
    Element.prim
      ~children:(if broken then [ Element.prim "bad" ] else [])
      ("n=" ^ string_of_int count)
  in
  let tally =
    Element.declare ~name:"tally" @@ fun (broken : bool) ->
    let runs = Hook.use_ref 0 in
    incr runs;
    Element.prim
      ~children:(if broken then [ Element.prim "bad" ] else [])
      ("runs=" ^ string_of_int !runs)
  in
  [
    case "a refused render starts nothing" (fun () ->
        let root = F.create () in
        journal := [];
        Alcotest.check_raises "the host would not build it" Refused (fun () ->
            ignore (F.render root (flare "bad")));
        Alcotest.check Alcotest.(list string) "nothing lit" [] (read ()));
    case "and the frame after it starts only its own" (fun () ->
        let root = F.create () in
        journal := [];
        (try ignore (F.render root (flare "bad")) with Refused -> ());
        ignore (F.render root (flare "good"));
        (* The effect the rejected description asked for belongs to a component
           that was never mounted, and there is no later frame it is owed to. *)
        Alcotest.check
          Alcotest.(list string)
          "only the description that was built" [ "start good" ] (read ()));
    case "a refused render leaves the tree where it was" (fun () ->
        let root = F.create () and latch = ref ignore in
        ignore (F.render root (lantern (latch, false)));
        !latch 4;
        Alcotest.check_raises "refused" Refused (fun () ->
            ignore (F.render root (lantern (latch, true))));
        let kept = ref [] in
        let built =
          F.render
            ~trace:(fun event -> kept := Trace.to_string Fun.id event :: !kept)
            root
            (lantern (latch, false))
        in
        Alcotest.check scene "the state is where the setter put it" "n=4" built;
        Alcotest.check log "and the component was kept, not mounted again"
          [ "update   lantern#0"; "update   lantern#0/#0 : n=4" ]
          (List.rev !kept));
    case "and says so, so its own trace can be read" (fun () ->
        (* The events of a refused render are what the reconciler was doing when
           it walked into the refusal, which is worth having. What they are not
           is tree history, and read as tree history they contradict the frame
           after: below, [torch] is unmounted by the refused render and updated
           by the next, and an update is the reconciler's promise that the state
           carried over. Both are true of the walk. The last line is what tells
           a reader — and the suite above — which of the two it is reading. *)
        let root = F.create () in
        let seen = ref [] in
        let watch event = seen := Trace.to_string Fun.id event :: !seen in
        ignore (F.render root (room [ torch () ]));
        Alcotest.check_raises "refused" Refused (fun () ->
            ignore (F.render ~trace:watch root (room [ Element.prim "bad" ])));
        Alcotest.check log "everything it did, and then that none of it stood"
          [
            "update   #0 : room";
            "unmount  #0/torch#0/#0 : flame";
            "unmount  #0/torch#0";
            "mount    #0/#0 : bad";
            "refused";
          ]
          (List.rev !seen);
        seen := [];
        ignore (F.render ~trace:watch root (room [ torch () ]));
        Alcotest.check log "and the torch it said it unmounted is still there"
          [
            "update   #0 : room";
            "update   #0/torch#0";
            "update   #0/torch#0/#0 : flame";
          ]
          (List.rev !seen));
    case "a render that is built says nothing of the sort" (fun () ->
        let root = F.create () in
        let seen = ref [] in
        ignore
          (F.render
             ~trace:(fun event -> seen := Trace.to_string Fun.id event :: !seen)
             root
             (room [ torch () ]));
        Alcotest.(check bool)
          "no refusal on a frame that stood" false (List.mem "refused" !seen));
    case "a frame asked for before a refusal is still asked for" (fun () ->
        let root = F.create () and latch = ref ignore in
        ignore (F.render root (lantern (latch, false)));
        Alcotest.(check bool) "settled" false (F.dirty root);
        !latch 1;
        (try ignore (F.render root (lantern (latch, true))) with Refused -> ());
        Alcotest.(check bool)
          "the setter's frame outlives the render that failed" true
          (F.dirty root));
    (* The other side of that promise, and the reason it is worded the way it
       is. A refusal rolls back the tree and the effects; it cannot roll back
       what the component did to a box it was handed, because the box is the
       same box every render and taking the write back would mean it was not. *)
    case "what a refused render wrote to a ref stays written" (fun () ->
        let root = F.create () in
        ignore (F.render root (tally false));
        Alcotest.check_raises "the second frame is refused" Refused (fun () ->
            ignore (F.render root (tally true)));
        Alcotest.check scene "three renders, and the ref counted all three"
          "runs=3"
          (F.render root (tally false)));
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
             { at = "swapper#0"; expected = "use_state"; found = "use_ref" })
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
             { at = "grower#0"; expected = "nothing"; found = "use_state" })
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
             { at = "shrinker#0"; expected = "use_ref"; found = "nothing" })
          (fun () -> ignore (run root (shrinker false))));
    case "a hook outside a render says so" (fun () ->
        Alcotest.check_raises "nothing is handling the effect"
          Hook.Hook_outside_render (fun () -> ignore (Hook.use_state 0)));
  ]

(* Two contexts of different types, to show that finding a binding also proves
   what type it holds. Neither of these needs a cast to read back. *)
let depth = Context.make 0
let theme = Context.make "plain"

let context =
  let reader =
    Element.declare ~name:"reader" @@ fun () ->
    Element.prim (string_of_int (Hook.use_context depth))
  in
  let both =
    Element.declare ~name:"both" @@ fun () ->
    Element.prim
      (Hook.use_context theme ^ "@" ^ string_of_int (Hook.use_context depth))
  in
  [
    case "unprovided is the default" (fun () ->
        Alcotest.check scene "nobody bound it" "0"
          (scene_of (R.create ()) (reader ())));
    case "a binding reaches the whole subtree" (fun () ->
        Alcotest.check scene "through a room it was not passed by" "room\n  4"
          (scene_of (R.create ())
             (Element.provide depth 4 [ room [ reader () ] ])));
    case "the nearest binding wins" (fun () ->
        Alcotest.check scene "the inner one shadows the outer" "1\n2"
          (scene_of (R.create ())
             (Element.provide depth 1
                [ reader (); Element.provide depth 2 [ reader () ] ])));
    case "a binding ends where its children do" (fun () ->
        Alcotest.check scene "the second reader is outside it" "5\n0"
          (scene_of (R.create ())
             (Element.fragment
                [ Element.provide depth 5 [ reader () ]; reader () ])));
    case "contexts of different types do not collide" (fun () ->
        Alcotest.check scene "each read back at its own type" "dusk@3"
          (scene_of (R.create ())
             (Element.provide theme "dusk"
                [ Element.provide depth 3 [ both () ] ])));
    case "providing builds nothing of its own" (fun () ->
        Alcotest.check scene "it flattens away like a fragment" "wall"
          (scene_of (R.create ()) (Element.provide depth 9 [ wall ])));
  ]

(* A store of the shape a game would actually keep. Two numbers rather than one,
   so that a selector can be swapped for another of the same type and the swap
   be the only thing that changed. *)
type game = { score : int; bonus : int; paused : bool }
type action = Scored of int | Bonus of int | Toggle_pause

let reducer state = function
  | Scored points -> { state with score = state.score + points }
  | Bonus points -> { state with bonus = state.bonus + points }
  | Toggle_pause -> { state with paused = not state.paused }

(* Declared once, as components must be: two of these built inside the cases
   below would be two different components to the reconciler. *)
let scoreboard =
  Element.declare ~name:"scoreboard" @@ fun (game : (game, action) Store.t) ->
  let score = Store.use_selector game (fun s -> s.score) in
  Element.prim ("score=" ^ string_of_int score)

let pause_light =
  Element.declare ~name:"pause_light" @@ fun (game : (game, action) Store.t) ->
  let paused = Store.use_selector game (fun s -> s.paused) in
  Element.prim (if paused then "paused" else "running")

(* A component that dispatches from its effect, which {!Store.dispatch} says is
   a thing an effect may do. Described before the scoreboard below, so its setup
   runs first — while the scoreboard has read the store but not yet subscribed
   to it. *)
let bell =
  Element.declare ~name:"bell" @@ fun (game : (game, action) Store.t) ->
  Hook.use_effect ~deps:game ~equal:( == ) (fun () ->
      Store.dispatch game (Scored 7);
      None);
  Element.prim "bell"

(* Its selector is its prop, so a frame can hand it a different one while the
   store stays the store it already subscribed to. *)
let dial =
  Element.declare ~name:"dial"
  @@ fun ((game : (game, action) Store.t), select) ->
  let n = Store.use_selector game select in
  Element.prim ("dial=" ^ string_of_int n)

(* A pair for the one moment a subscription could be leaked. The saboteur springs
   the tripwire from an effect, the way the bell above dispatches from one, and
   is described first so that its setup runs first; the brittle component's
   selector is then still fine for the render that reads it and raises on the
   comparison its own setup makes on the way in. *)
let saboteur =
  Element.declare ~name:"saboteur" @@ fun (tripwire : bool ref) ->
  Hook.use_effect ~deps:tripwire ~equal:( == ) (fun () ->
      tripwire := true;
      None);
  Element.prim "saboteur"

let brittle =
  Element.declare ~name:"brittle"
  @@ fun ((game : (game, action) Store.t), (tripwire : bool ref)) ->
  let score =
    Store.use_selector game (fun s -> if !tripwire then raise Exit else s.score)
  in
  Element.prim ("brittle=" ^ string_of_int score)

let fresh () =
  Store.create ~reducer ~initial:{ score = 0; bonus = 0; paused = false }

let store =
  [
    case "a selector reads its slice, and a dispatch moves it" (fun () ->
        let game = fresh () and root = R.create () in
        Alcotest.check scene "starts where the store did" "score=0"
          (scene_of root (scoreboard game));
        Store.dispatch game (Scored 10);
        Alcotest.check scene "and follows it" "score=10"
          (scene_of root (scoreboard game)));
    case "only the component whose slice moved asks for a frame" (fun () ->
        let game = fresh () in
        (* Two roots, because "dirty" is a question about a root. Each holds one
           component, and the two read different slices. *)
        let scores = R.create () and lights = R.create () in
        ignore (run scores (scoreboard game));
        ignore (run lights (pause_light game));
        Alcotest.(check bool) "both settled" false (R.dirty scores);
        Alcotest.(check bool) "both settled" false (R.dirty lights);
        Store.dispatch game (Scored 5);
        Alcotest.(check bool) "the scoreboard reads score" true (R.dirty scores);
        Alcotest.(check bool) "the light does not" false (R.dirty lights));
    case "an action that changes nothing it reads wakes nobody" (fun () ->
        let game = fresh () and root = R.create () in
        ignore (run root (scoreboard game));
        Store.dispatch game (Scored 0);
        Alcotest.(check bool) "the slice is where it was" false (R.dirty root);
        Store.dispatch game Toggle_pause;
        Alcotest.(check bool)
          "and this one never touched it" false (R.dirty root));
    case "a dispatch made while a selector is still subscribing is not lost"
      (fun () ->
        let game = fresh () and root = R.create () in
        (* Setups run one at a time, and the bell's is ahead of the
           scoreboard's. So this dispatch is made to a list the scoreboard is
           not on yet: the notification it would have woken on never comes. *)
        Alcotest.check scene "the frame shows what the render read"
          "room\n  bell\n  score=0"
          (scene_of root (room [ bell game; scoreboard game ]));
        Alcotest.(check int)
          "while the store has moved past it" 7 (Store.state game).score;
        Alcotest.(check bool) "so a frame is owed" true (R.dirty root);
        Alcotest.check scene "and it catches up" "room\n  bell\n  score=7"
          (scene_of root (room [ bell game; scoreboard game ]));
        Alcotest.(check bool) "settled" false (R.dirty root));
    case "a component handed another selector compares with the new one"
      (fun () ->
        let game = fresh () and root = R.create () in
        let score_of s = s.score and bonus_of s = s.bonus in
        Alcotest.check scene "reads the slice it was given" "dial=0"
          (scene_of root (dial (game, score_of)));
        Alcotest.check scene "and then the other one" "dial=0"
          (scene_of root (dial (game, bonus_of)));
        Alcotest.(check int)
          "the same store, so still the one subscription" 1
          (Store.subscriber_count game);
        Store.dispatch game (Scored 5);
        Alcotest.(check bool)
          "the slice it has stopped reading cannot wake it" false (R.dirty root);
        Store.dispatch game (Bonus 3);
        Alcotest.(check bool) "the one it now reads can" true (R.dirty root);
        Alcotest.check scene "and the frame shows it" "dial=3"
          (scene_of root (dial (game, bonus_of))));
    case "a comparison that raises on the way in leaves no subscription"
      (fun () ->
        let game = fresh () and root = R.create () in
        let tripwire = ref false in
        Alcotest.check_raises "the raise comes back out of the render" Exit
          (fun () ->
            ignore
              (run root (room [ saboteur tripwire; brittle (game, tripwire) ])));
        Alcotest.(check int)
          "and the store is listening to nobody" 0
          (Store.subscriber_count game));
    case "the tree does not leak subscriptions" (fun () ->
        let game = fresh () and root = R.create () in
        Alcotest.(check int) "nothing yet" 0 (Store.subscriber_count game);
        ignore (run root (room [ scoreboard game; scoreboard game ]));
        Alcotest.(check int) "one each" 2 (Store.subscriber_count game);
        ignore (run root (room [ scoreboard game ]));
        Alcotest.(check int)
          "the one that went, went" 1
          (Store.subscriber_count game);
        ignore (run root Element.empty);
        Alcotest.(check int)
          "and the tree is clean behind it" 0
          (Store.subscriber_count game));
    case "a component handed another store lets go of the first" (fun () ->
        let first = fresh () and second = fresh () and root = R.create () in
        ignore (run root (scoreboard first));
        Alcotest.(check int)
          "subscribed to what it was rendered with" 1
          (Store.subscriber_count first);
        Store.dispatch second (Scored 3);
        Alcotest.check scene "and it reads whichever it is handed" "score=3"
          (scene_of root (scoreboard second));
        Alcotest.(check int)
          "the first is let go of" 0
          (Store.subscriber_count first);
        Alcotest.(check int)
          "and the second taken out" 1
          (Store.subscriber_count second);
        Alcotest.(check bool) "settled after the swap" false (R.dirty root);
        Store.dispatch first (Scored 100);
        Alcotest.(check bool)
          "the store it left cannot wake it" false (R.dirty root);
        Store.dispatch second (Scored 1);
        Alcotest.(check bool) "the one it moved to can" true (R.dirty root));
    case "nor does a root that is thrown away whole" (fun () ->
        (* Which is what a run does with its mount when the window closes, and
           what a one-shot render does with the root it made for itself. *)
        let game = fresh () and root = R.create () in
        ignore (run root (room [ scoreboard game; pause_light game ]));
        Alcotest.(check int) "two readers" 2 (Store.subscriber_count game);
        R.destroy root;
        Alcotest.(check int)
          "and nothing left listening" 0
          (Store.subscriber_count game));
    (let reducer_calls = ref 0 in
     let counted state action =
       incr reducer_calls;
       reducer state action
     in
     case "the reducer is the only thing that writes" (fun () ->
         let game =
           Store.create ~reducer:counted
             ~initial:{ score = 1; bonus = 0; paused = false }
         in
         reducer_calls := 0;
         Store.dispatch game (Scored 2);
         Store.dispatch game (Scored 3);
         Alcotest.(check int) "once per dispatch" 2 !reducer_calls;
         Alcotest.(check int)
           "and the arithmetic is the reducer's" 6 (Store.state game).score));
  ]

(* {1 Debug paths tell places apart}

   to_debug_string exists so that a trace, a Hook_order_changed and a
   Duplicate_key can each name one place and mean one place. That is a promise
   about every pair of paths, and it was false for the commonest pair there is:
   a named step printed as its bare name, so [torch (); torch ()] — two of a
   thing, written the ordinary way — printed alike. A trace saying [mount torch]
   twice says nothing about which. *)
let a_debug_path_names_one_place () =
  let named i = Path.child ~name:"torch" Path.root i
  and bare i = Path.child Path.root i
  and keyed k = Path.child ~name:"torch" ~key:k Path.root 0 in
  let differs what a b =
    Alcotest.(check bool)
      what true
      (not (String.equal (Path.to_debug_string a) (Path.to_debug_string b)))
  in
  differs "two named unkeyed siblings" (named 0) (named 1);
  differs "two unnamed unkeyed siblings" (bare 0) (bare 1);
  differs "two keyed siblings" (keyed "north") (keyed "south");
  differs "a named step and an unnamed one at the same index" (named 0) (bare 0);
  differs "a keyed step and an unkeyed one" (keyed "north") (named 0);
  (* Nested, so the separator is doing its job as well as the steps. *)
  differs "siblings one level down"
    (Path.child ~name:"flame" (named 0) 0)
    (Path.child ~name:"flame" (named 1) 0);
  (* And the shapes, which are what a reader actually has to recognise. *)
  Alcotest.(check string)
    "a named unkeyed step carries its index" "torch#1"
    (Path.to_debug_string (named 1));
  Alcotest.(check string)
    "a keyed one needs none, being unique among its siblings" "torch[north]"
    (Path.to_debug_string (keyed "north"));
  Alcotest.(check string)
    "root is still root" "(root)"
    (Path.to_debug_string Path.root)

(* The same claim over generated paths rather than chosen ones: any two paths
   that Path.equal calls different print differently. *)
let debug_paths_are_injective =
  let step =
    QCheck2.Gen.(
      triple (int_bound 3)
        (option (oneof_list [ "a"; "b" ]))
        (option (oneof_list [ "torch"; "lamp" ])))
  in
  let path =
    QCheck2.Gen.map
      (List.fold_left
         (fun acc (index, key, name) -> Path.child ?key ?name acc index)
         Path.root)
      QCheck2.Gen.(list_size (int_bound 4) step)
  in
  QCheck2.Test.make ~count:2000 ~name:"different places print differently"
    (QCheck2.Gen.pair path path) (fun (a, b) ->
      Path.equal a b
      || not (String.equal (Path.to_debug_string a) (Path.to_debug_string b)))

let () =
  Alcotest.run "Loom"
    [
      ("mounting", mounting);
      ("context", context);
      ("store", store);
      ("keeping", keeping);
      ("keys", keys);
      ("state", state);
      ("memo", memo);
      ("effects", effects);
      ("destroying", destroying);
      ("refusing", refusing);
      ("hook order", hook_order);
      ( "paths",
        [ case "a debug path names one place" a_debug_path_names_one_place ] );
      ( "properties",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick history_does_not_show;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            debug_paths_are_injective;
        ] );
    ]
