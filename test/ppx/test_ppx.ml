(** What the rewriter puts on a description, and what it leaves alone.

    This suite is preprocessed by [ppx_camlcast] — see test/dune — so the
    constructors written below carry positions and the assertions can read them
    back off the elements themselves. That is the whole of the integration:
    everything else in this repository is compiled without the rewriter and is
    unaffected by it.

    {1 The list this holds honest}

    The rewriter runs before types exist, so the only thing it can tell a wall
    from a corner by is the constructor's name, and it therefore carries those
    names written out. A name added to {!Camlcast.P} and not to that list would
    silently keep its position, and the element it describes would silently stop
    being editable — no error, no warning, and nothing on screen.

    So rather than check the list against a guess at each signature's return
    type, this asks something stricter and simpler: every [val] P exports is in
    exactly one of three lists, two of them the rewriter's own and the third
    below. A constructor added to P belongs to none of them and fails here until
    somebody says which it is. *)

let case name body = Alcotest.test_case name `Quick body
let read path = In_channel.with_open_bin path In_channel.input_all
let lines text = String.split_on_char '\n' text

(* From test/ppx/, which is where dune runs this suite. A dependency of its
   stanza; see test/ppx/dune. *)
let vocabulary = "../../lib/p.mli"

let is_ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let declared line =
  let prefix = "val " in
  if String.starts_with ~prefix line then
    let rest = String.sub line 4 (String.length line - 4) in
    let stop =
      match String.index_opt rest ' ' with
      | Some i -> i
      | None -> String.length rest
    in
    let name = String.sub rest 0 stop in
    if name <> "" && String.for_all is_ident name then Some name else None
  else None

let exports () = List.filter_map declared (lines (read vocabulary))

(* What P exports that is not an element: a surface, a ceiling, a corner, a
   door, a pair of points, a plane. Wrapping one of these would not typecheck,
   which is the cheerful half of this hazard — the other half is a constructor
   that would typecheck and is simply forgotten. *)
let not_elements =
  [
    "floor";
    "roof";
    "open_sky";
    "corner";
    "corners";
    "door";
    "polygon";
    "opening";
    "through";
  ]

let sorted names = List.sort_uniq String.compare names

(* One material, because a wall needs one and nothing here looks at it. *)
let stone =
  Camlcast.Material.make
    ~pattern:
      (Camlcast.Texture.generate (fun ~u:_ ~v:_ -> Camlcast.Color.rgb 1 1 1))

let position element =
  match element with
  | Camlcast.Element.Prim { at; _ } -> at
  | Fragment { at; _ } | Component { at; _ } -> at
  | Empty | Provide _ -> None

(* The three shapes the rewriter has to recognise: a bare name under the open,
   a qualified one outside it, and a constructor that is a value rather than a
   call. Nothing here records the line it is on -- the assertions read the file
   back and check the anchor points at the constructor, which is the property
   worth having and the only one that survives being reformatted. *)
let bare =
  Camlcast.P.(
    wall ~height:2. ~material:stone (Camlcast.Vec.make 0. 0.)
      (Camlcast.Vec.make 1. 0.))

let qualified = Camlcast.P.spawn (Camlcast.Vec.make 0. 0.)
let constant = Camlcast.P.cursor

(* A local binding whose name is one P also uses, outside any [P.( ... )]. The
   rewriter must not touch it: it is not a description, and the fact that this
   file compiles at all is the assertion. A wrapped one would be handed to
   Element.at and refused by the typechecker. *)
let text = "not a description"

(* A constructor stopped before its unlabelled argument. That is how a game
   writes a helper -- demo/barred.ml binds [P.cut ~leaf ~lintel] and hands over
   the door and the wall at each place it uses it -- and it is not a
   description: it is a function. Wrapping it asked Element.at to take one, and
   the error landed on the game's own line saying nothing about the rewriter.

   As with [text] above, the fact that this file compiles is the assertion. *)
let partial = Camlcast.P.cut ~key:"way"

(* And what the helper builds. The call that finishes it names [partial], not
   [P.cut], so nothing here is a constructor the rewriter knows and the element
   comes out carrying no position -- which is the same answer camlcast.edit
   gives about a wall built by a helper, and for the same reason. *)
let through_a_helper =
  partial
    (Camlcast.P.door ~name:"way" ~width:1. ~clearance:2. ())
    ~along:(Camlcast.Vec.make 0. 0., Camlcast.Vec.make 2. 0.)

(* Whether this file was preprocessed by a rewriter that was awake, read off a
   description rather than asked of the rewriter. Nothing here is an extension
   node: an [%ext] would make this file fail to typecheck without the rewriter,
   and so make [dune build @check] -- what merlin and the language server build
   -- fail for the whole repository. *)
let positioned = position bare <> None

(* And whether it should have been, from dune -- see the action in test/dune.
   Two sources on purpose: a suite that let the rewriter answer both would pass
   whether or not the gate held, since a CAMLCAST_POSITIONS reaching a release
   build would move the answer and the positions together. *)
let expected =
  match Sys.getenv_opt "CAMLCAST_PROFILE" with
  | Some "release" -> false
  | Some _ | None -> true

(* [pos_fname] is the path dune handed the compiler; the suite runs beside its
   own source in _build, so the basename is what opens. *)
let anchors expect element =
  match (positioned, position element) with
  | false, None -> ()
  | false, Some (file, line, _, _) ->
      Alcotest.failf
        "%s carries a position (%s:%d) in a build that should have none" expect
        file line
  | true, None ->
      Alcotest.failf "%s carries no position in a build that should have one"
        expect
  | true, Some (file, line, column, _) ->
      Alcotest.(check bool)
        (expect ^ ": a file this suite can find")
        true
        (Filename.check_suffix file "test_ppx.ml");
      let source = lines (read (Filename.basename file)) in
      let text =
        match List.nth_opt source (line - 1) with
        | Some text -> text
        | None ->
            Alcotest.failf "%s: line %d is past the end of %s" expect line file
      in
      let reach = String.length text - column in
      Alcotest.(check bool)
        (Printf.sprintf "%s: line %d column %d is where it was written" expect
           line column)
        true
        (reach >= String.length expect
        && String.sub text column (String.length expect) = expect)

let () =
  Alcotest.run "Ppx"
    [
      ( "what it wraps",
        [
          case "a bare constructor under the open" (fun () ->
              anchors "wall" bare);
          case "a qualified constructor" (fun () ->
              anchors "Camlcast.P.spawn" qualified);
          case "a constructor that is a value, not a call" (fun () ->
              anchors "Camlcast.P.cursor" constant);
          case "the profile decides whether there are positions at all"
            (fun () ->
              Alcotest.(check bool)
                "dune's profile against what the rewriter did" expected
                positioned);
          case "what it leaves alone still typechecks" (fun () ->
              Alcotest.(check string)
                "a local named for a constructor" "not a description" text);
          case "a constructor short of its unlabelled argument is a function"
            (fun () ->
              (* Vacuously true in a build without positions, where nothing
                 carries one; the case that matters is the other profile. *)
              Alcotest.(check bool)
                "what a helper builds carries no position" true
                (position through_a_helper = None));
        ] );
      ( "the vocabulary",
        [
          case "every name P exports is classified" (fun () ->
              Alcotest.(check (list string))
                "P's exports against the rewriter's two lists and this suite's"
                (sorted (exports ()))
                (sorted
                   (Camlcast_vocabulary.functions @ Camlcast_vocabulary.values
                  @ not_elements)));
          case "the three lists do not overlap" (fun () ->
              let all =
                Camlcast_vocabulary.functions @ Camlcast_vocabulary.values
                @ not_elements
              in
              Alcotest.(check int)
                "no name is in two of them" (List.length all)
                (List.length (sorted all)));
        ] );
    ]
