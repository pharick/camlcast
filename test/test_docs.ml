(* The pages a reader lands on, checked against the tree they describe.

   examples/ is what keeps the guide honest: each step's program is compiled by
   the default build, so a snippet doc/making-a-game.mld quotes cannot outlive
   the API it was written for. A sample written as prose inside an interface has
   no such anchor. odoc does not typecheck a {[ ]} block, so such a sample can
   go on naming an argument or a constructor the layer no longer has — on the
   page a reader meets first, and with nothing anywhere to say so.

   This is that anchor, extended to reach one. Two questions:

   - every line of the sample has to be a line of a program the build compiles.
     Trimmed, so the sample may be indented to suit the page it sits on, and
     line by line, so it may quote an excerpt rather than a whole file. What it
     may not do is invent a line.

   - every [P.name] a page mentions has to be a name P exports.
     tools/odoc-refs.py already covers the ones written as {!P.name}, which
     odoc resolves; this covers the same name in a code span, in a sample, or
     in a .md file, none of which odoc reads as a reference at all.

   What neither reaches is a bare code span in prose: a sentence listing the
   constructors can still name one that is gone. The words a page may put in
   brackets are not a set anything here can know, so those lists stay the
   reader's. *)

(* Support is absent for the reason it is absent from test_loom: it is built on
   the engine's types, and a suite that reads text has no use for a Vec
   testable. So [case] is here rather than opened, and this suite links nothing
   but the framework it reports through. *)
let case name body = Alcotest.test_case name `Quick body
let read path = In_channel.with_open_bin path In_channel.input_all
let lines text = String.split_on_char '\n' text

(* Where the pages are, from test/, which is where dune runs a suite. Each is a
   dependency of this stanza; see test/dune. *)
let front_page = "../lib/camlcast.mli"
let vocabulary = "../lib/p.mli"
let pages = [ front_page; "../doc/index.mld"; "../doc/making-a-game.mld" ]

let examples () =
  List.filter_map
    (fun name ->
      if Filename.check_suffix name ".ml" then
        Some (Filename.concat "../examples" name)
      else None)
    (Array.to_list (Sys.readdir "../examples"))

let is_ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

(* Odoc disambiguates a name used twice by prefixing its namespace, so
   {!P.val-corner} and {!P.type-corner} are the function and the type. The
   prefix is not part of the name. *)
let namespaces = [ "val"; "type"; "module"; "exception" ]

(* Every [P.name] a page mentions. The name has to start lowercase, since
   [P.Something] is a module or a constructor and neither is in this
   vocabulary; and the character before the [P] has to be one an identifier
   cannot hold, so that a word ending in P is not read as a mention. *)
let p_names text =
  let n = String.length text in
  let lower = function 'a' .. 'z' -> true | _ -> false in
  let opens i =
    i + 2 < n && text.[i] = 'P' && text.[i + 1] = '.' && lower text.[i + 2]
  in
  let standalone i = i = 0 || not (is_ident text.[i - 1]) in
  let rec name i =
    let stop = ref i in
    while !stop < n && is_ident text.[!stop] do
      incr stop
    done;
    let word = String.sub text i (!stop - i) in
    if !stop < n && text.[!stop] = '-' && List.mem word namespaces then
      name (!stop + 1)
    else (word, !stop)
  in
  let rec scan i found =
    if i >= n then List.rev found
    else if opens i && standalone i then
      let word, next = name (i + 2) in
      scan next (word :: found)
    else scan (i + 1) found
  in
  scan 0 []

(* A val or a type declared at the start of a line. Read as text rather than
   reached through the module, because what is being checked is what a page
   says, and a page is text. *)
let declared keyword line =
  if String.starts_with ~prefix:keyword line then
    let n = String.length keyword in
    let rest = String.sub line n (String.length line - n) in
    List.nth_opt (String.split_on_char ' ' rest) 0
  else None

let exports () =
  List.filter_map
    (fun line ->
      match declared "val " line with
      | Some _ as found -> found
      | None -> declared "type " line)
    (lines (read vocabulary))

(* The lines inside every {[ ]} block, trimmed, in the order they are written.
   Both delimiters sit on lines of their own, which is how a page with a sample
   in it writes them. *)
let sampled text =
  let rec go inside found = function
    | [] -> List.rev found
    | line :: rest -> (
        match String.trim line with
        | "{[" -> go true found rest
        | "]}" -> go false found rest
        | bare when inside -> go inside (bare :: found) rest
        | _ -> go inside found rest)
  in
  go false [] (lines text)

let the_front_page_sample_is_a_program_that_compiles () =
  let written =
    List.concat_map
      (fun path -> List.map String.trim (lines (read path)))
      (examples ())
  in
  let block = sampled (read front_page) in
  Alcotest.(check bool)
    "the front page still has a sample in it" true (block <> []);
  List.iter
    (fun line ->
      if line <> "" then
        Alcotest.(check bool)
          (Printf.sprintf "an example program carries the line %S" line)
          true (List.mem line written))
    block

let the_pages_name_only_what_p_exports () =
  let exported = exports () in
  List.iter
    (fun page ->
      List.iter
        (fun name ->
          Alcotest.(check bool)
            (Printf.sprintf "%s mentions P.%s, which P exports" page name)
            true (List.mem name exported))
        (p_names (read page)))
    pages

let () =
  Alcotest.run "Docs"
    [
      ( "the front page",
        [
          case "its sample is a program that compiles"
            the_front_page_sample_is_a_program_that_compiles;
        ] );
      ( "the vocabulary",
        [
          case "the pages name only what P exports"
            the_pages_name_only_what_p_exports;
        ] );
    ]
