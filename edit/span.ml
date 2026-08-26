(* Implementation of {!Camlcast_edit.Span}; the interface carries the prose. *)

type span = { start : int; stop : int }
type value = Numbers of span list | Name of span | Computed
type argument = { label : string option; span : span; value : value }
type call = { callee : string; span : span; arguments : argument list }
type t = { source : string; calls : ((int * int) * call) list }

let text t = t.source

let span_of (loc : Location.t) =
  { start = loc.loc_start.pos_cnum; stop = loc.loc_end.pos_cnum }

(* Where a location begins, in the terms a position from ppx_camlcast is
   written in: a line, and a column within it. Location counts bytes from the
   start of the file and records where the line began, so the column is the
   difference. *)
let anchor_of (loc : Location.t) =
  (loc.loc_start.pos_lnum, loc.loc_start.pos_cnum - loc.loc_start.pos_bol)

(* Read out of the source rather than rebuilt from the parse tree: it is the
   text that was written, and reading it costs no knowledge of how a path is
   represented -- which has changed shape more than once. *)
let slice source { start; stop } = String.sub source start (stop - start)

(* The one classification, and the whole of what makes an argument editable.

   Written as a walk that names three constructors rather than as a match over
   every shape an argument can have. That is deliberate: Parsetree grows, and a
   classifier that listed tuples, records and constructors would stop compiling
   each time one of them changed. What it needs to know is only whether the
   expression is built from numbers alone, and that can be asked of any shape.

   An identifier disqualifies -- unless it is the head of an application, which
   is how [Vec.make (-3.) (-4.)] is a pair of coordinates written out while
   [plaza_corner k] is not. The difference is entirely in the arguments, and
   that is the difference between a value that is the same every frame and one
   that need not be. *)
let classify (expression : Parsetree.expression) =
  match expression.pexp_desc with
  | Pexp_ident _ -> Name (span_of expression.pexp_loc)
  | _ ->
      let numbers = ref [] and computed = ref false in
      let iterator =
        {
          Ast_iterator.default_iterator with
          expr =
            (fun self (inner : Parsetree.expression) ->
              match inner.pexp_desc with
              | Pexp_constant
                  { pconst_desc = Pconst_float _ | Pconst_integer _; _ } ->
                  numbers := span_of inner.pexp_loc :: !numbers
              | Pexp_ident _ -> computed := true
              | Pexp_apply ({ pexp_desc = Pexp_ident _; _ }, arguments) ->
                  List.iter
                    (fun (_, argument) -> self.expr self argument)
                    arguments
              | _ -> Ast_iterator.default_iterator.expr self inner);
        }
      in
      iterator.expr iterator expression;
      if !computed || !numbers = [] then Computed
      else Numbers (List.rev !numbers)

let argument_of (label, expression) =
  {
    label =
      (match (label : Asttypes.arg_label) with
      | Nolabel -> None
      | Labelled name | Optional name -> Some name);
    span = span_of expression.Parsetree.pexp_loc;
    value = classify expression;
  }

(* Every application in the file whose head is a plain name, which is every
   shape a description constructor is called in. Collected once, at parse, so
   that looking one up is a search of a list rather than a walk of a tree. *)
let collect source structure =
  let found = ref [] in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun self expression ->
          (match expression.Parsetree.pexp_desc with
          | Pexp_apply (({ pexp_desc = Pexp_ident _; _ } as head), arguments) ->
              found :=
                ( anchor_of expression.pexp_loc,
                  {
                    callee = slice source (span_of head.pexp_loc);
                    span = span_of expression.pexp_loc;
                    arguments = List.map argument_of arguments;
                  } )
                :: !found
          | _ -> ());
          Ast_iterator.default_iterator.expr self expression);
    }
  in
  iterator.structure iterator structure;
  List.rev !found

let parse ~path source =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf path;
  match Parse.implementation lexbuf with
  | structure -> Ok { source; calls = collect source structure }
  (* A syntax error is a condition and not a mistake in this program: the file
     is the game developer's, and may be halfway through being typed. It is
     reported through the compiler's own printer, so it reads the way the
     compiler's would and names the same place. *)
  | exception raised -> (
      match Location.error_of_exn raised with
      | Some (`Ok report) ->
          Error
            (`Msg
               (String.trim (Format.asprintf "%a" Location.print_report report)))
      | Some `Already_displayed | None -> raise raised)

(* Applications can begin where another begins, and the outermost is the one a
   position names: an anchor is where an expression starts, and a description
   constructor's arguments start after it. [collect] reports outermost first,
   so the first match is the right one. *)
let calls t = List.map snd t.calls

let at t ~line ~column =
  Option.map snd
    (List.find_opt (fun (anchor, _) -> anchor = (line, column)) t.calls)

let slice t span = slice t.source span

let number value =
  let written = string_of_float value in
  if Float.compare value 0. < 0 then "(" ^ written ^ ")" else written

let splice t edits =
  let ordered =
    List.sort (fun (a, _) (b, _) -> Int.compare a.start b.start) edits
  in
  let rec overlapping = function
    | (a, _) :: ((b, _) :: _ as rest) ->
        if a.stop > b.start then Some (a, b) else overlapping rest
    | _ -> None
  in
  match overlapping ordered with
  | Some (a, b) ->
      Error
        (`Msg
           (Printf.sprintf "two edits overlap: %d..%d and %d..%d" a.start a.stop
              b.start b.stop))
  | None ->
      let buffer = Buffer.create (String.length t.source) in
      let read =
        List.fold_left
          (fun read (span, replacement) ->
            Buffer.add_string buffer
              (String.sub t.source read (span.start - read));
            Buffer.add_string buffer replacement;
            span.stop)
          0 ordered
      in
      Buffer.add_string buffer
        (String.sub t.source read (String.length t.source - read));
      Ok (Buffer.contents buffer)
