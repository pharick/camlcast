(* Implementation of {!Camlcast_edit.Field}; the interface carries the prose. *)

type value = Number of float | Name of string
type t = { label : string; span : Span.span; value : value }

(* A labelled argument is called what it was called; a positional one is
   numbered, there being nothing in the source that names it. *)
let stem index (argument : Span.argument) =
  match argument.label with
  | Some label -> label
  | None -> "#" ^ string_of_int index

(* One number needs no suffix, several need telling apart. A point written
   [Vec.make 1. 2.] is one argument holding two. *)
let numbered stem count position =
  if count = 1 then stem else Printf.sprintf "%s.%d" stem position

(* A number as it is written, brackets and all: the span of a negative literal
   covers the brackets it needs to sit where it sits, which is the same fact
   Span.number exists for at the other end. *)
let number_written written =
  let trimmed = String.trim written in
  let bare =
    let length = String.length trimmed in
    if length >= 2 && trimmed.[0] = '(' && trimmed.[length - 1] = ')' then
      String.trim (String.sub trimmed 1 (length - 2))
    else trimmed
  in
  Option.value (float_of_string_opt bare) ~default:Float.nan

let of_call parsed call =
  List.concat
    (List.mapi
       (fun index (argument : Span.argument) ->
         let stem = stem index argument in
         match argument.value with
         | Span.Computed -> []
         | Span.Name span ->
             [ { label = stem; span; value = Name (Span.slice parsed span) } ]
         | Span.Numbers spans ->
             let count = List.length spans in
             List.mapi
               (fun position span ->
                 {
                   label = numbered stem count position;
                   span;
                   value = Number (number_written (Span.slice parsed span));
                 })
               spans)
       call.Span.arguments)

let write field value =
  match (field.value, value) with
  | Number _, Number number -> Ok (field.span, Span.number number)
  | Name _, Name name ->
      if String.trim name = "" then Error (`Msg "a name cannot be empty")
      else Ok (field.span, name)
  | Number _, Name _ ->
      Error (`Msg (field.label ^ " holds a number; a name cannot go there"))
  | Name _, Number _ ->
      Error (`Msg (field.label ^ " holds a name; a number cannot go there"))
