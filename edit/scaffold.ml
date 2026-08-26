(* Implementation of {!Camlcast_edit.Scaffold}; the interface carries the
   prose. *)

type file = { path : string; source : string }

let legal name =
  name <> ""
  && (match name.[0] with 'a' .. 'z' -> true | _ -> false)
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
       name

let check name =
  if not (legal name) then
    invalid_arg
      (Printf.sprintf
         "Scaffold: %S cannot be a module name; use lowercase letters, digits \
          and underscores, starting with a letter"
         name)

(* The partial application, and nothing between [declare] and the function it
   is given. Both halves of the rule the interface states are here, and this is
   the only place they have to be got right. *)
let component ~name ~body =
  check name;
  {
    path = name ^ ".ml";
    source =
      Printf.sprintf
        "open Camlcast\n\n\
         let %s =\n\
        \  Element.declare ~name:%S @@ fun () ->\n\
        \  %s\n"
        name name body;
  }

let extract parsed ~name span =
  check name;
  if span.Span.stop <= span.Span.start then
    Error (`Msg "there is nothing selected to move")
  else
    let taken = Span.slice parsed span in
    if String.trim taken = "" then Error (`Msg "the selection is blank")
    else Ok (component ~name ~body:(String.trim taken), (span, name ^ " ()"))

let extract_call parsed (call : Span.call) ~name =
  extract parsed ~name call.Span.span
