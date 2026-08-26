(* Implementation of {!Camlcast_edit.Link}; the interface carries the prose. *)

type source = { file : string; line : int; call : Span.call }
type found = Unpositioned | Unreadable of string | Found of source

(* One entry per file asked about, whether or not it could be used: a file that
   does not parse should be reported once and not re-read every frame, an
   overlay asking about the node under the crosshair sixty times a second. *)
type t = {
  read : string -> (string, [ `Msg of string ]) result;
  mutable files : (string, (Span.t, string) result) Hashtbl.t;
}

let create read = { read; files = Hashtbl.create 8 }

let parsed t file =
  match Hashtbl.find_opt t.files file with
  | Some already -> already
  | None ->
      let answer =
        match t.read file with
        | Error (`Msg message) -> Error message
        | Ok source -> (
            match Span.parse ~path:file source with
            | Ok parsed -> Ok parsed
            | Error (`Msg message) -> Error message)
      in
      Hashtbl.add t.files file answer;
      answer

let locate t = function
  | None -> Unpositioned
  | Some (file, line, column, _) -> (
      match parsed t file with
      | Error message -> Unreadable message
      | Ok parsed -> (
          match Span.at parsed ~line ~column with
          | Some call -> Found { file; line; call }
          | None ->
              Unreadable
                (Printf.sprintf "%s:%d: nothing is written at column %d" file
                   line column)))

let find t (node : Camlcast.Prim.t Camlcast_loom.Host.node) =
  locate t node.Camlcast_loom.Host.at

let editable source =
  List.exists
    (fun (argument : Span.argument) ->
      match argument.value with
      | Span.Numbers _ -> true
      | Span.Name _ | Span.Computed -> false)
    source.call.arguments
