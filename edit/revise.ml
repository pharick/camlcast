(* Implementation of {!Camlcast_edit.Revise}; the interface carries the prose. *)

type change = { path : string; before : string option }

type t = {
  read : string -> (string, [ `Msg of string ]) result;
  write : string -> string -> (unit, [ `Msg of string ]) result;
  mutable history : change list;
}

let create ~read ~write = { read; write; history = [] }

let from_disk () path =
  match In_channel.with_open_bin path In_channel.input_all with
  | source -> Ok source
  | exception Sys_error message -> Error (`Msg message)

(* Beside the target rather than in a temporary directory, because a rename is
   atomic only within one file system and beside it is the only place certain
   to be on the same one. *)
let to_disk () path text =
  let temporary = path ^ ".camlcast-writing" in
  match
    Out_channel.with_open_bin temporary (fun out ->
        Out_channel.output_string out text)
  with
  | () -> (
      match Sys.rename temporary path with
      | () -> Ok ()
      | exception Sys_error message ->
          (* The temporary is the only thing that can be left behind, and
             leaving it would be litter that looks like a partial write. *)
          (try Sys.remove temporary with Sys_error _ -> ());
          Error (`Msg message))
  | exception Sys_error message -> Error (`Msg message)

let ( let* ) = Result.bind

(* The cheapest check that an edit did what it claimed. A file that no longer
   parses is one an edit has damaged, and the only useful answer is to have
   written nothing. *)
let parses ~path text =
  match Span.parse ~path text with
  | Ok _ -> Ok ()
  | Error (`Msg message) ->
      Error
        (`Msg
           (path ^ ": the edit would leave text that does not parse: " ^ message))

let record t ~path ~before = t.history <- { path; before } :: t.history

let write t ~path text =
  let* () = parses ~path text in
  let before = Result.to_option (t.read path) in
  let* () = t.write path text in
  record t ~path ~before;
  Ok ()

let apply t ~path edits =
  (* Read afresh: what is on disk now is what is about to be written over. *)
  let* source = t.read path in
  let* parsed = Span.parse ~path source in
  let* written = Span.splice parsed edits in
  let* () = parses ~path written in
  let* () = t.write path written in
  record t ~path ~before:(Some source);
  Ok ()

let depth t = List.length t.history

type undone = Restored of string | Created of string

let undo t =
  match t.history with
  | [] -> Ok None
  | { path; before } :: rest -> (
      match before with
      | Some text ->
          let* () = t.write path text in
          t.history <- rest;
          Ok (Some (Restored path))
      | None ->
          (* Nothing to put back, and removing the file is a decision this
             module does not get to make. *)
          t.history <- rest;
          Ok (Some (Created path)))
