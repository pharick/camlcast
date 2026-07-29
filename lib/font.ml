(* Implementation of {!Camlcast.Font}; the interface carries the prose. *)

type t = {
  atlas : Image.t;
  width : int;
  height : int;
  columns : int;
  first : int;
  fallback : char option;
}

let make ?fallback ~atlas ~width ~height ~first () =
  if width <= 0 || height <= 0 then
    invalid_arg "Font.make: a cell must have a positive size";
  if width > atlas.Image.width || height > atlas.Image.height then
    invalid_arg "Font.make: a cell must fit in the atlas";
  (* No [Int.max 1] here: the check above is what makes the division at least
     one, and so what keeps [cell]'s [mod columns] from dividing by zero. *)
  { atlas; width; height; columns = atlas.Image.width / width; first; fallback }

let capacity t = t.columns * (t.atlas.Image.height / t.height)

let cell t c =
  let n = Char.code c - t.first in
  if n < 0 || n >= capacity t then None
  else Some (n mod t.columns * t.width, n / t.columns * t.height)

let glyph t c =
  match cell t c with
  | Some _ as found -> found
  | None -> Option.bind t.fallback (cell t)

(* The lines of [text], split on newlines. What {!measure}, {!wrap} and {!draw}
   all agree a line is. *)
let lines text = String.split_on_char '\n' text

let measure t text =
  let ls = lines text in
  let longest =
    List.fold_left (fun acc l -> Int.max acc (String.length l)) 0 ls
  in
  (longest * t.width, List.length ls * t.height)

let wrap t text ~width =
  let per_line = Int.max 1 (width / t.width) in
  let rec split_word word acc =
    if String.length word <= per_line then (word, acc)
    else
      split_word
        (String.sub word per_line (String.length word - per_line))
        (String.sub word 0 per_line :: acc)
  in
  let wrap_line line =
    let words = String.split_on_char ' ' line in
    let flush current out = match current with "" -> out | c -> c :: out in
    let current, out =
      List.fold_left
        (fun (current, out) word ->
          (* A word too long for any line is cut into full lines first; what is
             left of it then joins the flow like any other word. Cutting it
             flushes what was accumulated, so [current] is emptied with it —
             left as it was, the line just flushed would be written out a
             second time below. *)
          let word, current, out =
            if String.length word <= per_line then (word, current, out)
            else
              let rest, out = split_word word (flush current out) in
              (rest, "", out)
          in
          if current = "" then (word, out)
          else if String.length current + 1 + String.length word <= per_line
          then (current ^ " " ^ word, out)
          else (word, current :: out))
        ("", []) words
    in
    (* A line with nothing on it wraps to one empty line and not to none: the
       break was in the text, and dropping it here would leave {!measure} and
       {!draw} counting a line this does not return. *)
    match List.rev (flush current out) with
    | [] -> [ "" ]
    | wrapped -> wrapped
  in
  List.concat_map wrap_line (lines text)

let draw fb t text ~x ~y ~color =
  List.iteri
    (fun row line ->
      let y = y + (row * t.height) in
      String.iteri
        (fun col c ->
          match glyph t c with
          | None -> ()
          | Some (sx, sy) ->
              Paint.sub ~tint:color fb t.atlas
                ~x:(x + (col * t.width))
                ~y ~sx ~sy ~sw:t.width ~sh:t.height)
        line)
    (lines text)
