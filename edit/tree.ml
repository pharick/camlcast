(* Implementation of {!Camlcast_edit.Tree}; the interface carries the prose. *)

open Camlcast_loom

type row = {
  path : Path.t;
  label : string;
  depth : int;
  component : bool;
  remounts : int;
}

(* Keyed by Path.to_debug_string rather than by the path itself: Path is
   abstract and offers equality alone, and a table wants a key it can hash.
   That spelling is the one built to tell places apart -- Path says two
   different places never print the same way there, which Path.to_string
   cannot promise, dropping as it does the steps that distinguish siblings. *)
module Places = Map.Make (String)

type place = { row : row; order : int }

type t = {
  (* What the frame being reported has said so far. Held rather than shown,
     because a refusal can still take it away. *)
  mutable pending : place Places.t;
  mutable seen : int;
  (* The last frame that stood. *)
  mutable standing : place Places.t;
  (* Which places existed in the frame before this one, so that a mount can be
     told from a remount. Kept across a refusal, the frame before still being
     the frame before. *)
  mutable previous : unit Places.t;
  (* Counted per place and across frames, so a component remounting every
     frame reads as a number that climbs rather than a flag that is always on. *)
  mutable remounts : int Places.t;
  (* Set by a refusal and cleared by the commit that follows it. A refused
     render raises, so a caller that commits only after one that returned
     never reaches this -- but a caller should not have to know that to be
     safe, and a commit that promoted an emptied buffer would take the
     standing frame with it. *)
  mutable refused : bool;
}

let create () =
  {
    pending = Places.empty;
    seen = 0;
    standing = Places.empty;
    previous = Places.empty;
    remounts = Places.empty;
    refused = false;
  }

let describe = function
  | Trace.Component name -> (name, true)
  | Trace.Primitive prim -> (Camlcast.Prim.describe prim, false)

(* One less than the number of steps: the root path names no element, so the
   outermost thing a description writes is one step in, and calling that 0
   is what lets an indented listing start at the margin. *)
let depth_of path =
  let rec go path count =
    match Path.parent path with
    | None -> count
    | Some outer -> go outer (count + 1)
  in
  Int.max 0 (go path 0 - 1)

let note t path node ~mounted =
  let key = Path.to_debug_string path in
  let label, component = describe node in
  let remounts =
    if mounted && Places.mem key t.previous then begin
      let was = Option.value (Places.find_opt key t.remounts) ~default:0 in
      t.remounts <- Places.add key (was + 1) t.remounts;
      was + 1
    end
    else Option.value (Places.find_opt key t.remounts) ~default:0
  in
  let row = { path; label; depth = depth_of path; component; remounts } in
  t.pending <- Places.add key { row; order = t.seen } t.pending;
  t.seen <- t.seen + 1

let watch t = function
  | Trace.Mounted (path, node) -> note t path node ~mounted:true
  | Trace.Updated (path, node) -> note t path node ~mounted:false
  | Trace.Unmounted (path, _) ->
      t.pending <- Places.remove (Path.to_debug_string path) t.pending
  | Trace.Refused ->
      (* The events of this frame described a walk and not a tree. Nothing of
         it is kept, and the frame before stays standing. *)
      t.pending <- Places.empty;
      t.seen <- 0;
      t.refused <- true

let commit t =
  if t.refused then begin
    t.refused <- false;
    t.pending <- Places.empty;
    t.seen <- 0
  end
  else begin
    t.standing <- t.pending;
    t.previous <- Places.map (fun _ -> ()) t.pending;
    t.pending <- Places.empty;
    t.seen <- 0
  end

(* In the order the reconciler walked, which is a parent before its children:
   the order an indented listing is written in, and one this does not have to
   reconstruct from the paths. *)
let rows t =
  Places.bindings t.standing |> List.map snd
  |> List.sort (fun a b -> Int.compare a.order b.order)
  |> List.map (fun place -> place.row)

let lines ~plain ~remounted t =
  List.map
    (fun row ->
      let text =
        Printf.sprintf "%s%s%s"
          (String.make (row.depth * 2) ' ')
          row.label
          (if row.remounts > 0 then Printf.sprintf "  x%d" row.remounts else "")
      in
      { Panel.text; color = (if row.remounts > 0 then remounted else plain) })
    (rows t)

let remounting t =
  rows t
  |> List.filter (fun (row : row) -> row.remounts > 0 && row.component)
  |> List.sort (fun (a : row) (b : row) -> Int.compare b.remounts a.remounts)
