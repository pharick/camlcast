(* Implementation of {!Camlcast_loom.Path}; the interface carries the prose. *)

type step = { index : int; key : string option; name : string option }

(* Deepest step first, so {!child} conses onto the front and shares the rest.
   [depth] is carried rather than counted so {!equal} can reject two paths of
   different lengths without walking either of them, which is the common case
   when a subtree has grown or shrunk. *)
type t = { rev : step list; depth : int }

let root = { rev = []; depth = 0 }

let child parent ?key ?name index =
  { rev = { index; key; name } :: parent.rev; depth = parent.depth + 1 }

(* Outermost first: the order a path is read in, and the reverse of the order
   it is built in. Not exported, because only the two printers below need the
   list and exporting a copy would encourage callers to walk it instead of
   using {!equal}. *)
let steps t = List.rev t.rev

(* A keyed step is identified by its key, an unkeyed one by its index. A keyed
   step and an unkeyed one are never the same place, even if the numbers
   match. *)
let same_step a b =
  match (a.key, b.key) with
  | Some ka, Some kb -> String.equal ka kb
  | None, None -> a.index = b.index
  | Some _, None | None, Some _ -> false

let equal a b = a.depth = b.depth && List.equal same_step a.rev b.rev

let show_step step =
  match (step.name, step.key) with
  | Some name, Some key -> Some (name ^ "[" ^ key ^ "]")
  | Some name, None -> Some name
  | None, Some key -> Some ("[" ^ key ^ "]")
  | None, None -> None

let to_string t =
  match List.filter_map show_step (steps t) with
  | [] -> "(root)"
  | shown -> String.concat " / " shown

(* A key alone is enough to drop the index: it identifies which sibling this
   is, in the terms the matching actually uses, and two steps under one parent
   cannot share a key. Without a key the index is the only thing that tells
   siblings apart, so it is printed whether or not the step is named.

   The name alone is not enough, tempting as it is to print only it. Two
   unkeyed siblings of one component — [torch (); torch ()], the ordinary way
   to write two of a thing — are two different places sharing one name.
   Printing them alike breaks the one promise this spelling makes over
   {!to_string}, the promise the second spelling exists for: a trace that says
   [mount torch] twice does not say which, and a {!Hook_order_changed} naming
   [torch] sends the reader to look at both. *)
let debug_step step =
  let index = "#" ^ string_of_int step.index in
  match (step.name, step.key) with
  | Some name, Some key -> name ^ "[" ^ key ^ "]"
  | Some name, None -> name ^ index
  | None, Some key -> "[" ^ key ^ "]"
  | None, None -> index

let to_debug_string t =
  match steps t with
  | [] -> "(root)"
  | steps -> String.concat "/" (List.map debug_step steps)
