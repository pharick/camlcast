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

let depth t = t.depth
let steps t = List.rev t.rev

(* A keyed step is its key and an unkeyed one is its index; a keyed step and an
   unkeyed one are never the same place, even should the numbers line up. *)
let same_step a b =
  match (a.key, b.key) with
  | Some ka, Some kb -> String.equal ka kb
  | None, None -> a.index = b.index
  | Some _, None | None, Some _ -> false

let equal a b = a.depth = b.depth && List.equal same_step a.rev b.rev

let compare_step a b =
  match (a.key, b.key) with
  | Some ka, Some kb -> String.compare ka kb
  | None, None -> Int.compare a.index b.index
  (* Unkeyed before keyed, arbitrarily but consistently. *)
  | None, Some _ -> -1
  | Some _, None -> 1

let compare a b =
  match Int.compare a.depth b.depth with
  | 0 -> List.compare compare_step a.rev b.rev
  | order -> order

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

(* A key is dropped-index enough on its own: it says which of its siblings this
   is, and says it in the terms the matching actually uses, so two steps under
   one parent cannot share one. Without a key the index is the only thing that
   tells siblings apart, and it is printed whether or not the step is named.

   The name alone is not enough, which is what this used to print. Two unkeyed
   siblings of one component — [torch (); torch ()], the ordinary way to write
   two of a thing — are two different places with one name between them, and
   printing them alike broke the one promise this spelling makes over
   {!to_string}. It is the promise the whole second spelling exists for: a trace
   that says [mount torch] twice has told you nothing about which, and
   {!Hook_order_changed} naming [torch] sends a reader to look at both. *)
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
