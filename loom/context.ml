(* Implementation of {!Camlcast_loom.Context}; the interface carries the
   prose. *)

type 'a t = { id : 'a Type.Id.t; default : 'a }
type binding = Binding : 'a t * 'a -> binding

let make default = { id = Type.Id.make (); default }
let default t = t.default
let bind context value = Binding (context, value)

(* [provably_equal] returns a proof rather than a boolean, and matching on that
   proof is what lets [value] leave here at the type [wanted] was declared with.
   No cast, and none available: without the witness there would be no way to
   write this function at all. *)
let rec find : type a. binding list -> a t -> a option =
 fun bindings wanted ->
  match bindings with
  | [] -> None
  | Binding (context, value) :: outer -> (
      match Type.Id.provably_equal context.id wanted.id with
      | Some Type.Equal -> Some value
      | None -> find outer wanted)
