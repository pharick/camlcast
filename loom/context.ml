(* Implementation of {!Camlcast_loom.Context}; the interface carries the
   prose. *)

type 'a t = { id : 'a Type.Id.t; default : 'a }
type binding = Binding : 'a t * 'a -> binding

let make default = { id = Type.Id.make (); default }
let default t = t.default
let bind context value = Binding (context, value)

(* [provably_equal] returns a type-equality proof rather than a boolean.
   Matching on the proof lets [value] be returned at the type [wanted] was
   declared with. No cast is used, and none is available: without the witness
   this function could not be written. *)
let rec find : type a. binding list -> a t -> a option =
 fun bindings wanted ->
  match bindings with
  | [] -> None
  | Binding (context, value) :: outer -> (
      match Type.Id.provably_equal context.id wanted.id with
      | Some Type.Equal -> Some value
      | None -> find outer wanted)
