(** Binding operators for [Result], so that fallible SDL calls can be chained
    with [let*] instead of a staircase of [match ... with Ok _ | Error _]. *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

val with_resource :
  (unit -> ('a, 'e) result) ->
  ('a -> unit) ->
  ('a -> ('b, 'e) result) ->
  ('b, 'e) result
(** [with_resource acquire release use] acquires a resource, uses it, and
    releases it even if the body raises or returns an error. Failing to acquire
    it is propagated and releases nothing, since there is nothing to release. *)
