(** Binding operators for [Result], so that fallible SDL calls can be chained
    with [let*] instead of a staircase of [match ... with Ok _ | Error _].

    Both operators short-circuit: the first [Error] is the answer and nothing
    after it runs. Which of the two to reach for is decided by what the body
    gives back — another [result], or a plain value. *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** [Result.bind]: [let* x = e in body] runs [body] on the value of [e] if it is
    [Ok], and [body] returns a [result] of its own. For chaining one fallible
    call onto another. *)

val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
(** [Result.map], arguments the other way round: [let+ x = e in body] is the
    same, except that [body] returns a plain value which is wrapped in [Ok] for
    you. For the last step of a chain, where nothing further can fail. *)

val with_resource :
  (unit -> ('a, 'e) result) ->
  ('a -> unit) ->
  ('a -> ('b, 'e) result) ->
  ('b, 'e) result
(** [with_resource acquire release use] acquires a resource, uses it, and
    releases it even if the body raises or returns an error. The result is
    [use]'s, passed through untouched. Failing to acquire is propagated and
    releases nothing, since there is nothing to release.

    Releasing is [Fun.protect], so an exception from [use] is {e not} turned
    into an [Error] — the resource is released and the exception carries on up.

    @raise Fun.Finally_raised
      if [release] itself raises, which replaces whatever [use] was raising or
      returning. A release that can fail should swallow its own errors. *)
