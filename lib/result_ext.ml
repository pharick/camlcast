(** Binding operators for [Result], so that fallible SDL calls can be chained
    with [let*] instead of a staircase of [match ... with Ok _ | Error _]. *)

let ( let* ) = Result.bind
let ( let+ ) result f = Result.map f result

(** [iter_range ~first ~last f] applies [f] to every index in the inclusive
    range, stopping and propagating the first error. This is the [Result]
    flavoured equivalent of a [for] loop. *)
let rec iter_range ~first ~last f =
  if first > last then Ok ()
  else
    let* () = f first in
    iter_range ~first:(first + 1) ~last f
