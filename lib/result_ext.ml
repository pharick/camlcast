let ( let* ) = Result.bind
let ( let+ ) result f = Result.map f result

let rec iter_range ~first ~last f =
  if first > last then Ok ()
  else
    let* () = f first in
    iter_range ~first:(first + 1) ~last f

let with_resource acquire release use =
  let* resource = acquire () in
  Fun.protect ~finally:(fun () -> release resource) (fun () -> use resource)
