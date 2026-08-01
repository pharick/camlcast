let ( let* ) = Result.bind
let ( let+ ) result f = Result.map f result

let with_resource acquire release use =
  let* resource = acquire () in
  Fun.protect ~finally:(fun () -> release resource) (fun () -> use resource)
