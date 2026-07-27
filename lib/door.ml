type state = Open | Closed
type t = { state : state; material : Material.t }

let make ?(state = Closed) material = { state; material }
let set_state t state = { t with state }
let leaf t = match t.state with Closed -> Some t.material | Open -> None
let shut t = Option.is_some (leaf t)
