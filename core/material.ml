type t = { pattern : Texture.t }

let make ~pattern = { pattern }
let opaque t = Texture.opaque t.pattern

let opaque_at t ~along ~above =
  Texture.alpha t.pattern
    ~u:(Texture.column_of_offset t.pattern (along -. Float.floor along))
    ~v:(Texture.row_of_height t.pattern above)
  = 255

let plane_texel t ~x ~y =
  let frac v = v -. Float.floor v in
  Texture.sample t.pattern
    ~u:(Texture.column_of_offset t.pattern (frac x))
    ~v:(Texture.column_of_offset t.pattern (frac y))
