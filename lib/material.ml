type t = { pattern : Texture.t }

let make ~pattern = { pattern }
let opaque t = t.pattern.Texture.opaque

let plane_texel t ~x ~y =
  let frac v = v -. Float.floor v in
  Texture.sample t.pattern
    ~u:(Texture.column_of_offset t.pattern (frac x))
    ~v:(Texture.column_of_offset t.pattern (frac y))
