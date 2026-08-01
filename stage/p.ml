(* Implementation of {!Camlcast_stage.P}; the interface carries the prose. *)

open Camlcast_core
module E = Camlcast_loom.Element

type t = Prim.t E.t

let floor = Room.floor
let roof = Room.roof
let open_sky = Room.open_sky

let world ~atmosphere ~spawn children =
  E.prim ~children (Prim.World { atmosphere; spawn })

let room ?key ~name ~floor ~ceiling children =
  E.prim ?key ~children (Prim.Room { name; floor; ceiling })

let wall ?key ?(decals = []) ~height ~material a b =
  E.prim ?key ~children:decals (Prim.Wall { a; b; height; material })

let decal ?key ?facing ?glow ~along ~z ~half_width ~half_height image =
  E.prim ?key
    (Prim.Decal
       (Room.decal ?facing ?glow ~along ~z ~half_width ~half_height image))

let sprite ?key ?base ~size ~image pos =
  E.prim ?key (Prim.Sprite (Room.sprite ?base ~size ~image pos))

let camera ?(pitch = 0.) ~room ~pos ~angle () =
  E.prim (Prim.Camera { room; pos; angle; pitch })

let hud children = E.prim ~children Prim.Hud

let rect ?key ?(alpha = 255) ~x ~y ~w ~h ~color () =
  E.prim ?key (Prim.Rect { x; y; w; h; color; alpha })

let bar ?key ~x ~y ~w ~h ~fraction ~color () =
  E.prim ?key (Prim.Bar { x; y; w; h; fraction; color })

let text ?key ?(color = Color.rgb 255 255 255) ~font ~x ~y body =
  E.prim ?key (Prim.Text { x; y; text = body; color; font })

let picture ?key ?tint ~x ~y image =
  E.prim ?key (Prim.Picture { x; y; image; tint })

let crosshair ?(color = Color.rgb 255 255 255) () =
  E.prim (Prim.Crosshair color)

let finish = E.prim Prim.Finish
let link here there = E.prim (Prim.Link { here; there })

(* Room.wall is private, so its parts can be read back out — which is what lets
   the helpers below hand their work to the same Wall primitive a game writes by
   hand, decals and all, rather than needing a second kind of wall for walls the
   engine built. *)
let of_wall (w : Room.wall) = wall ~height:w.height ~material:w.material w.a w.b

(* Twice the signed area, by the shoelace sum. Positive is the winding
   Room.rectangle produces, and Room.rectangle is the one boundary the engine
   documents as impossible to get wrong — so it is the definition to measure
   against rather than a rule restated here and left to drift from it. *)
let twice_signed_area points =
  let rec go total = function
    | (p : Vec.t) :: ((q : Vec.t) :: _ as rest) ->
        go (total +. (p.x *. q.y) -. (q.x *. p.y)) rest
    | [ last ] -> (
        match points with
        | (first : Vec.t) :: _ ->
            total +. (last.Vec.x *. first.y) -. (first.x *. last.Vec.y)
        | [] -> total)
    | [] -> total
  in
  go 0. points

let wound points =
  if twice_signed_area points < 0. then List.rev points else points

let path ~height ~material points =
  E.fragment
    (List.map of_wall
       (Room.path ~closed:false ~height ~material (wound points)))

let outline ~height ~material points =
  let wound = wound points in
  (* Room.path does the closing and the refusing — fewer than three corners, two
     the same in a row — so those stay refused in one place and in the words
     they have always been refused in. *)
  E.fragment (List.map of_wall (Room.path ~closed:true ~height ~material wound))

let doorway ?door ~name ~width ~opening ~height ~material a b =
  let jambs, threshold =
    Room.doorway ?door ~name ~width ~opening ~height ~material a b
  in
  E.fragment (List.map of_wall jambs @ [ E.prim (Prim.Threshold threshold) ])
