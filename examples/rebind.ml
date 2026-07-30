(* The keys that walk are the game's too. This table moves walking forward
   and back onto I and K, adds Escape as the way out, and leaves strafing,
   looking and fullscreen exactly as Binding.default has them.

   This is step 13 of doc/making-a-game.mld, compiled; demo/controls.ml binds
   a second full set of walking keys and prints them with Key.name. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)

let world =
  let height = 4. in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:(Room.floor ~plane:floor ~material:ground)
      ~ceiling:(Room.roof ~plane:(Plane.above floor height) ~material:stone)
      (Room.rectangle ~height ~material:stone (Vec.make (-6.) (-6.))
         (Vec.make 6. 6.))
  in
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:Atmosphere.default
    ~spawn:("room", Vec.make (-4.5) 0.)

let bindings =
  Binding.make
    ~forward:
      {
        Binding.speed = 3.6 (* cells a second at full ask *);
        terms =
          [
            { Binding.source = Binding.Hold (Input.Key Key.i); weight = 1. };
            { Binding.source = Binding.Hold (Input.Key Key.k); weight = -1. };
          ];
      }
    ~leave:[ Input.Key Key.escape ] ()

let () =
  match
    Engine.with_window (fun window -> Engine.run_world window ~bindings world)
  with
  | Ok _ending -> ()
  | Error (`Msg m) ->
      prerr_endline m;
      exit 1
