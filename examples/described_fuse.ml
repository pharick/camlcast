(* examples/game.ml, described.

   The same game: space lights a fuse, the light goes out of the room as it
   burns, and when it has gone the run ends and the window closes by itself.

   Set the two side by side. game.ml keeps one record of the whole world — a
   phase, a clock and a player — and one update function that advances all of
   it, because that is the only shape the old API has. Here the phase and the
   clock belong to the component that uses them, the player belongs to the
   runtime, and there is no place where all three are written down together.

   That is the whole point of the rewrite, and this is the smallest program that
   shows it. *)

open Camlcast

let checker ~color ~u ~v =
  Color.level color (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 170)

let dressed color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = dressed (Color.rgb 150 150 160)
let ground = dressed (Color.rgb 116 110 98)
let height = 4.
let fuse = 12.
let flat = Plane.horizontal 0.

(* The air at a given amount of light left: the fade closes in and both
   brightnesses fall as the fuse burns down. *)
let air ~light =
  Atmosphere.make
    ~fog_distance:(2. +. (10. *. light))
    ~min_brightness:(0.05 +. (0.2 *. light))
    ~ambient:(0.1 +. (0.5 *. light))
    ~directional:(0.1 +. (0.3 *. light))
    ()

(* A higher-order component: it takes children and puts something around them.
   Nothing inside knows how brightly it is lit, and this knows nothing about
   what it is lighting — which is what lets either be changed without the other
   being read. There is no machinery to it, because a description is data and a
   component is a function: taking children and returning them wrapped is
   ordinary OCaml. *)
type lit = { light : float; spawn : string * Vec.t; children : P.t list }

let lit_world =
  Element.declare ~name:"lit_world" @@ fun props ->
  P.world ~atmosphere:(air ~light:props.light) ~spawn:props.spawn props.children

(* And a second one, over geometry rather than light: a square room of a given
   reach, with whatever else was given put inside it. *)
type hall = { name : string; reach : float; children : P.t list }

let hall =
  Element.declare ~name:"hall" @@ fun props ->
  let r = props.reach in
  P.(
    room ~name:props.name
      ~floor:(floor ~plane:flat ~material:ground)
      ~ceiling:(roof ~plane:(Plane.above flat height) ~material:stone)
      (outline ~height ~material:stone
         [
           Vec.make (-.r) (-.r);
           Vec.make r (-.r);
           Vec.make r r;
           Vec.make (-.r) r;
         ]
      :: props.children))

type phase = Waiting | Burning | Done

(* The whole game. Its state is its own, it says when it is over by lighting the
   world it returns, and nothing above it knows it has a clock in it. *)
let game =
  Element.declare ~name:"game" @@ fun () ->
  let phase, set_phase = Hook.use_state Waiting in
  let left, set_left = Hook.use_state fuse in
  Events.use_key_down Key.space (fun () ->
      if phase = Waiting then set_phase Burning);
  Events.use_frame (fun ~dt ->
      if phase = Burning then
        let remaining = left -. dt in
        if remaining <= 0. then begin
          set_left 0.;
          set_phase Done
        end
        else set_left remaining);
  let light =
    match phase with Waiting -> 1. | Burning -> left /. fuse | Done -> 0.
  in
  lit_world
    {
      light;
      spawn = ("room", Vec.make (-4.5) 0.);
      children =
        [
          hall { name = "room"; reach = 6.; children = [] };
          (* game.ml says this with ~finished, a callback the engine asks every
             frame. Here it is one more thing the description describes, in the
             same place and the same way as everything else it says. *)
          (if phase = Done then P.finish else Element.empty);
        ];
    }

let () =
  match Run.play ~title:"A described fuse" (game ()) with
  | Ok _ending -> ()
  | Error (`Msg message) ->
      prerr_endline message;
      exit 1
