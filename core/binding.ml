(** See {!Binding} for what a binding table is and how an axis adds up. This is
    the table itself, the engine's default one, and the one function that reads
    it — which is pure, and is the reason the reading and the meaning are two
    modules rather than one. *)

type source = Hold of Input.control | Read of Input.analog
type term = { source : source; weight : float }
type axis = { terms : term list; speed : float }

type t = {
  forward : axis;
  strafe : axis;
  turn : axis;
  pitch : axis;
  fullscreen : Input.control list;
  leave : Input.control list;
}

(* An axis from a pair of controls that push it opposite ways: the shape almost
   every keyboard binding has, and the shape the engine's own default is in. *)
let opposed ~speed ~positive ~negative =
  {
    speed;
    terms =
      List.map (fun control -> { source = Hold control; weight = 1. }) positive
      @ List.map
          (fun control -> { source = Hold control; weight = -1. })
          negative;
  }

let key k = Input.Key k

let default =
  {
    forward =
      opposed ~speed:Config.move_speed
        ~positive:[ key Key.w ]
        ~negative:[ key Key.s ];
    strafe =
      opposed ~speed:Config.move_speed
        ~positive:[ key Key.d ]
        ~negative:[ key Key.a ];
    turn =
      (let arrows =
         opposed ~speed:Config.turn_speed
           ~positive:[ key Key.right ]
           ~negative:[ key Key.left ]
       in
       {
         arrows with
         terms =
           arrows.terms
           @ [
               { source = Read Input.Mouse_x; weight = Config.look_sensitivity };
             ];
       });
    pitch =
      (let arrows =
         opposed ~speed:Config.pitch_speed
           ~positive:[ key Key.up ]
           ~negative:[ key Key.down ]
       in
       {
         arrows with
         terms =
           arrows.terms
           (* Mouse up is a negative delta but should look up, which is the
              whole of why this weight is the only negative one. *)
           @ [
               {
                 source = Read Input.Mouse_y;
                 weight = -.Config.pitch_sensitivity;
               };
             ];
       });
    fullscreen = [ key Key.f11 ];
    leave = [];
  }

let make ?(forward = default.forward) ?(strafe = default.strafe)
    ?(turn = default.turn) ?(pitch = default.pitch)
    ?(fullscreen = default.fullscreen) ?(leave = default.leave) () =
  { forward; strafe; turn; pitch; fullscreen; leave }

(** One axis, over one frame. The two accumulators are the whole of the rate
    versus displacement distinction {!Binding} sets out: rates are asks, so they
    are summed, clamped and paid out at [speed] over the frame; displacements
    are movements that have already happened, so they are added as they stand.

    The clamp is not a nicety. Without it two keys bound to [forward] would ask
    for two, and the player would walk at twice the speed the axis says — which
    is how the engine's own reading of the keyboard behaved before there was a
    table, and what it used [List.exists] to avoid. *)
let axis_value axis actions ~dt =
  let rate, displacement =
    List.fold_left
      (fun (rate, displacement) { source; weight } ->
        match source with
        | Hold control ->
            if Input.down actions control then (rate +. weight, displacement)
            else (rate, displacement)
        | Read analog -> (
            let read = Input.value actions analog *. weight in
            match Input.reads analog with
            | Input.Rate -> (rate +. read, displacement)
            | Input.Displacement -> (rate, displacement +. read)))
      (0., 0.) axis.terms
  in
  (Float.max (-1.) (Float.min 1. rate) *. axis.speed *. dt) +. displacement

let motion bindings actions ~dt =
  {
    Input.forward = axis_value bindings.forward actions ~dt;
    strafe = axis_value bindings.strafe actions ~dt;
    turn = axis_value bindings.turn actions ~dt;
    pitch = axis_value bindings.pitch actions ~dt;
  }

let taken controls actions = List.exists (Input.pressed actions) controls
