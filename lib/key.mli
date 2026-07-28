(** A place on the keyboard.

    A key is a {e scancode}: where the key sits on the board, not what is
    printed on it. {!w} is the key above {!s} on any layout, which is what a
    movement binding wants — a French player holding the key their board calls Z
    walks forward, exactly as everybody else does. The letters here are the ones
    a US board has printed on those places, because they have to be called
    something.

    The engine names them rather than passing SDL's own through, so that binding
    a key is not a reason for a game to depend on SDL. {!of_scancode} is the way
    back out for a key this module has not named. *)

type t

val count : int
(** How many places a keyboard has. {!Input} lays its controls out in one flat
    range, a block per device, and this is the size of the keyboard's. *)

val of_scancode : int -> t
(** The key at an SDL scancode. For one this module does not name: every key
    worth binding is below, but the list is a judgement about what games want
    and not a complete keyboard. *)

val to_scancode : t -> int

val name : t -> string
(** What to print on screen for this key, under the layout in use: {!a} is ["A"]
    on a QWERTY board and ["Q"] on an AZERTY one, because that is what the
    player sees printed on it. For a line of help text that says which key does
    what — see {!Camlcast_demo.Controls}.

    Falls back to the layout-independent name for a key the current layout does
    not reach, and to ["?"] for one it cannot name at all: an empty string in
    the middle of a help line reads as a bug rather than as an unnamed key.

    {b Ask for it after the window is open.} Which layout is in use is something
    SDL learns when the video subsystem starts, so before then this answers for
    a US board whatever is really plugged in. A help line built at module load
    is wrong on half the machines that read it; one built on the first frame it
    is drawn is right on all of them. *)

(** {1 Letters}

    Named for a US board. What matters is the place, not the letter — see the
    note at the top. *)

val a : t
val b : t
val c : t
val d : t
val e : t
val f : t
val g : t
val h : t
val i : t
val j : t
val k : t
val l : t
val m : t
val n : t
val o : t
val p : t
val q : t
val r : t
val s : t
val t : t
val u : t
val v : t
val w : t
val x : t
val y : t
val z : t

(** {1 The number row}

    Named as SDL names them — [k1] and not [1], which is not a name OCaml
    allows. The keypad's digits are separate keys and are further down. *)

val k1 : t
val k2 : t
val k3 : t
val k4 : t
val k5 : t
val k6 : t
val k7 : t
val k8 : t
val k9 : t
val k0 : t

(** {1 The function row} *)

val f1 : t
val f2 : t
val f3 : t
val f4 : t
val f5 : t
val f6 : t
val f7 : t
val f8 : t
val f9 : t
val f10 : t
val f11 : t
val f12 : t

(** {1 The big ones} *)

val return : t
val escape : t
val backspace : t
val tab : t
val space : t

(** {1 Punctuation}

    The places a US board prints these on. *)

val minus : t
val equals : t
val leftbracket : t
val rightbracket : t
val backslash : t
val semicolon : t
val apostrophe : t
val grave : t
val comma : t
val period : t
val slash : t

(** {1 Arrows} *)

val up : t
val down : t
val left : t
val right : t

(** {1 The block above the arrows}

    [kend] is the End key: [end] is a keyword, and SDL calls it this too. *)

val insert : t
val home : t
val pageup : t
val delete : t
val kend : t
val pagedown : t

(** {1 Modifiers}

    [lgui] and [rgui] are Command on macOS, Windows elsewhere. *)

val lshift : t
val rshift : t
val lctrl : t
val rctrl : t
val lalt : t
val ralt : t
val lgui : t
val rgui : t

(** {1 The keypad} *)

val numlockclear : t
val kp_divide : t
val kp_multiply : t
val kp_minus : t
val kp_plus : t
val kp_enter : t
val kp_period : t
val kp_1 : t
val kp_2 : t
val kp_3 : t
val kp_4 : t
val kp_5 : t
val kp_6 : t
val kp_7 : t
val kp_8 : t
val kp_9 : t
val kp_0 : t

(** {1 The rest} *)

val capslock : t
val printscreen : t
val scrolllock : t
val pause : t
val application : t
val menu : t
