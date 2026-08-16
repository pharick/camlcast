(** A position on the keyboard.

    A key is a {e scancode}: where the key sits on the board, not what is
    printed on it. {!w} is the key above {!s} on any layout, which is what a
    movement binding wants — a French player holding the key their board calls Z
    walks forward like everyone else, so bindings survive layout changes. The
    letters here are the ones a US board prints on those positions, because the
    values need names.

    The engine names them rather than passing SDL's own through, so binding a
    key does not force a game to depend on SDL. {!of_scancode} covers a key this
    module has not named. *)

type t
(** One position on the board. Abstract, and every value below is in range, so
    the only way to hold an out-of-range one is {!of_scancode} — which is why
    that is the only function here that can fail. *)

val count : int
(** The number of positions a keyboard has. {!Input} lays its controls out in
    one flat range, a block per device; this is the size of the keyboard's
    block. *)

val of_scancode : int -> t
(** The key at an SDL scancode, for a key this module does not name. Every key
    worth binding is listed below, but the list is a judgement about what games
    want, not a complete keyboard.

    The scancode must be a position a keyboard has — at least zero and less than
    {!count} — and an out-of-range one is refused rather than returned as a key
    that is silently something else. {!Input} lays keys and mouse buttons out in
    one flat range with the buttons starting where the keyboard ends, so the key
    at {!count} would be the left mouse button and would be believed.

    @raise Invalid_argument if the scancode is not a position a keyboard has. *)

val to_scancode : t -> int
(** The SDL scancode of a key: at least zero and below {!count}, and the two-way
    inverse of {!of_scancode}. For handing a key back to SDL or to anything else
    that speaks scancodes. *)

val name : t -> string
(** The on-screen name for this key under the layout in use: {!a} is ["A"] on a
    QWERTY board and ["Q"] on an AZERTY one, matching what the player sees
    printed on the key. For a line of help text saying which key does what — see
    the [controls] demo.

    Falls back to the layout-independent name for a key the current layout does
    not reach, and to ["?"] for one it cannot name at all: an empty string in
    the middle of a help line reads as a bug rather than as an unnamed key.

    {b Ask for it after the window is open.} SDL learns the layout when the
    video subsystem starts; before then this answers as for a US board
    regardless of what is plugged in. A help line built at module load is wrong
    on non-US machines; one built on the first frame it is drawn is right on all
    of them. *)

(** {1 Letters}

    Named for a US board. What matters is the position, not the letter — see the
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

(** {1 The function row}

    F1 to F12. The engine cannot know which of these the desktop has already
    claimed — F11 is fullscreen here, and window managers take others — so a
    game that binds them should expect a few never to arrive. *)

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

    The positions a US board prints these on. *)

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

(** {1 Arrows}

    The four arrow keys. {!Binding.default} turns with left and right and looks
    with up and down, so the engine walks and looks without a mouse. *)

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

(** {1 The keypad}

    Separate positions from the number row above, so [kp_1] and {!k1} are two
    keys and binding one does not bind the other. Whether they arrive at all
    depends on Num Lock and on the board having them. *)

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
