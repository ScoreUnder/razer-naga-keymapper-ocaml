type exec_state

val initial_state : KeyMap.t -> exec_state

val pp_keymap_load_failure :
  Format.formatter -> string -> Parser.parse_error list -> unit

val run_actions :
  X11.display -> exec_state -> Input.keypress -> int -> exec_state
