type keypress = RELEASE | PRESS | REPEAT
[@@deriving show { with_path = false }]

type evtype =
  | EV_UNK of int * int * int (* real event type, code, value *)
  | EV_SYN
  | EV_KEY of int * keypress
  | EV_REL of int * int (* axis, value *)
  | EV_ABS of int * int (* axis, value *)
  | EV_MSC of int * int
[@@deriving show { with_path = false }]

type input_event = { time_sec : int; time_usec : int; evtype : evtype }
[@@deriving show { with_path = false }]

external read_some_input_events : Unix.file_descr -> input_event array
  = "caml_read_some_input_events"
