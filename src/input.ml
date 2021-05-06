type keypress = RELEASE | PRESS | REPEAT
[@@deriving show { with_path = false }]

type evtype =
  | EV_UNK of { ev_type : int; code : int; value : int } (* real event type, code, value *)
  | EV_SYN
  | EV_KEY of int * keypress
  | EV_REL of { axis : int; value : int }
  | EV_ABS of { axis : int; value : int }
  | EV_MSC of { code : int; value : int }
[@@deriving show { with_path = false }]

type input_event = { time_sec : int; time_usec : int; evtype : evtype }
[@@deriving show { with_path = false }]

external read_some_input_events : Unix.file_descr -> input_event list
  = "caml_read_some_input_events"
