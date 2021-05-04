type parse_error

val pp_parse_error : Format.formatter -> parse_error -> unit

val show_parse_error : parse_error -> string

type error_list = parse_error list

val pp_error_list : Format.formatter -> error_list -> unit

val show_error_list : error_list -> string

val parse_conf_action : int -> string -> (Operator.t, parse_error) result

val parse_conf_line :
  int -> string -> (int * Operator.t, parse_error) result option
