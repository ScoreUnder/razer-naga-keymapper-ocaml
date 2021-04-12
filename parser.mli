type parse_error =
  | UnknownOperation of int * string
  | MissingHyphen of int
  | MissingEquals of int
  | BadNumber of int * string

val pp_parse_error : Format.formatter -> parse_error -> unit

val show_parse_error : parse_error -> string

val parse_conf_action :
  int -> string -> (Operator.operator * string, parse_error) result

val parse_conf_line :
  int ->
  string ->
  (int * (Operator.operator * string), parse_error) result option
