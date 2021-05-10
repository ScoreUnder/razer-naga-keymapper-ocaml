type t

val show : t -> string

val pp : Format.formatter -> t -> unit

val load_from_gen : string Gen.t -> (t, Parser.parse_error list) result

val load : string -> (t, Parser.parse_error list) result

val find : Input.keypress * int -> t -> Operator.t list

val empty : t
