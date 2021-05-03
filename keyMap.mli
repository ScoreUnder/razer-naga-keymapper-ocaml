type t = (Operator.t * string) list MyLib.IntMap.t

val show : t -> string

val pp : Format.formatter -> t -> unit

val load : string -> (t, Parser.parse_error list) result
