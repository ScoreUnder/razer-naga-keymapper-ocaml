type t =
  | Chmap of string
  | Key of (string * int) list
  | Run of string
  | Run2 of string
  | Click of int
  | Delay of float
  | Toggle of int * (string * int) list
[@@deriving show { with_path = false }]
