type t =
  | Chmap of string
  | Key of (string * int) list
  | Run of string
  | Click of int
  | Delay of float
  | Toggle of int * (string * int) list
[@@deriving show { with_path = false }]

let default_activation =
  let open Input in
  function
  | Chmap _ -> [ PRESS ]
  | Key _ -> [ PRESS; RELEASE; REPEAT ]
  | Run _ -> [ PRESS ]
  | Click _ -> [ PRESS; RELEASE; REPEAT ]
  | Delay _ -> [ PRESS; RELEASE ]
  | Toggle _ -> [ PRESS ]
