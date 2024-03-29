type keys = (string * int) list [@@deriving show { with_path = false }]

type t =
  | Chmap of string
  | Key of keys
  | KeyTap of keys
  | Type of keys
  | Run of string
  | RunWait of string
  | Click of int
  | Delay of float
  | Toggle of int * keys
[@@deriving show { with_path = false }]

let default_activation =
  let open Input in
  function
  | Chmap _ -> [ PRESS ]
  | Key _ -> [ PRESS; RELEASE; REPEAT ]
  | KeyTap _ -> [ PRESS ]
  | Type _ -> [ PRESS ]
  | Run _ -> [ PRESS ]
  | RunWait _ -> [ PRESS ]
  | Click _ -> [ PRESS; RELEASE; REPEAT ]
  | Delay _ -> [ PRESS; RELEASE ]
  | Toggle _ -> [ PRESS ]
