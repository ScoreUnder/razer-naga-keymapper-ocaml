type operator =
  | Chmap
  | Key
  | Run
  | Run2
  | Click
  | Delay
  | Toggle of int
[@@deriving show { with_path = false }]

let of_string err = function
  | "chmap" -> Ok Chmap
  | "key" -> Ok Key
  | "run" -> Ok Run
  | "run2" -> Ok Run2
  | "click" -> Ok Click
  | "delay" -> Ok Delay
  | "toggle" -> Ok (Toggle 0)
  | bad -> Error (err bad)
