open MyLib

let const x _ = x

type parse_error =
  | UnknownOperation of int * string
  | MissingHyphen of int
  | MissingEquals of int
  | BadNumber of int * string
  | BadKeyName of int * string
[@@deriving show { with_path = false }]

type error_list = parse_error list [@@deriving show { with_path = false }]

let operator_of_string line_num cmd =
  let open Operator in
  function
  | "chmap" -> Ok (Chmap cmd)
  | "key" -> (
      try Ok (Key (X11.parse_keysyms cmd))
      with X11.Bad_key_name name -> Error (BadKeyName (line_num, name)))
  | "run" -> Ok (Run cmd)
  | "run2" -> Ok (Run2 cmd)
  | "click" ->
      int_of_string_opt cmd
      |> Option.to_result ~none:(BadNumber (line_num, cmd))
      |> Result.map (fun btn -> Click btn)
  | "delay" ->
      float_of_string_opt cmd
      |> Option.to_result ~none:(BadNumber (line_num, cmd))
      |> Result.map (fun time -> Delay (time /. 1000.))
  | "toggle" -> (
      try Ok (Toggle (0, X11.parse_keysyms cmd))
      with X11.Bad_key_name name -> Error (BadKeyName (line_num, name)))
  | bad -> Error (UnknownOperation (line_num, bad))

let parse_conf_action num line =
  match String.split_once ~chr:'=' line with
  | None -> Error (MissingEquals num)
  | Some (left, right) ->
      operator_of_string num (String.trim right) (String.trim left)

let parse_conf_line num line =
  let num = succ num (* make lines 1-indexed *) in
  let trimmed = String.trim line in
  if String.starts_with trimmed ~chr:'#' || trimmed = "" then None
  else
    Some
      (match String.split_once ~chr:'-' trimmed with
      | None -> Error (MissingHyphen num)
      | Some (left, right) ->
          let ( let* ) = Result.bind in
          let left = String.trim left in
          let* keycode =
            Result.catch int_of_string left
            |> Result.map_error @@ const @@ BadNumber (num, left)
          in
          (keycode, parse_conf_action num right) |> Result.pull_snd)
