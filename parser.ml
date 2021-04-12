open Batteries

module Result = struct
  include Result

  let pull_fst (a, b) = map (fun v -> (v, b)) a

  let pull_snd (a, b) = map (fun v -> (a, v)) b
end

module String = struct
  include String

  let split_once ~chr str =
    index_opt str chr
    |> Option.map (fun ind -> (slice ~last:ind str, slice ~first:(ind + 1) str))
end

type parse_error =
  | UnknownOperation of int * string
  | MissingHyphen of int
  | MissingEquals of int
  | BadNumber of int * string
[@@deriving show { with_path = false }]

let parse_conf_action num line =
  match String.split_once ~chr:'=' line with
  | None -> Error (MissingEquals num)
  | Some (left, right) ->
      let left =
        Operator.of_string (fun err -> UnknownOperation (num, err))
        @@ String.trim left
      in
      let right = String.trim right in
      (left, right) |> Result.pull_fst

let parse_conf_line num line =
  let num = succ num (* make lines 1-indexed *) in
  let trimmed = String.trim line in
  if String.starts_with trimmed "#" || trimmed = "" then None
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
