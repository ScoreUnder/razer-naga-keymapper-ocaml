open MyLib

type parse_error =
  | UnknownOperation of int * string
  | MissingHyphen of int
  | MissingEquals of int
  | BadNumber of int * string
  | BadKeyName of int * string
  | BadKeypressType of int * string
  | BadUnicode of int * string
[@@deriving show { with_path = false }]

type error_list = parse_error list [@@deriving show { with_path = false }]

let parse_syms_then f line_num syms =
  try Ok (f (X11.parse_keysyms syms))
  with X11.Bad_key_name name -> Error [ BadKeyName (line_num, name) ]

let str_to_charcodes line_num s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec consume acc =
    match Uutf.decode decoder with
    | `Uchar u -> consume (Ok u :: acc)
    | `End -> acc
    | `Malformed b -> consume (Error (BadUnicode (line_num, b)) :: acc)
    | `Await -> failwith "Uutf expects manual supply?"
  in
  Result.combine_lst_rev (consume [])

let charcodes_to_keys num lst =
  lst
  |> List.map (fun ch ->
         let keyname = ch |> Uchar.to_int |> Printf.sprintf "U%04x" in
         try Ok (keyname, X11.parse_keysym keyname)
         with X11.Bad_key_name name -> Error (BadKeyName (num, name)))
  |> Result.combine_lst_rev |> Result.list_rev

let str_to_keys num str =
  Result.bind (str_to_charcodes num str) (charcodes_to_keys num)

let operator_of_string line_num cmd =
  let open Operator in
  function
  | "chmap" -> Ok (Chmap cmd)
  | "key" -> parse_syms_then (fun s -> Key s) line_num cmd
  | "keytap" -> parse_syms_then (fun s -> KeyTap s) line_num cmd
  | "type" -> str_to_keys line_num cmd |> Result.map (fun r -> Type r)
  | "run" -> Ok (Run cmd)
  | "click" ->
      int_of_string_opt cmd
      |> Option.to_result ~none:[ BadNumber (line_num, cmd) ]
      |> Result.map (fun btn -> Click btn)
  | "delay" ->
      float_of_string_opt cmd
      |> Option.to_result ~none:[ BadNumber (line_num, cmd) ]
      |> Result.map (fun time -> Delay (time /. 1000.))
  | "toggle" -> parse_syms_then (fun s -> Toggle (0, s)) line_num cmd
  | bad -> Error [ UnknownOperation (line_num, bad) ]

let parse_conf_action num line =
  match String.split_once ~chr:'=' line with
  | None -> Error [ MissingEquals num ]
  | Some (left, right) ->
      operator_of_string num (String.trim right) (String.trim left)

let parse_keypress_type line_num str =
  let open Input in
  match str with
  | "PRESS" -> Ok PRESS
  | "RELEASE" -> Ok RELEASE
  | "REPEAT" -> Ok REPEAT
  | _ -> Error (BadKeypressType (line_num, str))

let parse_keypress_types num str =
  String.split_on_char ' ' str
  |> List.filter_map (fun s ->
         if s = "" then None
         else Some (parse_keypress_type num (String.uppercase_ascii s)))
  |> Result.combine_lst_rev

let parse_conf_line num line =
  let num = succ num (* make lines 1-indexed *) in
  let trimmed = String.trim line in
  if String.starts_with trimmed ~chr:'#' || trimmed = "" then None
  else
    Some
      (let open Result.Syntax in
      let* left, right =
        String.split_once ~chr:'-' trimmed
        |> Option.to_result ~none:[ MissingHyphen num ]
      in
      let left = String.trim left in
      let keystr, press_str =
        left |> String.split_once ~chr:' ' |> Option.value ~default:(left, "")
      in
      let+ keypresses = parse_keypress_types num press_str
      and* keycode =
        Result.catch int_of_string keystr
        |> Result.map_error @@ Fun.const [ BadNumber (num, keystr) ]
      and* action = parse_conf_action num right in
      let keypresses =
        if keypresses <> [] then keypresses
        else Operator.default_activation action
      in
      ((keypresses, keycode), action))
