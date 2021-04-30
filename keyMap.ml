open MyLib

type t = (Operator.operator * string) list IntMap.t [@@deriving show]

let rec renumber_toggles n lst =
  let open Operator in
  let rec inner n acc = function
    | Toggle _ :: xs -> inner (succ n) xs (Toggle n :: acc)
    | x :: xs -> inner n xs (x :: acc)
    | [] -> (List.rev acc, n)
  in
  let rec aux n acc = function
    | x :: xs ->
        let inner_list, n = inner n [] x in
        aux n (inner_list :: acc) xs
    | [] -> List.rev acc
  in
  aux 0 [] lst

let load path : (t, Parser.parse_error list) result =
  Gen.(
    IO.with_lines path (fun lines ->
        lines
        |> mapi Parser.parse_conf_line
        |> filter_map (fun x -> x)
        |> collect_result_enum_rev
        |> Result.map_both
             (fun x -> x |> of_list |> group_by_rev fst snd)
             List.rev))
