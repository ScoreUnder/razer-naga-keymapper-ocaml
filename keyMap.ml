open MyLib

type t = Operator.t list IntMap.t [@@deriving show]

let renumber_toggles lst =
  let open Operator in
  let rec aux n acc = function
    | (a, Toggle (_, k)) :: xs -> aux (succ n) ((a, Toggle (n, k)) :: acc) xs
    | x :: xs -> aux n (x :: acc) xs
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
             (fun x -> x |> renumber_toggles |> of_list |> group_by_rev fst snd)
             List.rev))

let find = IntMap.find_default []
