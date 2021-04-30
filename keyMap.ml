open MyLib

type t = (Operator.operator * string) list IntMap.t [@@deriving show]

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
