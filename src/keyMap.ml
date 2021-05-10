open MyLib

module KeyMap = struct
  include Map.Make (struct
    type t = Input.keypress * int

    let compare = compare
  end)

  let find_default def x map = try find x map with Not_found -> def

  let multi_add_rev k v m =
    let next = v :: find_default [] k m in
    add k next m

  let pp_key fmt ((keypress, btn) : key) =
    let open Format in
    pp_open_box fmt 0;
    pp_print_char fmt '(';
    pp_print_cut fmt ();
    Input.pp_keypress fmt keypress;
    pp_print_char fmt ',';
    pp_print_space fmt ();
    pp_print_int fmt btn;
    pp_print_cut fmt ();
    pp_print_char fmt ')';
    pp_close_box fmt ()

  let pp_kv pp_val fmt (k, v) =
    let open Format in
    pp_open_box fmt 0;
    pp_key fmt k;
    pp_print_char fmt ':';
    pp_print_space fmt ();
    pp_val fmt v;
    pp_close_box fmt ()

  let pp pp_val fmt obj =
    let open Format in
    if obj = empty then pp_print_string fmt "{}"
    else (
      pp_open_vbox fmt 2;
      pp_print_char fmt '{';
      pp_print_cut fmt ();
      pp_print_seq
        ~pp_sep:(fun f () ->
          pp_print_char f ',';
          pp_print_space f ())
        (pp_kv pp_val) fmt
      @@ to_seq obj;
      pp_print_break fmt 0 (-2);
      pp_print_char fmt '}';
      pp_close_box fmt ())
end

type t = Operator.t list KeyMap.t [@@deriving show]

let renumber_toggles lst =
  let open Operator in
  let rec aux n acc = function
    | (a, Toggle (_, k)) :: xs -> aux (succ n) ((a, Toggle (n, k)) :: acc) xs
    | x :: xs -> aux n (x :: acc) xs
    | [] -> List.rev acc
  in
  aux 0 [] lst

let of_list_rev : ((Input.keypress list * int) * Operator.t) list -> t =
  List.fold_left
    (fun acc ((kps, btn), act) ->
      List.fold_left
        (fun acc kp -> KeyMap.multi_add_rev (kp, btn) act acc)
        acc kps)
    KeyMap.empty

let collect_result_enum_rev e =
  Gen.fold (Fun.flip Result.combine_lst_right_one) (Ok []) e
  |> Result.map_error List.flatten

let load_from_gen lines =
  lines
  |> Gen.mapi Parser.parse_conf_line
  |> Gen.filter_map Fun.id
  |> Gen.map (Result.map_error List.rev)
  |> collect_result_enum_rev
  |> Result.map_both (fun x -> x |> renumber_toggles |> of_list_rev) List.rev

let load path : (t, Parser.parse_error list) result =
  Gen.IO.with_lines path load_from_gen

let find = KeyMap.find_default []

let empty = KeyMap.empty
