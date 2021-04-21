module Result = struct
  include Result

  let pull_fst (a, b) = map (fun v -> (v, b)) a

  let pull_snd (a, b) = map (fun v -> (a, v)) b

  let catch f x = try Ok (f x) with e -> Error e

  (** [combine result1 result2] joins a pair of results, joining the error
      string with a newline if necessary and returning the OK result as a
      tuple2 *)
  let combine a b =
    match a with
    | Ok oka -> (
        match b with Ok okb -> Ok (oka, okb) | Error erb -> Error erb)
    | Error era -> (
        match b with Ok _ -> Error era | Error erb -> Error (era ^ "\n" ^ erb))

  (** converts an option into a result, given a default value for the error case *)
  let of_option_d d o = match o with Some x -> Ok x | None -> Error d

  let map_both o e r = match r with Ok v -> Ok (o v) | Error v -> Error (e v)
end

module String = struct
  include String

  let split_once ~chr str =
    index_opt str chr
    |> Option.map (fun ind ->
           let after_pos = succ ind in
           (sub str 0 ind, sub str after_pos (String.length str - after_pos)))

  let starts_with ~chr str = if str = "" then false else str.[0] == chr
end

module IntMap = struct
  include Map.Make (Int)

  let find_default def x map = try find x map with Not_found -> def

  let multi_add_rev k v m =
    let next = v :: find_default [] k m in
    add k next m

  let pp_key fmt (key : key) = Format.pp_print_int fmt key

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

module Gen = struct
  include Gen

  let group_by_rev kf vf e =
    fold (fun acc el -> IntMap.multi_add_rev (kf el) (vf el) acc) IntMap.empty e
end

module List = struct
  include List

  let rec find_map_opt f l =
    match l with
    | x :: xs -> (
        match f x with Some _ as s -> s | None -> find_map_opt f xs)
    | [] -> None
end
