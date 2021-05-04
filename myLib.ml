module Result = struct
  include Result

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

  let combine_lst l =
    let rec aux_err acc = function
      | Ok _ :: xs -> aux_err acc xs
      | Error x :: xs -> aux_err (x :: acc) xs
      | [] -> Error acc
    in
    let rec aux acc = function
      | Ok x :: xs -> aux (x :: acc) xs
      | Error x :: xs -> aux_err [ x ] xs
      | [] -> Ok acc
    in
    aux [] l

  let combine_tup a b =
    match a with
    | Ok oka -> ( match b with Ok okb -> Ok (oka, okb) | Error _ as e -> e)
    | Error era as e -> (
        match b with Ok okb -> e | Error erb -> Error (era @ erb))

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

  let eq_no_case a b =
    let len = String.length a in
    let rec cmp_chrs n =
      if n = len then true
      else if a.[n] <> b.[n] then false
      else cmp_chrs (succ n)
    in
    len = String.length b && cmp_chrs 0
end

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module List = struct
  include List

  let rec find_map_opt f l =
    match l with
    | x :: xs -> (
        match f x with Some _ as s -> s | None -> find_map_opt f xs)
    | [] -> None

  let rec assoc_opt_eq eq v = function
    | (x, y) :: _ when eq x v -> Some y
    | _ :: xs -> assoc_opt_eq eq v xs
    | [] -> None
end
