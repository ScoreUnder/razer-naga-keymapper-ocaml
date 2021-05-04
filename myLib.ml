module Result = struct
  include Result

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

  let combine_lst_rev l =
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
        match b with Ok _ -> e | Error erb -> Error (era @ erb))

  let map_both o e r = match r with Ok v -> Ok (o v) | Error v -> Error (e v)

  let list_rev r = map_both List.rev List.rev r

  module Syntax = struct
    let ( let+ ) a b = map b a

    let ( and* ) a b = combine_tup a b
  end
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

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module List = struct
  include List

  let rec find_map_opt f l =
    match l with
    | x :: xs -> (
        match f x with Some _ as s -> s | None -> find_map_opt f xs)
    | [] -> None
end
