module Map = struct
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
        (pp_kv pp_val)
        fmt
      @@ to_seq obj;
      pp_print_break fmt 0 (-2);
      pp_print_char fmt '}';
      pp_close_box fmt ())
end

module Gen = struct
  include Gen

  let group_by_rev kf vf e =
    fold (fun acc el -> Map.multi_add_rev (kf el) (vf el) acc) Map.empty e
end

module Result = struct
  include Result

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

module List = struct
  include List

  let rec find_map_opt f l =
    match l with
    | x :: xs -> (
        match f x with Some v as s -> s | None -> find_map_opt f xs)
    | [] -> None
end

let collect_result_enum_rev e =
  Gen.fold
    (fun acc v ->
      match acc with
      | Ok lst -> (
          match v with Ok v -> Ok (v :: lst) | Error e -> Error [ e ])
      | Error lst -> Error (match v with Ok _ -> lst | Error v -> v :: lst))
    (Ok []) e

module NagaDaemon = struct
  let devices =
    let dev_prefix = "/dev/input/by-id/usb-Razer_" in
    [
      (* NAGA EPIC *)
      ("Razer_Naga_Epic-if01-event-kbd", "Razer_Naga_Epic-event-mouse");
      (* NAGA EPIC DOCK *)
      ("Razer_Naga_Epic_Dock-if01-event-kbd", "Razer_Naga_Epic_Dock-event-mouse");
      (* NAGA 2014 *)
      ("Razer_Naga_2014-if02-event-kbd", "Razer_Naga_2014-event-mouse");
      (* NAGA MOLTEN *)
      ("Razer_Naga-if01-event-kbd", "Razer_Naga-event-mouse");
      (* NAGA EPIC CHROMA *)
      ( "Razer_Naga_Epic_Chroma-if01-event-kbd",
        "Razer_Naga_Epic_Chroma-event-mouse" );
      (* NAGA EPIC CHROMA DOCK *)
      ( "Razer_Naga_Epic_Chroma_Dock-if01-event-kbd",
        "Razer_Naga_Epic_Chroma_Dock-event-mouse" );
      (* NAGA CHROMA *)
      ("Razer_Naga_Chroma-if02-event-kbd", "Razer_Naga_Chroma-event-mouse");
      (* NAGA HEX *)
      ("Razer_Naga_Hex-if01-event-kbd", "Razer_Naga_Hex-event-mouse");
      (* NAGA HEX v2 *)
      ("Razer_Naga_Hex_V2-if02-event-kbd", "Razer_Naga_Hex_V2-event-mouse");
      (* Naga Trinity *)
      ( "Razer_Naga_Trinity_00000000001A-if02-event-kbd",
        "Razer_Naga_Trinity_00000000001A-event-mouse" );
    ]
    |> List.map (fun (a, b) -> (dev_prefix ^ a, dev_prefix ^ b))

  let config_path = "mapping_01.txt"

  module Types = struct
    type open_file = { path : string; fd : Unix.file_descr }

    type dev_pair = { keyboard : open_file; pointer : open_file }
  end

  let open_if_exists (devkbd, devptr) =
    try
      let kbdfd = Unix.openfile devkbd [ Unix.O_RDONLY ] 0 in
      try
        let ptrfd = Unix.openfile devptr [ Unix.O_RDONLY ] 0 in
        Some
          Types.
            {
              keyboard = { path = devkbd; fd = kbdfd };
              pointer = { path = devptr; fd = ptrfd };
            }
      with Unix.Unix_error _ ->
        Unix.close kbdfd;
        None
    with Unix.Unix_error _ -> None

  let find_razer_device (devices : (string * string) list) :
      Types.dev_pair option =
    devices |> List.find_map_opt open_if_exists

  let load_conf path =
    Gen.(
      IO.with_lines path (fun lines ->
          lines
          |> mapi Parser.parse_conf_line
          |> filter_map (fun x -> x)
          |> collect_result_enum_rev
          |> Result.map_both
               (fun x -> x |> of_list |> group_by_rev fst snd)
               List.rev))
end

let init_devices devices =
  ignore @@ NagaDaemon.Types.(Ioctl.(eviocgrab devices.keyboard.fd))

let rec run devices config state =
  ignore (run, config, state);
  let open NagaDaemon.Types in
  match
    Unix.select [ devices.keyboard.fd; devices.pointer.fd ] [] [] (-1.0)
  with
  | ls, _, _ ->
      (if List.mem devices.keyboard.fd ls then
       let size = 0x18 in
       let buf = Bytes.create (size * 64) in
       ignore @@ Unix.read devices.keyboard.fd buf 0 (Bytes.length buf));
      if List.mem devices.pointer.fd ls then ()

let () =
  let initial_config =
    NagaDaemon.(load_conf config_path)
    |> Result.map_error [%derive.show: Parser.parse_error list]
  in
  let devices =
    NagaDaemon.(find_razer_device devices)
    |> Result.of_option_d
         "No naga devices found or you don't have permission to access them."
  in

  initial_config
  |> Result.iter (fun result ->
         print_endline
           ([%derive.show: (Operator.operator * string) list Map.t] result));

  let combined_result = Result.combine initial_config devices in
  let open NagaDaemon.Types in
  combined_result
  |> Result.iter (fun (initial_config, devices) ->
         Printf.printf "Reading from: %s and %s\n%!" devices.keyboard.path
           devices.pointer.path;
         init_devices devices;
         run devices initial_config Map.empty);

  devices
  |> Result.iter (fun devices ->
         Unix.close devices.keyboard.fd;
         Unix.close devices.pointer.fd);

  combined_result
  |> Result.iter_error (fun err ->
         prerr_endline err;
         exit 1)
