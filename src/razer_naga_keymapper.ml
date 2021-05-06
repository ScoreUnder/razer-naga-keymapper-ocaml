open MyLib

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

  let init_devices devices =
    ignore @@ Types.(Ioctl.(eviocgrab devices.keyboard.fd))

  let rec process_events dpy exec_state =
    let open Input in
    function
    | { evtype = EV_KEY (rawkey, presstype); _ } :: evs ->
        let key = if rawkey >= 275 then rawkey - 262 else rawkey - 1 in
        let next_exec_state =
          Execution.run_actions dpy exec_state presstype key
        in
        process_events dpy next_exec_state evs
    | _ :: evs -> process_events dpy exec_state evs
    | [] -> exec_state

  let rec wait_for_events devices dpy exec_state =
    match
      try Unix.select devices [] [] (-1.0)
      with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
    with
    | fd :: _, _, _ ->
        Input.read_some_input_events fd
        |> Array.to_list
        |> process_events dpy exec_state
        |> wait_for_events devices dpy
    | _, _, _exfd :: _ -> failwith "Exceptional condition in file descriptor?"
    | _ -> wait_for_events devices dpy exec_state

  let run devices ~keys_only dpy initial_keymap =
    init_devices devices;
    ProcessReaper.setup ();
    let open Types in
    let devices =
      if keys_only then [ devices.keyboard.fd ]
      else [ devices.keyboard.fd; devices.pointer.fd ]
    in
    wait_for_events devices dpy (Execution.initial_state initial_keymap)
end

let process_args () =
  let keys_only = ref false in
  let argspecs =
    [
      ( "-k",
        Arg.Set keys_only,
        "Use side keys only (no top buttons) - might reduce CPU usage" );
    ]
  in
  let keymap = ref None in
  let set_keymap path =
    if !keymap = None then keymap := Some path
    else
      raise_notrace
      @@ Arg.Bad "Only one keymap must be specified on the command line"
  in
  let usage_line = Sys.argv.(0) ^ " [options...] [keymap.txt]" in
  Arg.parse argspecs set_keymap usage_line;
  (!keymap, !keys_only)

let () =
  let config_path, keys_only = process_args () in
  let config_path = Option.value config_path ~default:NagaDaemon.config_path in
  let initial_keymap =
    try
      KeyMap.load config_path
      |> Result.map_error (fun err ->
             Execution.pp_keymap_load_failure Format.str_formatter config_path
               err;
             [ Format.flush_str_formatter () ])
    with Sys_error e -> Error [ "Could not load config file: " ^ e ]
  in
  let devices =
    NagaDaemon.(find_razer_device devices)
    |> Option.to_result
         ~none:
           [
             "No naga devices found or you don't have permission to access \
              them.";
           ]
  in
  let xdpy =
    X11.open_display () |> Option.to_result ~none:[ "Could not open display" ]
  in

  initial_keymap
  |> Result.iter
       (Format.printf "Loaded keymap from %S:@\n%a@\n%!" config_path KeyMap.pp);

  let open NagaDaemon.Types in
  let combined_result =
    let open Result.Syntax in
    let+ dpy = xdpy
    and* initial_keymap = initial_keymap
    and* devices = devices in
    Printf.printf "Taking input from: %S and %S\n%!" devices.keyboard.path
      devices.pointer.path;
    NagaDaemon.run devices ~keys_only dpy initial_keymap
  in

  devices
  |> Result.iter (fun devices ->
         Unix.close devices.keyboard.fd;
         Unix.close devices.pointer.fd);

  combined_result
  |> Result.iter_error (fun err ->
         List.iter prerr_endline err;
         exit 1)
