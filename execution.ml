open Input
open Operator
open MyLib

let pp_keymap_load_failure fmt path err =
  Format.fprintf fmt "Could not parse keymap at \"%a\":@\n%a"
    Format.pp_print_string path Parser.pp_error_list err

let system cmd =
  print_endline cmd;
  Unix.system cmd

let fork_and_run cmd =
  if Unix.fork () = 0 then (
    ignore @@ system cmd;
    Unix._exit 0
  )

let press_key presstype key =
  let key_invocation =
    let open Input in
    match presstype with
    | PRESS -> Some "xdotool keydown -- "
    | RELEASE -> Some "xdotool keyup -- "
    | REPEAT -> None
  in
  key_invocation |> Option.iter (fun cmd -> cmd ^ key |> system |> ignore)

let click presstype key =
  let key_invocation =
    let open Input in
    match presstype with
    | PRESS -> Some "xdotool mousedown -- "
    | RELEASE -> Some "xdotool mouseup -- "
    | REPEAT -> None
  in
  key_invocation |> Option.iter (fun cmd -> cmd ^ key |> system |> ignore)

let run_action (keymap, state) = function
  | Chmap, path, PRESS ->
      KeyMap.load path
      |> Result.fold
           ~ok:(fun next_keymap -> (next_keymap, state))
           ~error:(fun err ->
             pp_keymap_load_failure Format.err_formatter path err;
             prerr_newline ();
             (keymap, state))
  | Key, keyname, presstype ->
      press_key presstype keyname;
      (keymap, state)
  | Run2, cmd, _ | Run, cmd, PRESS ->
      cmd |> fork_and_run;
      (keymap, state)
  | Click, mousebtn, presstype ->
      click presstype mousebtn;
      (keymap, state)
  | Workspace, name, PRESS ->
      "xdotool set_desktop -- " ^ name |> system |> ignore;
      (keymap, state)
  | WorkspaceR, name, PRESS ->
      "xdotool set_desktop --relative -- " ^ name |> system |> ignore;
      (keymap, state)
  | Position, pos, PRESS ->
      "xdotool mousemove -- " ^ pos |> system |> ignore;
      (keymap, state)
  | Media, key, presstype ->
      press_key presstype ("XF86" ^ key);
      (keymap, state)
  | Delay, amt, presstype ->
      Unix.sleepf @@ (Float.of_string amt /. 1000.);
      (keymap, state)
  | Toggle n, key, PRESS ->
      let is_pressed = IntMap.find_opt n state |> Option.value ~default:false in
      press_key (if is_pressed then RELEASE else PRESS) key;
      let next_state = IntMap.add n (not is_pressed) state in
      (keymap, next_state)
  | _, _, (RELEASE | REPEAT) -> (keymap, state)

let rec run_actions (keymap, state) presstype = function
  | (act, cmd) :: xs ->
      run_actions
        (run_action (keymap, state) (act, cmd, presstype))
        presstype xs
  | [] -> (keymap, state)
