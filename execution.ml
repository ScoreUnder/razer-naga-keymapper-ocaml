open Input
open Operator
open MyLib

let pp_keymap_load_failure fmt path err =
  Format.fprintf fmt "Could not parse keymap at \"%a\":@\n%a"
    Format.pp_print_string path Parser.pp_error_list err

let fork_and_run cmd =
  Unix.(create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] stdin stdout stderr)
  |> ProcessReaper.register_for_reaping

let run_action dpy (keymap, state) = function
  | Chmap, path, PRESS -> (
      match KeyMap.load path with
      | Ok next_keymap ->
          print_endline @@ KeyMap.show next_keymap;
          (next_keymap, state)
      | Error err ->
          pp_keymap_load_failure Format.err_formatter path err;
          prerr_newline ();
          (keymap, state))
  | Key, keyname, presstype ->
      X11.press_key dpy presstype keyname;
      (keymap, state)
  | Run2, cmd, _ | Run, cmd, PRESS ->
      cmd |> fork_and_run;
      (keymap, state)
  | Click, mousebtn, presstype ->
      X11.click dpy presstype mousebtn;
      (keymap, state)
  | Delay, amt, (PRESS|RELEASE) ->
      Unix.sleepf @@ (Float.of_string amt /. 1000.);
      (keymap, state)
  | Toggle n, key, PRESS ->
      let is_pressed = IntMap.find_opt n state |> Option.value ~default:false in
      X11.press_key dpy (if is_pressed then RELEASE else PRESS) key;
      let next_state = IntMap.add n (not is_pressed) state in
      (keymap, next_state)
  | _, _, (RELEASE | REPEAT) -> (keymap, state)

let rec run_actions dpy (keymap, state) presstype = function
  | (act, cmd) :: xs ->
      run_actions dpy
        (run_action dpy (keymap, state) (act, cmd, presstype))
        presstype xs
  | [] -> (keymap, state)
