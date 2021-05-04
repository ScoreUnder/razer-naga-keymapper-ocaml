open Input
open Operator
open MyLib

let pp_keymap_load_failure fmt path err =
  Format.fprintf fmt "Could not parse keymap at \"%a\":@\n%a"
    Format.pp_print_string path Parser.pp_error_list err

let fork_and_run cmd =
  Unix.(create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] stdin stdout stderr)
  |> ProcessReaper.register_for_reaping

let run_action dpy presstype ((keymap, state) as vars) = function
  | Chmap path -> (
      match KeyMap.load path with
      | Ok next_keymap ->
          print_endline @@ KeyMap.show next_keymap;
          (next_keymap, state)
      | Error err ->
          pp_keymap_load_failure Format.err_formatter path err;
          Format.pp_print_newline Format.err_formatter ();
          vars)
  | Key keysyms ->
      X11.press_key dpy presstype keysyms;
      vars
  | KeyTap keysyms ->
      X11.press_key dpy PRESS keysyms;
      X11.press_key dpy RELEASE keysyms;
      vars
  | Type keysyms ->
      X11.type_seq dpy keysyms;
      vars
  | Run cmd ->
      cmd |> fork_and_run;
      vars
  | Click mousebtn ->
      X11.click dpy presstype mousebtn;
      vars
  | Delay amt ->
      Unix.sleepf amt;
      vars
  | Toggle (n, keysyms) ->
      let is_pressed = IntMap.find_opt n state |> Option.value ~default:false in
      X11.press_key dpy (if is_pressed then RELEASE else PRESS) keysyms;
      let next_state = IntMap.add n (not is_pressed) state in
      (keymap, next_state)

let run_actions dpy vars presstype acts =
  List.fold_left (run_action dpy presstype) vars acts
