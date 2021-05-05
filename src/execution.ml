open Input
open Operator
open MyLib

type exec_state = { keymap : KeyMap.t; toggles : IntSet.t }

let initial_state keymap = { keymap; toggles = IntSet.empty }

let pp_keymap_load_failure fmt path err =
  Format.fprintf fmt "Could not parse keymap at \"%a\":@\n%a"
    Format.pp_print_string path Parser.pp_error_list err

let fork_and_run cmd =
  Unix.(create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] stdin stdout stderr)
  |> ProcessReaper.register_for_reaping

let run_action dpy presstype vars = function
  | Chmap path -> (
      try
        match KeyMap.load path with
        | Ok next_keymap ->
            Format.printf "Loaded keymap from %S:@\n%a@\n%!" path KeyMap.pp
              next_keymap;
            { vars with keymap = next_keymap }
        | Error err ->
            pp_keymap_load_failure Format.err_formatter path err;
            Format.pp_print_newline Format.err_formatter ();
            vars
      with Sys_error e ->
        prerr_endline ("Could not load config file: " ^ e);
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
      let is_pressed = IntSet.mem n vars.toggles in
      X11.press_key dpy (if is_pressed then RELEASE else PRESS) keysyms;
      let next_toggles =
        IntSet.(if is_pressed then remove else add) n vars.toggles
      in
      { vars with toggles = next_toggles }

let run_actions dpy vars presstype btn =
  let acts = KeyMap.find (presstype, btn) vars.keymap in
  List.fold_left (run_action dpy presstype) vars acts
