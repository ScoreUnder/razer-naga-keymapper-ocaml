open Operator

let pp_keymap_load_failure fmt path err =
  Format.fprintf fmt "Could not parse keymap at \"%a\":@\n%a"
    Format.pp_print_string path Parser.pp_error_list err

let run_action keymap = function
  | Chmap, path ->
      KeyMap.load path
      |> Result.fold
           ~ok:(fun next_keymap -> next_keymap)
           ~error:(fun err ->
             pp_keymap_load_failure Format.err_formatter path err;
             prerr_newline ();
             keymap)
  | _ -> keymap

let rec run_actions keymap = function
  | x :: xs -> run_actions (run_action keymap x) xs
  | [] -> keymap
