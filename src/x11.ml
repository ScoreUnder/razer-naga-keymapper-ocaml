type display

external open_display : unit -> display option = "caml_xopen_display"

external flush : display -> int = "caml_xflush" [@@noalloc]

external fake_button : display -> int -> Input.keypress -> int
  = "caml_xtest_fake_button"
  [@@noalloc]

external fake_key : display -> int -> Input.keypress -> int
  = "caml_xtest_fake_key"
  [@@noalloc]

external string_to_keysym : string -> int = "caml_string_to_keysym" [@@noalloc]

external keysym_to_keycode : display -> int -> int = "caml_keysym_to_keycode"
  [@@noalloc]

exception Bad_key_name of string

exception Unbound_key of string

(* xdotool-compatible key aliases *)
let key_aliases =
  [
    ("alt", "Alt_L");
    ("control", "Control_L");
    ("ctrl", "Control_L");
    ("meta", "Meta_L");
    ("shift", "Shift_L");
    ("super", "Super_L");
  ]

let parse_keysym s =
  let s = String.trim s in
  let s =
    List.assoc_opt (String.lowercase_ascii s) key_aliases
    |> Option.value ~default:s
  in
  let sym = string_to_keysym s in
  if sym = 0 then try int_of_string s with Failure _ -> raise (Bad_key_name s)
  else sym

let parse_keysyms s =
  s |> String.split_on_char '+'
  |> List.map (fun name -> (name, parse_keysym name))

(* TODO: this will have the same bug that xdotool had in the past:
   https://web.archive.org/web/20160617092529/https://code.google.com/p/semicomplete/issues/detail?id=13
*)
let keysyms_to_keycodes dpy syms =
  List.map
    (fun (name, sym) ->
      let code = keysym_to_keycode dpy sym in
      if code = 0 then raise (Unbound_key name) else code)
    syms

let press_key dpy presstype key =
  try
    keysyms_to_keycodes dpy key
    |> List.iter (fun k -> fake_key dpy k presstype |> ignore);
    flush dpy |> ignore
  with Unbound_key k -> prerr_endline @@ "Unbound key " ^ k

let click dpy presstype key =
  fake_button dpy key presstype |> ignore;
  flush dpy |> ignore

let type_seq dpy key =
  try
    keysyms_to_keycodes dpy key
    |> List.iter (fun k ->
           fake_key dpy k PRESS |> ignore;
           fake_key dpy k RELEASE |> ignore);
    flush dpy |> ignore
  with Unbound_key k -> prerr_endline @@ "Unbound key " ^ k
