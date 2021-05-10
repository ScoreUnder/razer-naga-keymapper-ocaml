let parse_error = Alcotest.testable Parser.pp_parse_error ( = )

let keymap = Alcotest.testable KeyMap.pp ( = )

let keypress = Alcotest.testable Input.pp_keypress ( = )

let operator = Alcotest.testable Operator.pp ( = )
