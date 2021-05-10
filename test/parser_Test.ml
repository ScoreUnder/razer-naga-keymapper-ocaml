open TestTypes

let parse_result =
  Alcotest.(
    option
      (result (pair (pair (list keypress) int) operator) (list parse_error)))

let test_err name err map =
  Alcotest.test_case name `Quick (fun () ->
      map |> Parser.parse_conf_line 0
      |> Alcotest.check parse_result "keymap" (Some (Error err)))

let test_ok name ok map =
  Alcotest.test_case name `Quick (fun () ->
      map |> Parser.parse_conf_line 0
      |> Alcotest.check parse_result "keymap" (Some (Ok ok)))

let test_ok_simple name ok = test_ok name (([ Input.PRESS ], 1), ok)

let tests =
  ( "Parser",
    [
      "1 - type = abcdef\xCCghijklm\xD0nopqrstuv\xFFwxyz"
      |> test_err
           "parse_error: multiple invalid unicode characters (single line \
            parse)"
           Parser.
             [
               BadUnicode (1, "\xCCg");
               BadUnicode (1, "\xD0n");
               BadUnicode (1, "\xFF");
             ];
      "1-type=Hello World"
      |> test_ok_simple "type character ordering"
         @@ Operator.Type
              [
                ("U0048", 0x48);
                ("U0065", 0x65);
                ("U006c", 0x6c);
                ("U006c", 0x6c);
                ("U006f", 0x6f);
                ("U0020", 0x20);
                ("U0057", 0x57);
                ("U006f", 0x6f);
                ("U0072", 0x72);
                ("U006c", 0x6c);
                ("U0064", 0x64);
              ];
      "1-keytap = ctrl+alt+Delete"
      |> test_ok_simple "key chord"
         @@ Operator.KeyTap
              [ ("ctrl", 0xffe3); ("alt", 0xffe9); ("Delete", 0xffff) ];
      "1 - run = executable file"
      |> test_ok_simple "run" @@ Operator.Run "executable file";
      "1 - runwait = another executable"
      |> test_ok_simple "runwait" @@ Operator.RunWait "another executable";
      "1 - runinvalid = something"
      |> test_err "invalid command"
           Parser.[ UnknownOperation (1, "runinvalid") ];
      "1 - chmap = file.txt"
      |> test_ok_simple "chmap" @@ Operator.Chmap "file.txt";
      "1 press - delay = 1234.5"
      |> test_ok_simple "delay" @@ Operator.Delay 1.2345;
      "1 - delay = 123hello"
      |> test_err "bad number in delay" Parser.[ BadNumber (1, "123hello") ];
      "1 - key = hyperdrive"
      |> test_err "bad key name" Parser.[ BadKeyName (1, "hyperdrive") ];
    ] )
