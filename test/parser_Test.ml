open TestTypes

let tests =
  ( "Parser",
    [
      Alcotest.test_case
        "parse_error: multiple invalid unicode characters (single line parse)"
        `Quick (fun () ->
          "1 - type = abcdef\xCCghijklm\xD0nopqrstuv\xFFwxyz"
          |> Parser.parse_conf_line 0
          |> Alcotest.(
               check
                 (option
                    (result
                       (pair (pair (list keypress) int) operator)
                       (list parse_error))))
               "keymap"
               (Some
                  (Error
                     [
                       Parser.BadUnicode (1, "\xCCg");
                       Parser.BadUnicode (1, "\xD0n");
                       Parser.BadUnicode (1, "\xFF");
                     ])));
    ] )
