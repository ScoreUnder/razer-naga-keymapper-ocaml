open TestTypes

let keymap_of_string str =
  str |> Gen.of_string |> Gen.lines |> KeyMap.load_from_gen

let test_parsing name expected content =
  Alcotest.test_case name `Quick (fun () ->
      content |> keymap_of_string
      |> Alcotest.(check @@ result keymap (list parse_error)) "keymap" expected)

let tests =
  ( "KeyMap",
    [
      {|    # empty file
    
      # ... no content|}
      |> test_parsing "empty map" (Ok KeyMap.empty);
      {|1 click = 2|}
      |> test_parsing "parse error: missing hyphen"
           (Error [ Parser.MissingHyphen 1 ]);
      {|
      
      
      
      1 click = 2|}
      |> test_parsing "parse error line number"
           (Error [ Parser.MissingHyphen 5 ]);
      {|1 - click 2|}
      |> test_parsing "parse error: missing equals"
           (Error [ Parser.MissingEquals 1 ]);
      {|1 - clicc = 2|}
      |> test_parsing "parse error: bad operator"
           (Error [ Parser.UnknownOperation (1, "clicc") ]);
      {|one - click = 2|}
      |> test_parsing "parse error: invalid number"
           (Error [ Parser.BadNumber (1, "one") ]);
      {|1 - keytap = invalidkey|}
      |> test_parsing "parse error: invalid key name"
           (Error [ Parser.BadKeyName (1, "invalidkey") ]);
      {|1 invalid - keytap = x|}
      |> test_parsing "parse error: invalid keypress type"
           (Error [ Parser.BadKeypressType (1, "invalid") ]);
      "1 - type = \xE0"
      |> test_parsing "parse error: invalid unicode"
           (Error [ Parser.BadUnicode (1, "\xE0") ]);
      {|1 - click 2
        1 click = 2
        1 - bad1bad = 3
        bad2bad - click = 3
        1 - keytap = bad3bad
        1 bad4bad - click = 3
        1 - type = |}
      ^ "\xE5"
      |> test_parsing "parse error: multiple errors all reported"
           (Error
              [
                Parser.MissingEquals 1;
                Parser.MissingHyphen 2;
                Parser.UnknownOperation (3, "bad1bad");
                Parser.BadNumber (4, "bad2bad");
                Parser.BadKeyName (5, "bad3bad");
                Parser.BadKeypressType (6, "bad4bad");
                Parser.BadUnicode (7, "\xE5");
              ]);
      {|1 - click = 1
        2 - click = 2
        3 - click = 3
        4 - click = boom|}
      |> test_parsing "parse error after non-errors"
           (Error [ Parser.BadNumber (4, "boom") ]);
      "1 - type = abcdef\xCCghijklm\xD0nopqrstuv\xFFwxyz"
      |> test_parsing
           "parse error: multiple invalid unicode characters in larger string"
           (Error
              [
                Parser.BadUnicode (1, "\xCCg");
                Parser.BadUnicode (1, "\xD0n");
                Parser.BadUnicode (1, "\xFF");
              ]);
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
