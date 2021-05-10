open TestTypes

let keymap_of_string str =
  str |> Gen.of_string |> Gen.lines |> KeyMap.load_from_gen

let keymap_to_string =
  Alcotest.(pp (result keymap error_list)) |> Fmt.to_to_string

let test_parsing name expected content =
  Alcotest.test_case name `Quick (fun () ->
      content |> keymap_of_string
      |> Alcotest.(check @@ result keymap error_list) "keymap" expected)

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
              Parser.
                [
                  MissingEquals 1;
                  MissingHyphen 2;
                  UnknownOperation (3, "bad1bad");
                  BadNumber (4, "bad2bad");
                  BadKeyName (5, "bad3bad");
                  BadKeypressType (6, "bad4bad");
                  BadUnicode (7, "\xE5");
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
              Parser.
                [
                  BadUnicode (1, "\xCCg");
                  BadUnicode (1, "\xD0n");
                  BadUnicode (1, "\xFF");
                ]);
      Alcotest.test_case "default activation for Key" `Quick (fun () ->
          let map = keymap_of_string {|1 - key = a|} |> Result.get_ok in
          let check typ name result =
            map
            |> KeyMap.find (typ, 1)
            |> Alcotest.(check (list operator)) name result
          in
          let open Input in
          check PRESS "key press" [ Operator.Key [ ("a", 0x61) ] ];
          check RELEASE "key release" [ Operator.Key [ ("a", 0x61) ] ];
          check REPEAT "key repeat" [ Operator.Key [ ("a", 0x61) ] ]);
      Alcotest.test_case "default activation for KeyTap" `Quick (fun () ->
          let map = keymap_of_string {|1 - keytap = a|} |> Result.get_ok in
          let check typ name result =
            map
            |> KeyMap.find (typ, 1)
            |> Alcotest.(check (list operator)) name result
          in
          let open Input in
          check PRESS "keytap press" [ Operator.KeyTap [ ("a", 0x61) ] ];
          check RELEASE "keytap release" [];
          check REPEAT "keytap repeat" []);
      Alcotest.test_case "instruction ordering" `Quick (fun () ->
          let map =
            keymap_of_string
              {|
                1 press release repeat-keytap=a
                1       release repeat-keytap=b
                2 press release repeat-keytap=c
                1 press               -keytap=d
                2 press         repeat-keytap=e
                1 press release repeat-keytap=f
              |}
            |> Result.get_ok
          in
          let check typ btn name result =
            map
            |> KeyMap.find (typ, btn)
            |> Alcotest.(check (list operator)) name result
          in
          let open Input in
          let open Operator in
          check PRESS 1 "press 1"
            [
              KeyTap [ ("a", 0x61) ];
              KeyTap [ ("d", 0x64) ];
              KeyTap [ ("f", 0x66) ];
            ];
          check RELEASE 1 "release 1"
            [
              KeyTap [ ("a", 0x61) ];
              KeyTap [ ("b", 0x62) ];
              KeyTap [ ("f", 0x66) ];
            ];
          check REPEAT 1 "repeat 1"
            [
              KeyTap [ ("a", 0x61) ];
              KeyTap [ ("b", 0x62) ];
              KeyTap [ ("f", 0x66) ];
            ];
          check PRESS 2 "press 2"
            [ KeyTap [ ("c", 0x63) ]; KeyTap [ ("e", 0x65) ] ];
          check RELEASE 2 "release 2" [ KeyTap [ ("c", 0x63) ] ];
          check REPEAT 2 "repeat 2"
            [ KeyTap [ ("c", 0x63) ]; KeyTap [ ("e", 0x65) ] ]);
      Alcotest.test_case "renumbered toggles" `Quick (fun () ->
          let map =
            keymap_of_string
              {|
                1-toggle=a
                1 press release -toggle=b
                2-toggle=a
              |}
            |> Result.get_ok
          in
          let check typ btn name result =
            map
            |> KeyMap.find (typ, btn)
            |> Alcotest.(check (list operator)) name result
          in
          let open Input in
          let open Operator in
          check PRESS 1 "press 1"
            [ Toggle (2, [ ("a", 0x61) ]); Toggle (1, [ ("b", 0x62) ]) ];
          check RELEASE 1 "release 1" [ Toggle (1, [ ("b", 0x62) ]) ];
          check PRESS 2 "press 2" [ Toggle (0, [ ("a", 0x61) ]) ]);
      Alcotest.test_case "full keymap with show" `Quick (fun () ->
          {|1 - chmap = elsewhere
            2 - click = 8
            3 - click = 9
            4 - key = ctrl+shift
            4 - keytap = k
            5 - type = @
            6 - run = gedit
            7 repeat press - run = echo hi
            7 release - runwait = sleep 2
            8 - delay = 1
            9 - toggle = k
            9 press release - toggle = k|}
          |> keymap_of_string |> keymap_to_string
          |> Alcotest.(check string)
               "show"
               {|Ok
  {
    (RELEASE, 2): [(Click 8)],
    (RELEASE, 3): [(Click 9)],
    (RELEASE, 4): [(Key [("ctrl", 65507); ("shift", 65505)])],
    (RELEASE, 7): [(RunWait "sleep 2")],
    (RELEASE, 8): [(Delay 0.001)],
    (RELEASE, 9): [(Toggle (0, [("k", 107)]))],
    (PRESS, 1): [(Chmap "elsewhere")],
    (PRESS, 2): [(Click 8)],
    (PRESS, 3): [(Click 9)],
    (PRESS, 4):
    [(Key [("ctrl", 65507); ("shift", 65505)]); (KeyTap [("k", 107)])],
    (PRESS, 5): [(Type [("U0040", 64)])],
    (PRESS, 6): [(Run "gedit")],
    (PRESS, 7): [(Run "echo hi")],
    (PRESS, 8): [(Delay 0.001)],
    (PRESS, 9): [(Toggle (1, [("k", 107)])); (Toggle (0, [("k", 107)]))],
    (REPEAT, 2): [(Click 8)],
    (REPEAT, 3): [(Click 9)],
    (REPEAT, 4): [(Key [("ctrl", 65507); ("shift", 65505)])],
    (REPEAT, 7): [(Run "echo hi")]
  }|});
      Alcotest.test_case "broken keymap with show" `Quick (fun () ->
          {|1 - click = 3
            2 - click = 5
            3 - ccccc = 3
            - 3-4=4
            30
            3rr- run=run
            1 - run = hi
            x-x
            1-key=invalidkey
            2 adfs-key=1
            2-type=|}
          ^ "\221"
          |> keymap_of_string |> keymap_to_string
          |> Alcotest.(check string)
               "show"
               {|Error
  [(UnknownOperation (3, "ccccc")); (BadNumber (4, ""));
    (UnknownOperation (4, "3-4")); (MissingHyphen 5); (BadNumber (6, "3rr"));
    (BadNumber (8, "x")); (MissingEquals 8); (BadKeyName (9, "invalidkey"));
    (BadKeypressType (10, "adfs")); (BadUnicode (11, "\221"))]|});
    ] )
