let () =
  Alcotest.run "Razer Naga Keymapper" [ KeyMap_Test.tests; Parser_Test.tests ]
