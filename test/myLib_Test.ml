open MyLib

let tests =
  ( "MyLib",
    [
      Alcotest.test_case "find_with_map_opt (present)" `Quick (fun () ->
          [ 2; 4; 6; 8; 9; 10; 12; 14 ]
          |> List.find_map_opt (fun v ->
                 if v mod 2 = 0 then None else Some (v * 3))
          |> Alcotest.(check (option int)) "result" (Some 27));
      Alcotest.test_case "find_with_map_opt (absent)" `Quick (fun () ->
          [ 2; 4; 6; 8; 10; 12; 14 ]
          |> List.find_map_opt (fun v ->
                 if v mod 2 = 0 then None else Some (v * 3))
          |> Alcotest.(check (option int)) "result" None);
    ] )
