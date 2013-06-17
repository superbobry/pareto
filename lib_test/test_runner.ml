let () =
  let open OUnit in
  ignore (run_test_tt_main (Test.all ()))
