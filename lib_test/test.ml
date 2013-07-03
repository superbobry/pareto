open OUnit

let all () = TestList [
    Tests_test.test;
    Sample_test.test;
    Distributions_test.test
  ]
