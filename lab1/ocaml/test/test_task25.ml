open Alcotest
open Lab1.Task25_recursion

let test_fib_exact_digits_tail_recursion () =
  check int "1 digit -> index 1" 1 (fib_exact_digits_tail_recursion 1) ;
  check int "2 digits -> index 7" 7 (fib_exact_digits_tail_recursion 2) ;
  check int "3 digits -> index 12" 12 (fib_exact_digits_tail_recursion 3) ;
  check int "4 digits -> index 17" 17 (fib_exact_digits_tail_recursion 4) ;
  check int "5 digits -> index 21" 21 (fib_exact_digits_tail_recursion 5) ;
  check int "1000 digits -> index 4782" 4782 (fib_exact_digits_tail_recursion 1000)

let () =
  let open Alcotest in
  run "task25"
    [ ( "fib_exact_digits_tail_recursion"
      , [test_case "tail recursion" `Quick test_fib_exact_digits_tail_recursion] ) ]
