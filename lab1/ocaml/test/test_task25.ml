open Alcotest
open Task25_fib.Task25_recursion
open Task25_fib.Task25_mix

let test_fib_exact_digits_tail_recursion () =
  check int "1 digit -> index 1" 1 (fib_exact_digits_tail_recursion 1) ;
  check int "2 digits -> index 7" 7 (fib_exact_digits_tail_recursion 2) ;
  check int "3 digits -> index 12" 12 (fib_exact_digits_tail_recursion 3) ;
  check int "4 digits -> index 17" 17 (fib_exact_digits_tail_recursion 4) ;
  check int "5 digits -> index 21" 21 (fib_exact_digits_tail_recursion 5) ;
  check int "1000 digits -> index 4782" 4782 (fib_exact_digits_tail_recursion 1000)

let test_fib_exact_digits_recursion () =
  check int "1 digit -> index 1" 1 (fib_exact_digits_recursion 1) ;
  check int "2 digits -> index 7" 7 (fib_exact_digits_recursion 2) ;
  check int "3 digits -> index 12" 12 (fib_exact_digits_recursion 3) ;
  check int "4 digits -> index 17" 17 (fib_exact_digits_recursion 4) ;
  check int "5 digits -> index 21" 21 (fib_exact_digits_recursion 5) ;
  check int "1000 digits -> index 4782" 4782 (fib_exact_digits_recursion 1000)

let test_fib_exact_digits_modules () =
  check int "1 digit -> index 1" 1 (fib_exact_digits_modules 1) ;
  check int "2 digits -> index 7" 7 (fib_exact_digits_modules 2) ;
  check int "3 digits -> index 12" 12 (fib_exact_digits_modules 3) ;
  check int "4 digits -> index 17" 17 (fib_exact_digits_modules 4) ;
  check int "5 digits -> index 21" 21 (fib_exact_digits_modules 5) ;
  check int "1000 digits -> index 4782" 4782 (fib_exact_digits_modules 1000)

let () =
  let open Alcotest in
  run "task25"
    [ ( "fib_exact_digits_tail_recursion"
      , [test_case "tail recursion" `Quick test_fib_exact_digits_tail_recursion] )
    ; ("fib_exact_digits_recursion", [test_case "recursion" `Quick test_fib_exact_digits_recursion])
    ; ("fib_exact_digits_modules", [test_case "modules" `Quick test_fib_exact_digits_modules]) ]
