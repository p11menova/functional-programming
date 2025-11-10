open Lab1.Utils
open Alcotest

let test_gcd () =
  check int "gcd(54,24)=6" 6 (gcd 54 24) ;
  check int "gcd(5,0)=5" 5 (gcd 5 0) ;
  check int "gcd(0,5)=5" 5 (gcd 0 5) ;
  check int "gcd(17,31)=1" 1 (gcd 17 31)

let test_lcm () =
  check int "lcm(0,7)=0" 0 (lcm 0 7) ;
  check int "lcm(3,4)=12" 12 (lcm 3 4) ;
  check int "lcm(12,15)=60" 60 (lcm 12 15)

let pow_int a n = Z.pow (Z.of_int a) n

let test_has_exact_num_digits () =
  check bool "1 has 1000 digits = false" false (has_exact_num_digits (pow_int 1 0) 1000) ;
  check bool "100 has 3 digits = true" true (has_exact_num_digits (pow_int 100 1) 3) ;
  check bool "10^1000 has 1000 = true" true (has_exact_num_digits (pow_int 10 1000) 1000) ;
  check bool "10^999 has 1000 digits = true" true (has_exact_num_digits (pow_int 10 999) 1000) ;
  check bool "10^999 - 1 has 1000 digits = false" false
    (has_exact_num_digits (Z.sub (pow_int 10 999) Z.one) 1000)

let () =
  let open Alcotest in
  run "utils"
    [ ("gcd", [test_case "basic gcd" `Quick test_gcd])
    ; ("lcm", [test_case "basic lcm" `Quick test_lcm])
    ; ("has_1000_digits", [test_case "has 1000 digits" `Quick test_has_exact_num_digits]) ]
