open Alcotest

let test_gcd () =
  check int "gcd(54,24)=6" 6 (Lab1.Utils.gcd 54 24) ;
  check int "gcd(5,0)=5" 5 (Lab1.Utils.gcd 5 0) ;
  check int "gcd(0,5)=5" 5 (Lab1.Utils.gcd 0 5) ;
  check int "gcd(17,31)=1" 1 (Lab1.Utils.gcd 17 31)

let test_lcm () =
  check int "lcm(0,7)=0" 0 (Lab1.Utils.lcm 0 7) ;
  check int "lcm(3,4)=12" 12 (Lab1.Utils.lcm 3 4) ;
  check int "lcm(12,15)=60" 60 (Lab1.Utils.lcm 12 15)

let test_lcm_range_tail_rec () =
  check int "LCM(1..10)=2520" 2520 (Lab1.Task5_recursion.lcm_from_one_to_target_tail_rec 10) ;
  check int "LCM(1..1)=1" 1 (Lab1.Task5_recursion.lcm_from_one_to_target_tail_rec 1) ;
  check int "LCM(1..20)=232792560" 232792560 (Lab1.Task5_recursion.lcm_from_one_to_target_tail_rec 20)

let test_lcm_range_rec () =
  check int "LCM(1..10)=2520" 2520 (Lab1.Task5_recursion.lcm_from_one_to_target_rec 10) ;
  check int "LCM(1..2)=2" 2 (Lab1.Task5_recursion.lcm_from_one_to_target_rec 2) ;
  check int "LCM(1..20)=232792560" 232792560 (Lab1.Task5_recursion.lcm_from_one_to_target_rec 20)

let test_lcm_range_fold () =
  check int "LCM(1..10)=2520" 2520 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_left 10) ;
  check int "LCM(1..2)=2" 2 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_left 2) ;
  check int "LCM(1..20)=232792560" 232792560 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_left 20) ;
  check int "LCM(1..1)=1" 1 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_left 1) ;
  check int "LCM(1..10)=2520" 2520 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_right 10) ;
  check int "LCM(1..2)=2" 2 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_right 2) ;
  check int "LCM(1..20)=232792560" 232792560 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_right 20) ;
  check int "LCM(1..1)=1" 1 (Lab1.Task5_recursion.lcm_from_one_to_target_fold_right 1)

let () =
  let open Alcotest in
  run "task5"
    [ ("gcd", [test_case "basic gcd" `Quick test_gcd])
    ; ("lcm", [test_case "basic lcm" `Quick test_lcm])
    ; ("lcm_range_tail_rec", [test_case "lcm(1..n) - tail recursion" `Quick test_lcm_range_tail_rec])
    ; ("lcm_range_rec", [test_case "lcm(1..n) - recursion" `Quick test_lcm_range_rec])
    ; ("lcm_range_fold", [test_case "lcm(1..n) - list generation + fold" `Quick test_lcm_range_fold]) ]
