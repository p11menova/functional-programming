open Alcotest
open Task5_lcm.Task5_map
open Task5_lcm.Task5_recursion
open Task5_lcm.Task5_fold
open Task5_lcm.Task5_lazy

let test_lcm_range_tail_rec () =
  check int "LCM(1..10)=2520" 2520 (lcm_from_one_to_target_tail_rec 10) ;
  check int "LCM(1..1)=1" 1 (lcm_from_one_to_target_tail_rec 1) ;
  check int "LCM(1..20)=232792560" 232792560 (lcm_from_one_to_target_tail_rec 20)

let test_lcm_range_rec () =
  check int "LCM(1..10)=2520" 2520 (lcm_from_one_to_target_rec 10) ;
  check int "LCM(1..2)=2" 2 (lcm_from_one_to_target_rec 2) ;
  check int "LCM(1..20)=232792560" 232792560 (lcm_from_one_to_target_rec 20)

let test_lcm_range_fold () =
  check int "LCM(1..10)=2520" 2520 (lcm_from_one_to_target_fold_left 10) ;
  check int "LCM(1..2)=2" 2 (lcm_from_one_to_target_fold_left 2) ;
  check int "LCM(1..20)=232792560" 232792560 (lcm_from_one_to_target_fold_left 20) ;
  check int "LCM(1..1)=1" 1 (lcm_from_one_to_target_fold_left 1) ;
  check int "LCM(1..10)=2520" 2520 (lcm_from_one_to_target_fold_right 10) ;
  check int "LCM(1..2)=2" 2 (lcm_from_one_to_target_fold_right 2) ;
  check int "LCM(1..20)=232792560" 232792560 (lcm_from_one_to_target_fold_right 20) ;
  check int "LCM(1..1)=1" 1 (lcm_from_one_to_target_fold_right 1)

let test_lcm_range_map () =
  check int "LCM(1..1)=1" 1 (lcm_one_to_target_map 1) ;
  check int "LCM(1..10)=2520" 2520 (lcm_one_to_target_map 10) ;
  check int "LCM(1..2)=2" 2 (lcm_one_to_target_map 2) ;
  check int "LCM(1..20)=232792560" 232792560 (lcm_one_to_target_map 20)

let test_lcm_range_lazy () =
  check int "LCM(1..1)=1" 1 (lcm_from_one_to_target_lazy 1) ;
  check int "LCM(1..10)=2520" 2520 (lcm_from_one_to_target_lazy 10) ;
  check int "LCM(1..2)=2" 2 (lcm_from_one_to_target_lazy 2) ;
  check int "LCM(1..20)=232792560" 232792560 (lcm_from_one_to_target_lazy 20) ;
  check int "LCM(1..4)=12" 12 (lcm_from_one_to_target_lazy 4)

let () =
  let open Alcotest in
  run "task5"
    [ ("lcm_range_tail_rec", [test_case "lcm(1..n) - tail recursion" `Quick test_lcm_range_tail_rec])
    ; ("lcm_range_rec", [test_case "lcm(1..n) - recursion" `Quick test_lcm_range_rec])
    ; ("lcm_range_fold", [test_case "lcm(1..n) - list generation + fold" `Quick test_lcm_range_fold])
    ; ("lcm_range_map", [test_case "lcm(1..n) - filter + map" `Quick test_lcm_range_map])
    ; ( "lcm_range_lazy"
      , [test_case "lcm(1..n) - lazy iterator + inf list" `Quick test_lcm_range_lazy] ) ]
