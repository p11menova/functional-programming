open Alcotest

let test_gcd () =
  check int "gcd(54,24)=6" 6 (Task5.gcd 54 24) ;
  check int "gcd(5,0)=5" 5 (Task5.gcd 5 0) ;
  check int "gcd(0,5)=5" 5 (Task5.gcd 0 5) ;
  check int "gcd(17,31)=1" 1 (Task5.gcd 17 31)

let test_lcm () =
  check int "lcm(0,7)=0" 0 (Task5.lcm 0 7) ;
  check int "lcm(3,4)=12" 12 (Task5.lcm 3 4) ;
  check int "lcm(12,15)=60" 60 (Task5.lcm 12 15)

let test_lcm_range_tail_rec () =
  check int "LCM(1..10)=2520" 2520 (Task5.lcm_from_one_to_target_tail_rec 10) ;
  check int "LCM(1..1)=1" 1 (Task5.lcm_from_one_to_target_tail_rec 1) ;
  check int "LCM(1..20)=232792560" 232792560 (Task5.lcm_from_one_to_target_tail_rec 20)

let () =
  let open Alcotest in
  run "task5"
    [ ("gcd", [test_case "basic gcd" `Quick test_gcd])
    ; ("lcm", [test_case "basic lcm" `Quick test_lcm])
    ; ("lcm_range", [test_case "lcm(1..n) - tail recursion" `Quick test_lcm_range_tail_rec]) ]
