let () =
  let open Task5 in
  let n10 = lcm_from_one_to_target_tail_rec 10 in
  let n20 = lcm_from_one_to_target_tail_rec 20 in
  Printf.printf "LCM(1..10) = %d\n" n10 ;
  Printf.printf "LCM(1..20) = %d\n" n20
