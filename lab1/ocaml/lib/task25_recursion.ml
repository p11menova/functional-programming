open Utils
open Z

(* tail recursion *)
let fib_exact_digits_tail_recursion n =
  if n <= 1 then to_int Z.one
  else
    let rec aux idx a b =
      if has_exact_num_digits b n then to_int idx else aux (add idx Z.one) b (add a b)
    in
    aux (of_int 2) Z.one Z.one
