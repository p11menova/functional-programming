open Utils

(* tail recursion *)

let fib_exact_digits_tail_recursion n =
  if n <= 1 then 1
  else
    let rec aux idx a b = if has_exact_num_digits b n then idx else aux (succ idx) b (Z.add a b) in
    aux 2 Z.one Z.one

(* рекурсия но не сильно симпатичная потому что задача не прям очень рекурсивно решается*)
(* получить пару чисел фиббоначи F_i F_i-1*)
let rec get_fib_pair i =
  if i <= 1 then (Z.one, Z.zero) (* F_1 = 1, F_0 = 0 *)
  else
    let prev_fib, prev_prev_fib = get_fib_pair (i - 1) in
    let current_fib = Z.add prev_fib prev_prev_fib in
    (current_fib, prev_fib)

let fib_exact_digits_recursion n =
  let rec aux current_idx =
    let fib_i, _ = get_fib_pair current_idx in
    if has_exact_num_digits fib_i n then current_idx else aux (succ current_idx)
  in
  aux 1
