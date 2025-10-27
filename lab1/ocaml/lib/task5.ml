let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm a b = a / gcd a b * b

(* хвостовая рекурсия *)
let lcm_from_one_to_target_tail_rec n =
  (* aux - вспомогательная внутр. функция ; aux (i - текущий элемент, acc - накопленное значение -
     НОК(1..i-1)) *)
  let rec aux i acc = if i > n then acc else aux (i + 1) (lcm acc i) in
  aux 1 1

(* рекурсия *)
let rec lcm_from_one_to_target_rec n =
  if n = 1 then 1 else lcm n (lcm_from_one_to_target_rec (n - 1))
