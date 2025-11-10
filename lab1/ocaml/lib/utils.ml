let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)

let lcm a b = a / gcd a b * b

open Z

(* проверка на >= 1000 цифр для Z.t *)
let has_exact_num_digits z n = String.length (to_string z) >= n
