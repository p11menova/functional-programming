open Z

(* проверка на >= 1000 цифр для Z.t *)
let has_exact_num_digits z n = String.length (to_string z) >= n
