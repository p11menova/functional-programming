open Utils

(* хвостовая рекурсия *)
let lcm_from_one_to_target_tail_rec n =
  (* aux - вспомогательная внутр. функция ; aux (i - текущий элемент, acc - накопленное значение -
     НОК(1..i-1)) *)
  let rec aux i acc =
    match i with
    | x when x > n -> acc
    | _ -> aux (i + 1) (lcm acc i)
  in
  aux 1 1

(* рекурсия *)
let rec lcm_from_one_to_target_rec = function
  | 1 -> 1
  | n -> lcm n (lcm_from_one_to_target_rec (n - 1))

(* let range n = Seq.(1 -- n |> List.of_seq) я хотела так но у меня не поддерживается оператор -- *)
