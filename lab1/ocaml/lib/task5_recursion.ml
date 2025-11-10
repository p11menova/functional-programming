open Utils

(* хвостовая рекурсия *)
let lcm_from_one_to_target_tail_rec n =
  (* aux - вспомогательная внутр. функция ; aux (i - текущий элемент, acc - накопленное значение - НОК(1..i-1)) *)
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

(* генерация последовательности *)
(* range n is_reversed *)
let range n = function
  | true -> List.init n (fun i -> n - i) (* List.init length fun = [fun 0; fun 1; ... ; fun length]*)
  | false -> List.init n succ

(* свертка *)
(* List.fold_right: [a; b; c] -> f a (f b (f c init))

   List.fold_left: [a; b; c] -> f (f (f init a) b) c

   [3; 2; 1] -> lcm 3 (lcm 2 (lcm 1 1)) -> lcm 3 (lcm 2 1) -> lcm 3 2 -> 6 => fold_right fits

   [1; 2; 3] -> lcm (lcm ( lcm 1 1) 2) 3 -> lcm ( lcm 1 2) 3 -> lcm 2 3 -> 6 => fold_left fits 
*)

let lcm_from_one_to_target_fold_left n = List.fold_left lcm 1 (range n false)

let lcm_from_one_to_target_fold_right n = List.fold_right lcm (range n true) 1
