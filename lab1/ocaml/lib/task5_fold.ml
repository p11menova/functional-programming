open Utils

(* генерация последовательности *)
(* range n is_reversed *)
(* List.init length fun = [fun 0; fun 1; ... ; fun length]*)

let range n = function
  | true -> List.init n (fun i -> n - i)
  | false -> List.init n succ

(* свертка *)
(* List.fold_right: [a; b; c] -> f a (f b (f c init))

   List.fold_left: [a; b; c] -> f (f (f init a) b) c

   [3; 2; 1] -> lcm 3 (lcm 2 (lcm 1 1)) -> lcm 3 (lcm 2 1) -> lcm 3 2 -> 6 => fold_right fits

   [1; 2; 3] -> lcm (lcm ( lcm 1 1) 2) 3 -> lcm ( lcm 1 2) 3 -> lcm 2 3 -> 6 => fold_left fits 
*)

let lcm_from_one_to_target_fold_left n = List.fold_left lcm 1 (range n false)

let lcm_from_one_to_target_fold_right n = List.fold_right lcm (range n true) 1
