open Utils

(* ленивая генерация пос-ти фиб *)
(* Seq.unfold функция-генератор старт_пос-ти *)
(* функция-генератор -> (1, (1, 2)) - текущий элем и след значения состояния (откуда будет генерироваться пос-ть)*)
let fib_generator = Seq.unfold (fun (a, b) -> Some (a, (b, Z.add a b))) (Z.one, Z.one)

(* добавляем к числам фиб их индексы (начиная с 1) *)
let index_fib_sequence = Seq.mapi (fun i fib_val -> (i + 1, fib_val)) fib_generator

let fib_exact_digits_modules (n : int) =
  match Seq.find (fun (_idx, value) -> has_exact_num_digits value n) index_fib_sequence with
  | Some (idx, _) -> idx
  | None -> -1
