(* идея: НОК(1..n) = произв. всех простых чисел <= n в их максимальной (k: p^k <= n) степени

   сначала генерируем список [1..n] потом прореживаем его через фильтр is_prime потом для каждого
   простого находим макси степень - map через combine перемножаем *)

let is_prime n =
  if n <= 1 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else
    let rec aux i = if i * i > n then true else if n mod i = 0 then false else aux (i + 2) in
    aux 3

(* List.init length fun = [fun 0; fun 1; ... ; fun length]*)
let range n = function
  | true -> List.init n (fun i -> n - i)
  | false -> List.init n succ

(* if p^cur > n <-> p^(cur+1) > n, than cur is enough*)
let max_power p n =
  let rec aux cur_power = if cur_power > n / p then cur_power else aux (cur_power * p) in
  aux p

let lcm_one_to_target_map n =
  range n true |> List.filter is_prime
  |> List.map (fun p -> max_power p n)
  |> List.fold_left ( * ) 1

(* либо так *)
let lcm_one_to_target_map_alt n =
  List.fold_left ( * ) 1
  @@ List.map (fun p -> max_power p n)
  @@ List.filter is_prime @@ range n true
