(** Модуль для парсинга аргументов командной строки *)

type algorithm =
  | Linear
  | Lagrange

type config = {
    algorithms : algorithm list
  ; step : float option
  ; n_points : int option
  ; window_size : int
}

let default_config = { algorithms = []; step = Some 0.2; n_points = None; window_size = 4 }

let parse_args argv =
  let rec parse acc = function
    | [] -> acc
    | "--linear" :: rest -> parse { acc with algorithms = Linear :: acc.algorithms } rest
    | "--lagrange" :: rest -> parse { acc with algorithms = Lagrange :: acc.algorithms } rest
    | "--step" :: step_str :: rest -> parse { acc with step = Some (float_of_string step_str) } rest
    | "-s" :: step_str :: rest -> parse { acc with step = Some (float_of_string step_str) } rest
    | "--n" :: n_str :: rest -> parse { acc with n_points = Some (int_of_string n_str) } rest
    | "-n" :: n_str :: rest -> parse { acc with n_points = Some (int_of_string n_str) } rest
    | "--window" :: w_str :: rest -> parse { acc with window_size = int_of_string w_str } rest
    | "-w" :: w_str :: rest -> parse { acc with window_size = int_of_string w_str } rest
    | arg :: rest ->
        Printf.eprintf "Неизвестный аргумент: %s\n" arg;
        parse acc rest
  in
  let config = parse default_config argv in
  { config with algorithms = List.rev config.algorithms }
