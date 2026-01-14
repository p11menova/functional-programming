type point = float * float

let parse_point line =
  try
    let line = String.trim line in
    if line = "" then
      None
    else
      let parts =
        if String.contains line ';' then
          String.split_on_char ';' line
        else if String.contains line '\t' then
          String.split_on_char '\t' line
        else
          String.split_on_char ' ' line
      in
      match parts with
      | [ x_str; y_str ] ->
          let x = float_of_string (String.trim x_str) in
          let y = float_of_string (String.trim y_str) in
          Some (x, y)
      | _ -> None
  with
  | _ -> None


let format_point algorithm_name (x, y) = Printf.sprintf "%s: %.4f %.4f" algorithm_name x y

(** генератор точек для интерполяции на основе шага *)
let generate_points x_min x_max step =
  let rec gen acc current =
    if current > x_max +. 0.0001 then
      List.rev acc
    else
      gen (current :: acc) (current +. step)
  in
  gen [] x_min


(** генератор точек для интерполяции на основе количества *)
let generate_n_points x_min x_max n =
  if n <= 1 then
    [ x_min ]
  else
    let step = (x_max -. x_min) /. float_of_int (n - 1) in
    List.init n (fun i -> x_min +. (float_of_int i *. step))
