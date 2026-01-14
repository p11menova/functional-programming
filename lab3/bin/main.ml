module Args = Lab3.Args
module Window = Lab3.Window
module Stream = Lab3.Stream
module Interpolation = Lab3.Interpolation

let process_point (config : Args.config) window point algorithm_name interpolate_func is_linear =
  let new_window = Window.add_point window point in
  let points = Window.get_points new_window in
  let n_points = List.length points in

  match Window.get_x_range new_window with
  | None -> (new_window, [])
  | Some (x_min, x_max) ->
      (* дляя линейной : генерируем точки в последнем сегменте *)
      (* для лагранда: генерируем одну точку в центре, если окно заполнено *)
      let target_xs =
        if is_linear && n_points >= 2 then
          let sorted = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) points in
          let (x1, _) = List.nth sorted (n_points - 2) in
          let (x2, _) = List.nth sorted (n_points - 1) in
          let is_first_segment = n_points = 2 in
          match (config.step, config.n_points) with
          | (Some step, _) ->
              if is_first_segment then
                Stream.generate_points x1 x2 step
              else
                let start = x1 +. step in
                if start <= x2 then Stream.generate_points start x2 step else [ x2 ]
          | (None, Some n) ->
              if is_first_segment then
                Stream.generate_n_points x1 x2 n
              else
                let step = (x2 -. x1) /. float_of_int (n + 1) in
                List.init n (fun i -> x1 +. (float_of_int (i + 1) *. step))
          | (None, None) -> if is_first_segment then [ x1; x2 ] else [ x2 ]
        else if (not is_linear) && Window.is_full new_window then
          let center_x = (x_min +. x_max) /. 2.0 in
          [ center_x ]
        else if (not is_linear) && n_points >= config.window_size then
          let center_x = (x_min +. x_max) /. 2.0 in
          [ center_x ]
        else
          []
      in

      let results =
        List.fold_left
          (fun acc x ->
            match interpolate_func points x with
            | Some y -> (x, y) :: acc
            | None -> acc )
          []
          target_xs
        |> List.rev
      in

      let output_lines = List.map (Stream.format_point algorithm_name) results in
      (new_window, output_lines)


let process_linear config window point =
  process_point config window point "linear" Interpolation.linear_interpolation true


let process_lagrange config window point =
  process_point config window point "lagrange" Interpolation.lagrange_interpolation false


let process_stream (config : Args.config) =
  let windows =
    List.map
      (function
        | Args.Linear -> (Args.Linear, Window.create_window config.window_size)
        | Args.Lagrange -> (Args.Lagrange, Window.create_window config.window_size) )
      config.algorithms
  in

  let rec process_line windows =
    try
      let line = read_line () in
      match Stream.parse_point line with
      | None -> process_line windows
      | Some point ->
          let (new_windows, all_outputs) =
            List.fold_left
              (fun (acc_windows, acc_outputs) (alg, window) ->
                let (new_window, outputs) =
                  match alg with
                  | Args.Linear -> process_linear config window point
                  | Args.Lagrange -> process_lagrange config window point
                in
                ((alg, new_window) :: acc_windows, outputs @ acc_outputs) )
              ([], [])
              windows
          in

          List.iter print_endline all_outputs;
          flush stdout;

          process_line (List.rev new_windows)
    with
    | End_of_file ->
        List.iter
          (fun (alg, window) ->
            let points = Window.get_points window in
            match Window.get_x_range window with
            | None -> ()
            | Some (x_min, x_max) ->
                (* Для линейной интерполяции при EOF не генерируем точки, так как все сегменты уже обработаны *)
                let target_xs =
                  match alg with
                  | Args.Linear -> []
                  | Args.Lagrange ->
                      (* Для Лагранжа генерируем точки для последнего окна *)
                      ( match (config.step, config.n_points) with
                      | (Some step, _) -> Stream.generate_points x_min x_max step
                      | (None, Some n) -> Stream.generate_n_points x_min x_max n
                      | (None, None) -> [ x_max ] )
                in
                let algorithm_name =
                  match alg with
                  | Args.Linear -> "linear"
                  | Args.Lagrange -> "lagrange"
                in
                let interpolate_func =
                  match alg with
                  | Args.Linear -> Interpolation.linear_interpolation
                  | Args.Lagrange -> Interpolation.lagrange_interpolation
                in
                List.iter
                  (fun x ->
                    match interpolate_func points x with
                    | Some y -> print_endline (Stream.format_point algorithm_name (x, y))
                    | None -> () )
                  target_xs;
                flush stdout )
          windows
  in
  process_line windows


let () =
  let config = Args.parse_args (List.tl (Array.to_list Sys.argv)) in
  if config.algorithms = [] then (
    Printf.eprintf
      "использование: %s [--linear|--lagrange] [--step <шаг>] [--n <количество>] [--window <размер>]\n"
      Sys.argv.(0);
    exit 1 )
  else
    process_stream config
