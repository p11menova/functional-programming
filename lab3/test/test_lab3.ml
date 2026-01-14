open Alcotest
module Interpolation = Lab3.Interpolation
module Stream = Lab3.Stream
module Window = Lab3.Window

let test_linear_interpolation () =
  let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 2.0) ] in

  let result = Interpolation.linear_interpolation points 0.5 in
  check (option (float 0.01)) "linear interpolation at 0.5" (Some 0.5) result;

  let result = Interpolation.linear_interpolation points 0.0 in
  check (option (float 0.01)) "linear interpolation at 0.0" (Some 0.0) result;

  let result = Interpolation.linear_interpolation points 2.0 in
  check (option (float 0.01)) "linear interpolation at 2.0" (Some 2.0) result;

  let result = Interpolation.linear_interpolation points 3.0 in
  check (option (float 0.01)) "linear interpolation outside range" None result;

  let single_point = (1.0, 2.0) :: [] in
  let result = Interpolation.linear_interpolation single_point 1.0 in
  check (option (float 0.01)) "linear interpolation single point" (Some 2.0) result


let test_lagrange_interpolation () =
  let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0) ] in

  let result = Interpolation.lagrange_interpolation points 0.0 in
  check (option (float 0.01)) "lagrange at x=0" (Some 0.0) result;

  let result = Interpolation.lagrange_interpolation points 1.0 in
  check (option (float 0.01)) "lagrange at x=1" (Some 1.0) result;

  let result = Interpolation.lagrange_interpolation points 2.0 in
  check (option (float 0.01)) "lagrange at x=2" (Some 4.0) result;

  (* интерполяция в промежуточной точке (для y=x^2) *)
  let result = Interpolation.lagrange_interpolation points 1.5 in
  let expected = 2.25 in
  (* 1.5^2 = 2.25 *)
  ( match result with
  | Some v -> check (float 0.01) "lagrange at x=1.5" expected v
  | None -> fail "lagrange interpolation returned None" );

  (* тест с одной точкой *)
  let single_point = (1.0, 2.0) :: [] in
  let result = Interpolation.lagrange_interpolation single_point 1.0 in
  check (option (float 0.01)) "lagrange single point" (Some 2.0) result;

  (* тест с пустым списком *)
  let result = Interpolation.lagrange_interpolation [] 1.0 in
  check (option (float 0.01)) "lagrange empty list" None result


let test_parse_point () =
  let result = Stream.parse_point "1.5 2.5" in
  check (option (pair (float 0.01) (float 0.01))) "parse space" (Some (1.5, 2.5)) result;

  let result = Stream.parse_point "1.5;2.5" in
  check (option (pair (float 0.01) (float 0.01))) "parse semicolon" (Some (1.5, 2.5)) result;

  let result = Stream.parse_point "1.5\t2.5" in
  check (option (pair (float 0.01) (float 0.01))) "parse tab" (Some (1.5, 2.5)) result;

  let result = Stream.parse_point "" in
  check (option (pair (float 0.01) (float 0.01))) "parse empty" None result;

  let result = Stream.parse_point "invalid" in
  check (option (pair (float 0.01) (float 0.01))) "parse invalid" None result;

  let result = Stream.parse_point "  1.5 2.5  " in
  check (option (pair (float 0.01) (float 0.01))) "parse with spaces" (Some (1.5, 2.5)) result


let test_generate_points () =
  let result = Stream.generate_points 0.0 1.0 0.25 in
  let expected = [ 0.0; 0.25; 0.5; 0.75; 1.0 ] in
  check (list (float 0.01)) "generate points step 0.25" expected result;

  let result = Stream.generate_points 0.0 1.0 0.3 in
  let expected = [ 0.0; 0.3; 0.6; 0.9 ] in
  check (list (float 0.01)) "generate points step 0.3" expected result;

  let result = Stream.generate_points 0.0 1.0 2.0 in
  let expected = [ 0.0 ] in
  check (list (float 0.01)) "generate points large step" expected result


let test_generate_n_points () =
  let result = Stream.generate_n_points 0.0 1.0 5 in
  let expected = [ 0.0; 0.25; 0.5; 0.75; 1.0 ] in
  check (list (float 0.01)) "generate 5 points" expected result;

  let result = Stream.generate_n_points 0.0 1.0 2 in
  let expected = [ 0.0; 1.0 ] in
  check (list (float 0.01)) "generate 2 points" expected result;

  let result = Stream.generate_n_points 0.0 1.0 1 in
  let expected = [ 0.0 ] in
  check (list (float 0.01)) "generate 1 point" expected result


let test_window () =
  let window = Window.create_window 3 in
  check int "window initial size" 0 (List.length (Window.get_points window));
  check bool "window not full" false (Window.is_full window);

  let window = Window.create_window 3 in
  let window = Window.add_point window (0.0, 0.0) in
  let window = Window.add_point window (1.0, 1.0) in
  let window = Window.add_point window (2.0, 2.0) in

  check int "window size after 3 points" 3 (List.length (Window.get_points window));
  check bool "window full" true (Window.is_full window);

  let window = Window.add_point window (3.0, 3.0) in
  check int "window size after overflow" 3 (List.length (Window.get_points window));

  let window = Window.create_window 5 in
  let window = Window.add_point window (2.0, 2.0) in
  let window = Window.add_point window (0.0, 0.0) in
  let window = Window.add_point window (1.0, 1.0) in
  let points = Window.get_points window in
  let xs = List.map fst points in
  check (list (float 0.01)) "window sorted" [ 0.0; 1.0; 2.0 ] xs;

  let window = Window.create_window 3 in
  let window = Window.add_point window (0.0, 0.0) in
  let window = Window.add_point window (2.0, 2.0) in
  let window = Window.add_point window (1.0, 1.0) in
  match Window.get_x_range window with
  | Some (x_min, x_max) ->
      check (float 0.01) "x_min" 0.0 x_min;
      check (float 0.01) "x_max" 2.0 x_max
  | None -> fail "get_x_range returned None"


let test_format_point () =
  let result = Stream.format_point "linear" (1.5, 2.5) in
  check string "format point" "linear: 1.5000 2.5000" result;

  let result = Stream.format_point "lagrange" (0.0, 0.0) in
  check string "format point zero" "lagrange: 0.0000 0.0000" result


let () =
  run
    "Lab3 Tests"
    [
      ( "Interpolation"
      , [
          test_case "Linear interpolation" `Quick test_linear_interpolation
        ; test_case "Lagrange interpolation" `Quick test_lagrange_interpolation
        ] )
    ; ( "Stream"
      , [
          test_case "Parse point" `Quick test_parse_point
        ; test_case "Generate points" `Quick test_generate_points
        ; test_case "Generate n points" `Quick test_generate_n_points
        ; test_case "Format point" `Quick test_format_point
        ] )
    ; ("Window", [ test_case "Window operations" `Quick test_window ])
    ]
