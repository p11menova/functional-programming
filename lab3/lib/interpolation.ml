type point = float * float

let linear_interpolate (x1, y1) (x2, y2) x =
  if x2 -. x1 = 0.0 then
    y1
  else
    y1 +. ((y2 -. y1) /. (x2 -. x1) *. (x -. x1))


let linear_interpolation points x =
  let rec find_segment = function
    | [] -> None
    | [ (x', y') ] -> if x = x' then Some ((x', y'), (x', y')) else None
    | (x1, y1) :: (x2, y2) :: rest ->
        if x >= x1 && x <= x2 then
          Some ((x1, y1), (x2, y2))
        else
          find_segment ((x2, y2) :: rest)
  in
  match find_segment points with
  | None -> None
  | Some ((x1, y1), (x2, y2)) -> Some (linear_interpolate (x1, y1) (x2, y2) x)


let lagrange_interpolation points x =
  let n = List.length points in
  if n = 0 then
    None
  else if n = 1 then
    Some (snd (List.hd points))
  else
    let sorted_points = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) points in

    (* вычисление базисных полиномов Лагранжа L_i(x) и суммирование *)
    let result = ref 0.0 in

    List.iteri
      (fun i (xi, yi) ->
        (* вычисляем L_i(x) = Π(j≠i) (x - x_j) / (x_i - x_j) *)
        let li = ref 1.0 in
        List.iteri
          (fun j (xj, _) ->
            if i <> j then
              let denominator = xi -. xj in
              if denominator <> 0.0 then
                li := !li *. ((x -. xj) /. denominator) )
          sorted_points;
        (* добавляем y_i * L_i(x) к результату *)
        result := !result +. (yi *. !li) )
      sorted_points;

    Some !result
