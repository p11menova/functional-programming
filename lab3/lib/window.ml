type point = float * float

type window = {
    points : point list
  ; max_size : int
}

(** вспомогательная функция для взятия первых n элементов *)
let take n lst =
  let rec take_aux acc count = function
    | [] -> List.rev acc
    | hd :: tl -> if count >= n then List.rev acc else take_aux (hd :: acc) (count + 1) tl
  in
  take_aux [] 0 lst


let create_window max_size = { points = []; max_size }

let add_point window point =
  let new_points = point :: window.points in
  let sorted_points = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) new_points in
  let trimmed_points =
    if List.length sorted_points > window.max_size then
      take window.max_size sorted_points
    else
      sorted_points
  in
  { window with points = trimmed_points }


let get_points window = window.points

let is_full window = List.length window.points >= window.max_size

let get_x_range window =
  match window.points with
  | [] -> None
  | _ ->
      let xs = List.map fst window.points in
      let x_min = List.fold_left min (List.hd xs) (List.tl xs) in
      let x_max = List.fold_left max (List.hd xs) (List.tl xs) in
      Some (x_min, x_max)


let trim_window window =
  if List.length window.points > window.max_size then
    let sorted = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) window.points in
    { window with points = take window.max_size sorted }
  else
    window
