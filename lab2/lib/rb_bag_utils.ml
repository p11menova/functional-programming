open Rb_bag_initialization
(* utils for red-black tree structure: functions helpers *)
(* used in tests for checking if tree keeps being valid after inserting/removing elems*)

let color_of = function
  | Leaf -> Black
  | Node { color; _ } -> color

let is_red = function
  | Node { color = Red; _ } -> true
  | _ -> false

let make_red value left right = Node { color = Red; value; left; right }
let make_black value left right = Node { color = Black; value; left; right }
(* methods for checking all RBT invariants *)

(** 1. the root of the tree must be black *)
let check_root_is_black = function
  | Leaf -> true
  | Node { color; _ } -> color = Black

(** 2. no red node has a red child *)
let rec check_no_red_red = function
  | Leaf -> true
  | Node { color = Red; left; right; _ } ->
    (* if node is red, both children must be black *)
    color_of left = Black
    && color_of right = Black
    && check_no_red_red left
    && check_no_red_red right
  | Node { color = Black; left; right; _ } ->
    (* if node is black, recursively check children *)
    check_no_red_red left && check_no_red_red right

(** 3. every path from the root to a leaf has the same number of black nodes =
    black height *)
let check_black_height tree =
  (* helper function that returs None if black height is not the same for all
     paths, Some(height) otherwise *)
  let rec get_black_height = function
    | Leaf -> Some 1
    | Node { color; left; right; _ } ->
      (* recursively get heights of left and right subtrees *)
      ( match (get_black_height left, get_black_height right) with
      | (None, _) -> None
      | (_, None) -> None
      | (Some left_height, Some right_height) ->
        if left_height <> right_height then None
        else Some (left_height + if color = Black then 1 else 0) )
  in

  match get_black_height tree with
  | Some _ -> true
  | None -> false

(* i am not checking the fact, that each node must have a color, because it is
   not possible to create a tree with invalid colors *)

let is_tree_valid t =
  check_root_is_black t && check_no_red_red t && check_black_height t

(** красивая печать дерева в ASCII-арт формате *)
let pp fmt_to_string ppf tree =
  let rec print_node indent is_last = function
    | Leaf -> Format.fprintf ppf "%s└── (Leaf)\n" indent
    | Node { color; value = (v, count); left; right } ->
      let color_str =
        match color with
        | Red -> "red"
        | Black -> "black"
      in
      let value_str = fmt_to_string v in
      let prefix = if is_last then "└── " else "├── " in
      Format.fprintf ppf "%s%s[%s] (%s, cnt=%d)\n" indent prefix color_str
        value_str count;
      let new_indent = indent ^ if is_last then "    " else "│   " in
      ( match (left, right) with
      | (Leaf, Leaf) -> ()
      | (Leaf, right) -> print_node new_indent true right
      | (left, Leaf) -> print_node new_indent true left
      | (left, right) ->
        print_node new_indent false left;
        print_node new_indent true right )
  in
  match tree with
  | Leaf -> Format.fprintf ppf "(empty tree)\n"
  | Node _ as node ->
    Format.fprintf ppf "RB-Tree:\n";
    print_node "" true node

(** преобразование дерева в строку *)
let to_string fmt_to_string tree = Format.asprintf "%a" (pp fmt_to_string) tree
