(** utils for red-black tree structure: functions helpers *)
open Rb_bag_initialization


let color_of = function 
| Leaf -> Black
| Node {color; _} -> color

let is_red = function
  | Node {color = Red; _} -> true
  | _ -> false

(* methods for checking all RBT invariants *)
(** 1. the root of the tree must be black *)
let check_root_is_black = function
  | Leaf -> true 
  | Node {color; _} -> color = Black

(** 2. no red node has a red child *)
let rec check_no_red_red = function
  | Leaf -> true
  | Node {color = Red; left; right; _} -> (* if node is red, both children must be black *)
      color_of left = Black && 
      color_of right = Black &&
      check_no_red_red left &&
      check_no_red_red right
  | Node {color = Black; left; right; _} ->
      (* if node is black, recursively check children *)
      check_no_red_red left && check_no_red_red right

(** 3. every path from the root to a leaf has the same number of black nodes = black height *)
let check_black_height = None;