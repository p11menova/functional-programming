open Lab2
open Rb_bag_utils

let () =
  print_endline "Hello, World!";
  let tree = Rb_bag.of_list [ 1; 2; 3; 4; 5 ] in
  Printf.printf "%s\n" (to_string string_of_int tree)
;;

let tree2 = Rb_bag.of_list [ 1; 1; 2; 3 ] in
Printf.printf "%s\n" (to_string string_of_int tree2);
Printf.printf "%b\n" (check_root_is_black tree2);
Printf.printf "%b\n" (check_no_red_red tree2);
Printf.printf "%b\n" (check_black_height tree2);
Printf.printf "%b\n" (is_tree_valid tree2);
Printf.printf "%s\n" (to_string string_of_int (Rb_bag.of_list [ 1; 0 ]))
