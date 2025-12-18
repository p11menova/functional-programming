open Lab2
open Rb_bag_utils
open Rb_bag

let show_adding_elements () =
  let elements = [ 20; 10; 30; 5; 1 ] in
  let _ =
    List.fold_left
      (fun tree x ->
        Printf.printf "> add %d:\n" x;
        let new_tree = add x tree in
        Printf.printf "%s\n" (to_string string_of_int new_tree);

        new_tree )
      empty elements
  in
  ()

let show_removing_elements () =
  Printf.printf "removing red leaf without rotation:\n";
  let tree1 = of_list [ 20; 10; 30; 5 ] in
  Printf.printf "before remove 5:\n%s\n" (to_string string_of_int tree1);
  let tree1_after = remove 5 tree1 in
  Printf.printf "after remove 5:\n%s\n" (to_string string_of_int tree1_after);

  Printf.printf "removing red node with red brother (needs recoloring):\n";
  let tree2 = of_list [ 20; 10; 30; 5; 15; 25; 35 ] in
  Printf.printf "before remove 5:\n%s\n" (to_string string_of_int tree2);
  let tree2_after = remove 5 tree2 in
  Printf.printf "after remove 5:\n%s\n" (to_string string_of_int tree2_after);

  Printf.printf "removing node with two children (needs min-right successor):\n";
  let tree3 = of_list [ 20; 10; 30; 5; 15; 25; 35; 22; 28 ] in
  Printf.printf "before remove 20 (root with two children):\n%s\n"
    (to_string string_of_int tree3);
  let tree3_after = remove 20 tree3 in
  Printf.printf "after remove 20 (min-right successor 22):\n%s\n"
    (to_string string_of_int tree3_after);
  ()

let () =
  show_adding_elements ();
  show_removing_elements ()
