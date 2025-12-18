(** Unit тесты для RB-Bag *)

open Lab2

(* базовые операции *)
let test_empty () =
  Alcotest.(check bool) "empty is empty" true (Rb_bag.is_empty Rb_bag.empty);
  Alcotest.(check int) "empty size is 0" 0 (Rb_bag.size Rb_bag.empty);
  Alcotest.(check int)
    "empty unique_count is 0" 0
    (Rb_bag.unique_count Rb_bag.empty)

let test_size () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3; 3; 3 ] in
  Alcotest.(check int) "size counts all elements" 6 (Rb_bag.size bag);
  Alcotest.(check int)
    "unique_count counts unique elements" 3 (Rb_bag.unique_count bag)

let test_count () =
  let bag = Rb_bag.of_list [ 1; 1; 1; 2; 2; 3 ] in
  Alcotest.(check int) "count of 1" 3 (Rb_bag.count 1 bag);
  Alcotest.(check int) "count of 2" 2 (Rb_bag.count 2 bag);
  Alcotest.(check int) "count of 3" 1 (Rb_bag.count 3 bag);
  Alcotest.(check int) "count of non-existent" 0 (Rb_bag.count 5 bag)

let test_contains () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3 ] in
  Alcotest.(check bool) "contains existing element" true (Rb_bag.contains 2 bag);
  Alcotest.(check bool) "contains non-existent" false (Rb_bag.contains 5 bag)

(** добавление и удаление *)
let test_add () =
  let bag = Rb_bag.add 1 Rb_bag.empty in
  Alcotest.(check bool) "not empty after add" false (Rb_bag.is_empty bag);
  Alcotest.(check int) "size after add" 1 (Rb_bag.size bag);
  Alcotest.(check int) "count of added element" 1 (Rb_bag.count 1 bag);
  (* проверка инвариантов после добавления *)
  Alcotest.(check bool)
    "tree valid after add" true
    (Rb_bag_utils.is_tree_valid bag);
  let bag2 = Rb_bag.add 1 bag in
  Alcotest.(check int) "size after duplicate add" 2 (Rb_bag.size bag2);
  Alcotest.(check int) "count after duplicate add" 2 (Rb_bag.count 1 bag2);
  Alcotest.(check bool)
    "tree valid after duplicate add" true
    (Rb_bag_utils.is_tree_valid bag2);
  let bag3 = Rb_bag.add 2 bag2 in
  Alcotest.(check int) "size after add different" 3 (Rb_bag.size bag3);
  Alcotest.(check int) "count of new element" 1 (Rb_bag.count 2 bag3);
  Alcotest.(check bool)
    "tree valid after add different" true
    (Rb_bag_utils.is_tree_valid bag3)

(** проверка инвариантов после множественных добавлений *)
let test_add_invariants () =
  let bag = ref Rb_bag.empty in
  (* добавляем много элементов в разном порядке *)
  let elements = [ 5; 3; 7; 1; 9; 2; 8; 4; 6; 10 ] in
  List.iter
    (fun x ->
      bag := Rb_bag.add x !bag;
      Alcotest.(check bool)
        ("tree valid after adding " ^ string_of_int x)
        true
        (Rb_bag_utils.is_tree_valid !bag) )
    elements;
  (* добавляем дубликаты *)
  let duplicates = [ 5; 5; 3; 3; 7; 1 ] in
  List.iter
    (fun x ->
      bag := Rb_bag.add x !bag;
      Alcotest.(check bool)
        ("tree valid after adding duplicate " ^ string_of_int x)
        true
        (Rb_bag_utils.is_tree_valid !bag) )
    duplicates

let test_remove () =
  let bag = Rb_bag.of_list [ 1; 1; 2; 3 ] in

  Alcotest.(check bool)
    "tree valid before remove" true
    (Rb_bag_utils.is_tree_valid bag);
  let bag' = Rb_bag.remove 1 bag in
  Alcotest.(check int) "count after remove" 1 (Rb_bag.count 1 bag');
  Alcotest.(check int) "size after remove" 3 (Rb_bag.size bag');
  Alcotest.(check bool)
    "tree valid after remove" true
    (Rb_bag_utils.is_tree_valid bag');
  let bag'' = Rb_bag.remove 1 bag' in
  Alcotest.(check int) "count after second remove" 0 (Rb_bag.count 1 bag'');
  Alcotest.(check int) "size after second remove" 2 (Rb_bag.size bag'');
  Alcotest.(check bool)
    "tree valid after second remove" true
    (Rb_bag_utils.is_tree_valid bag'');
  let bag''' = Rb_bag.remove 5 bag'' in
  Alcotest.(check int)
    "remove non-existent doesn't change size" 2 (Rb_bag.size bag''');
  Alcotest.(check bool)
    "tree valid after remove non-existent" true
    (Rb_bag_utils.is_tree_valid bag''');

  let bag'''' = Rb_bag.remove 3 bag''' in
  Alcotest.(check int) "remove elem 3 changed size" 1 (Rb_bag.size bag'''');
  Alcotest.(check bool)
    "tree valid after remove elem 3" true
    (Rb_bag_utils.is_tree_valid bag'''')

(** функциональные операции *)
let test_filter () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3; 4; 4; 4 ] in
  let filtered = Rb_bag.filter (fun x -> x mod 2 = 0) bag in
  Alcotest.(check int) "filter even numbers count 2" 2 (Rb_bag.count 2 filtered);
  Alcotest.(check int) "filter even numbers count 4" 3 (Rb_bag.count 4 filtered);
  Alcotest.(check int) "filter removes odd numbers" 0 (Rb_bag.count 1 filtered);
  Alcotest.(check int)
    "filter removes odd numbers 3" 0 (Rb_bag.count 3 filtered);
  let all_filtered = Rb_bag.filter (fun _ -> false) bag in
  Alcotest.(check bool)
    "filter all false returns empty" true
    (Rb_bag.is_empty all_filtered)

let test_map () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3 ] in
  let mapped = Rb_bag.map (fun x -> x * 2) bag in
  (* после map (fun x -> x * 2) на [1; 2; 2; 3] должно быть [2; 4; 4; 6] *)
  (* поэтому count 2 = 1, count 4 = 2, count 6 = 1 *)
  Alcotest.(check int) "map doubles count of 2" 1 (Rb_bag.count 2 mapped);
  Alcotest.(check int) "map doubles count of 4" 2 (Rb_bag.count 4 mapped);
  Alcotest.(check int) "map doubles count of 6" 1 (Rb_bag.count 6 mapped);
  Alcotest.(check int) "map preserves size" 4 (Rb_bag.size mapped)

let test_fold_left () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3 ] in
  let sum = Rb_bag.fold_left ( + ) 0 bag in
  Alcotest.(check int) "fold_left sum" 8 sum;
  let product = Rb_bag.fold_left ( * ) 1 bag in
  Alcotest.(check int) "fold_left product" 12 product;
  let count_even =
    Rb_bag.fold_left (fun acc x -> if x mod 2 = 0 then acc + 1 else acc) 0 bag
  in
  Alcotest.(check int) "fold_left count even" 2 count_even

let test_fold_right () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3 ] in
  let sum = Rb_bag.fold_right ( + ) bag 0 in
  Alcotest.(check int) "fold_right sum" 8 sum;
  let list = Rb_bag.fold_right (fun x acc -> x :: acc) bag [] in
  Alcotest.(check (list int)) "fold_right to list" [ 1; 2; 2; 3 ] list

(** моноид *)
let test_union () =
  let a = Rb_bag.of_list [ 1; 2; 2 ] in
  let b = Rb_bag.of_list [ 2; 3 ] in
  let c = Rb_bag.union a b in
  Alcotest.(check int) "union count of 1" 1 (Rb_bag.count 1 c);
  Alcotest.(check int) "union count of 2" 3 (Rb_bag.count 2 c);
  Alcotest.(check int) "union count of 3" 1 (Rb_bag.count 3 c);
  let empty_union = Rb_bag.union Rb_bag.empty a in
  Alcotest.(check int) "union with empty preserves" 3 (Rb_bag.size empty_union);
  let union_empty = Rb_bag.union a Rb_bag.empty in
  Alcotest.(check int) "union empty preserves" 3 (Rb_bag.size union_empty);
  (* проверка идентичности (рефлексивности) *)
  Alcotest.(check bool) "union empty a = a" true (Rb_bag.equal empty_union a);
  Alcotest.(check bool) "union a empty = a" true (Rb_bag.equal union_empty a);
  (* проверка ассоциативности *)
  let d = Rb_bag.of_list [ 4; 5; 5 ] in
  let left_assoc = Rb_bag.union (Rb_bag.union a b) d in
  let right_assoc = Rb_bag.union a (Rb_bag.union b d) in
  Alcotest.(check bool)
    "union is associative: (a union b) union d = a union (b union d)" true
    (Rb_bag.equal left_assoc right_assoc)

(** утилиты *)
let test_to_list () =
  let bag = Rb_bag.of_list [ 1; 2; 2; 3 ] in
  let lst = Rb_bag.to_list bag in
  Alcotest.(check (list int)) "to_list contains all elements" [ 1; 2; 2; 3 ] lst;
  let empty_lst = Rb_bag.to_list Rb_bag.empty in
  Alcotest.(check (list int)) "to_list empty is empty" [] empty_lst

let test_of_list () =
  let bag = Rb_bag.of_list [ 3; 1; 2; 2; 1 ] in
  Alcotest.(check int) "of_list count 1" 2 (Rb_bag.count 1 bag);
  Alcotest.(check int) "of_list count 2" 2 (Rb_bag.count 2 bag);
  Alcotest.(check int) "of_list count 3" 1 (Rb_bag.count 3 bag);
  Alcotest.(check int) "of_list size" 5 (Rb_bag.size bag);
  Alcotest.(check bool)
    "created tree is valid" true
    (Rb_bag_utils.is_tree_valid bag)

let test_iter () =
  let bag = Rb_bag.of_list [ 1; 2; 3 ] in
  let acc = ref [] in
  Rb_bag.iter (fun x -> acc := x :: !acc) bag;
  Alcotest.(check (list int)) "iter collects all elements" [ 3; 2; 1 ] !acc

let test_all () =
  let bag = Rb_bag.of_list [ 2; 4; 6; 8 ] in
  Alcotest.(check bool)
    "all even returns true" true
    (Rb_bag.all (fun x -> x mod 2 = 0) bag);
  Alcotest.(check bool)
    "all > 10 returns false" false
    (Rb_bag.all (fun x -> x > 10) bag);
  let empty_all = Rb_bag.all (fun _ -> false) Rb_bag.empty in
  Alcotest.(check bool) "all on empty returns true" true empty_all

let test_any () =
  let bag = Rb_bag.of_list [ 1; 3; 5; 7 ] in
  Alcotest.(check bool)
    "any even returns false" false
    (Rb_bag.any (fun x -> x mod 2 = 0) bag);
  Alcotest.(check bool)
    "any > 5 returns true" true
    (Rb_bag.any (fun x -> x > 5) bag);
  let empty_any = Rb_bag.any (fun _ -> true) Rb_bag.empty in
  Alcotest.(check bool) "any on empty returns false" false empty_any

let () =
  let open Alcotest in
  run "RB-Bag Tests"
    [
      ( "базовые операции",
        [
          test_case "empty" `Quick test_empty;
          test_case "size" `Quick test_size;
          test_case "count" `Quick test_count;
          test_case "contains" `Quick test_contains;
        ] );
      ( "добавление и удаление",
        [
          test_case "add" `Quick test_add;
          test_case "add invariants" `Quick test_add_invariants;
          test_case "remove" `Quick test_remove;
        ] );
      ( "функциональные операции",
        [
          test_case "filter" `Quick test_filter;
          test_case "map" `Quick test_map;
          test_case "fold_left" `Quick test_fold_left;
          test_case "fold_right" `Quick test_fold_right;
        ] );
      ("моноид", [ test_case "union" `Quick test_union ]);
      ( "утилиты",
        [
          test_case "to_list" `Quick test_to_list;
          test_case "of_list" `Quick test_of_list;
          test_case "iter" `Quick test_iter;
          test_case "all" `Quick test_all;
          test_case "any" `Quick test_any;
        ] );
    ]
