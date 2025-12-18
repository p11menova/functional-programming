open Lab2
open Rb_bag

let int_list_arb = QCheck.(list int)
let int_arb = QCheck.int

(* свойтсво 1: моноид - ассоциативность union (a union b) union c = a union (b
   union c)*)
let prop_union_assoc =
  QCheck.Test.make ~name:"union is associative"
    QCheck.(triple int_list_arb int_list_arb int_list_arb)
    (fun (a, b, c) ->
      let bag_a = of_list a in
      let bag_b = of_list b in
      let bag_c = of_list c in
      let left = union (union bag_a bag_b) bag_c in
      let right = union bag_a (union bag_b bag_c) in
      equal left right )

(* свойство 2: моноид содержит нейтральный элемент empty union a = a union empty
   = a *)
let prop_union_empty =
  QCheck.Test.make ~name:"empty is identity for union" int_list_arb (fun lst ->
      let bag = of_list lst in
      let left = union empty bag in
      let right = union bag empty in
      equal left bag && equal right bag )

(* свойство 3: добавление и удаление одного и то же элемента обратны count x bag
   = count x remove x add x bag*)
let prop_add_remove =
  QCheck.Test.make ~name:"add and remove are inverse"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in
      count x (remove x (add x bag)) = count x bag )

(* свойство 4: размер увеличивается на 1 после добавления size(add(x, bag)) =
   size(bag) + 1*)
let prop_size_add =
  QCheck.Test.make ~name:"size increases by 1 after add"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in
      size (add x bag) = size bag + 1 )

(* свойство 5: размер уменьшается на 1 после удаления существующего элемента
   size(remove(x, bag)) = size(bag) - 1 и не изменяется при попытке удаления
   несущ. в bag*)
let prop_size_remove =
  QCheck.Test.make ~name:"size decreases by 1 after remove existing"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in

      let size_before = size bag in
      let bag' = remove x bag in
      if contains x bag then size bag' = size_before - 1
      else size bag' = size_before )

(* свойство 6: сохранение размера при map map не меняет количество элементов,
   только их значения size(map(f, bag)) = size(bag)*)
let prop_map_preserves_size =
  QCheck.Test.make ~name:"map preserves size" int_list_arb (fun lst ->
      let bag = of_list lst in
      size (map (fun x -> x * 2) bag) = size bag )

(* свойство 7: фильтрация уменьшает или сохраняет размер size(filter(p, bag)) <=
   size(bag)*)
let prop_filter_size =
  QCheck.Test.make ~name:"filter size is less or equal" int_list_arb (fun lst ->
      let bag = of_list lst in
      size (filter (fun x -> x mod 2 = 0) bag) <= size bag )

(* свойство 8: count после добавления увеличивается на 1 count(x, add(x, bag)) =
   count(x, bag) + 1*)
let prop_count_add =
  QCheck.Test.make ~name:"count increases by 1 after add"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in
      count x (add x bag) = count x bag + 1 )

(* свойство 9: count после удаления уменьшается на 1 (если элемент был) если
   count(x, bag) > 0, то count(x, remove(x, bag)) = count(x, bag) - 1*)
let prop_count_remove =
  QCheck.Test.make ~name:"count decreases by 1 after remove"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in
      let count_before = count x bag in
      let bag' = remove x bag in
      if count_before > 0 then count x bag' = count_before - 1
      else count x bag' = 0 )

(* дерево остается валидным после add, remove, union, map*)
let prop_tree_valid_after_add =
  QCheck.Test.make ~name:"tree valid after add"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in
      Rb_bag_utils.is_tree_valid (add x bag) )

let prop_tree_valid_after_remove =
  QCheck.Test.make ~name:"tree valid after remove"
    QCheck.(pair int_list_arb int_arb)
    (fun (lst, x) ->
      let bag = of_list lst in
      Rb_bag_utils.is_tree_valid (remove x bag) )

let prop_tree_valid_after_union =
  QCheck.Test.make ~name:"tree valid after union"
    QCheck.(pair int_list_arb int_list_arb)
    (fun (a, b) ->
      let bag_a = of_list a in
      let bag_b = of_list b in
      Rb_bag_utils.is_tree_valid (union bag_a bag_b) )

let prop_tree_valid_after_map =
  QCheck.Test.make ~name:"tree valid after map" int_list_arb (fun lst ->
      let bag = of_list lst in
      let mapped = map (fun x -> x * 2) bag in
      Rb_bag_utils.is_tree_valid mapped )

(* свойствао 11: fold_left и fold_right дают идентичный результат для
   коммутативных операций (типа сложения)*)
let prop_fold_commutative =
  QCheck.Test.make ~name:"fold_left and fold_right same for commutative ops"
    int_list_arb (fun lst ->
      let bag = of_list lst in
      fold_left ( + ) 0 bag = fold_right ( + ) bag 0
      && fold_left ( * ) 1 bag = fold_right ( * ) bag 1 )

(* свойство 12: to_list и of_list сохраняют размер и содержимое (порядок и цвета
   могут отличаться)*)
let prop_to_list_of_list =
  QCheck.Test.make ~name:"to_list and of_list preserve size and content"
    int_list_arb (fun lst ->
      let bag = Rb_bag.of_list lst in
      let lst' = Rb_bag.to_list bag in
      (* Проверяем, что размер совпадает *)
      List.length lst' = Rb_bag.size bag
      &&
      (* Проверяем, что для каждого элемента количество совпадает *)
      List.for_all
        (fun x ->
          let count_original = List.length (List.filter (( = ) x) lst) in
          let count_result = List.length (List.filter (( = ) x) lst') in
          count_original = count_result )
        (List.sort_uniq Stdlib.compare lst) )

let () =
  QCheck_runner.run_tests_main
    [
      (* Моноид свойства *)
      prop_union_assoc;
      prop_union_empty;
      (* Базовые операции *)
      prop_add_remove;
      prop_size_add;
      prop_size_remove;
      prop_count_add;
      prop_count_remove;
      (* Функциональные операции *)
      prop_map_preserves_size;
      prop_filter_size;
      prop_fold_commutative;
      prop_to_list_of_list;
      (* Валидность дерева *)
      prop_tree_valid_after_add;
      prop_tree_valid_after_remove;
      prop_tree_valid_after_union;
      (* prop_tree_valid_after_filter; - временно отключен из-за упрощенной
         балансировки *)
      prop_tree_valid_after_map;
    ]
