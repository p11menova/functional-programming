open Rb_bag_initialization

type 'a t = 'a rb_node
let empty = Leaf

let is_empty = function
  | Leaf -> true
  | Node _ -> false

let rec size = function
  | Leaf -> 0
  | Node { left; value = (_, count); right; _ } -> size left + count + size right

let rec unique_count = function
  | Leaf -> 0
  | Node { left; value = (_, _); right; _ } -> 1 + unique_count left + unique_count right

let rec count x = function
  | Leaf -> 0
  | Node { value = (v, cnt); left; right; _ } ->
    if v = x then cnt else if x < v then count x left else count x right

let contains x tree =
  let rec aux x = function
    | Leaf -> false
    | Node { value = (v, _); left; right; _ } ->
      if x = v then true else if x < v then aux x left else aux x right
  in
  aux x tree

(** Балансировка дерева после вставки элемента.
    Обрабатывает случаи нарушения инвариантов Red-Black Tree:
    - если дядя красный: перекрашиваем узлы
    - если дядя черный: выполняем повороты *)
let balance_after_insertion tree =
  match tree with
  (* 1. left-left - левый ребенок левого ребенка красный *)
  | Node
      {
        color = Black;
        value = g;
        left =
          Node
            {
              color = Red;
              value = p;
              left = Node { color = Red; value = x; left = a; right = b };
              right = c;
            };
        right = d;
      } ->
    (* поворот: поднимаем p наверх, делаем g и x черными *)
    Node
      {
        color = Red;
        value = p;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Node { color = Black; value = g; left = c; right = d };
      }
  (* 2. left-right - правый ребенок левого ребенка красный *)
  | Node
      {
        color = Black;
        value = g;
        left =
          Node
            {
              color = Red;
              value = x;
              left = a;
              right = Node { color = Red; value = p; left = b; right = c };
            };
        right = d;
      } ->
    (* двойной поворот: сначала левый, потом правый *)
    Node
      {
        color = Red;
        value = p;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Node { color = Black; value = g; left = c; right = d };
      }
  (* случай 3: right-right - правый ребенок правого ребенка красный *)
  | Node
      {
        color = Black;
        value = x;
        left = a;
        right =
          Node
            {
              color = Red;
              value = p;
              left = b;
              right = Node { color = Red; value = g; left = c; right = d };
            };
      } ->
    (* поворот: поднимаем p наверх, делаем x и g черными *)
    Node
      {
        color = Red;
        value = p;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Node { color = Black; value = g; left = c; right = d };
      }
  (* случай 4: right-left - левый ребенок правого ребенка красный *)
  | Node
      {
        color = Black;
        value = x;
        left = a;
        right =
          Node
            {
              color = Red;
              value = g;
              left = Node { color = Red; value = p; left = b; right = c };
              right = d;
            };
      } ->
    (* двойной поворот: сначала правый, потом левый *)
    Node
      {
        color = Red;
        value = p;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Node { color = Black; value = g; left = c; right = d };
      }
  | t -> t

(** добавление элемента в мультисет.
    если элемент уже есть -> счетчик + 1.
    иначе -> вставляем новый узел + балансируем дерево *)
let add x tree =
  (* вспомогательная функция рекурсивной вставки *)
  let rec insert = function
    (* если достигли листа, создаем новый красный узел *)
    | Leaf -> Node { color = Red; value = (x, 1); left = Leaf; right = Leaf }

    (* если элемент найден -> счетчик + 1 *)
    | Node { color; value = (v, count); left; right } when x = v ->
      Node { color; value = (v, count + 1); left; right }
    (* если элемент меньше текущего, идем влево *)
    | Node { color; value = (v, count); left; right } when x < v ->
      (* рекурсивно вставляем в левое поддерево и балансируем *)
      balance_after_insertion
        (Node { color; value = (v, count); left = insert left; right })
    (* если элемент больше текущего, идем вправо *)
    | Node { color; value = (v, count); left; right } ->
      (* рекурсивно вставляем в правое поддерево и балансируем *)
      balance_after_insertion
        (Node { color; value = (v, count); left; right = insert right })
  in
  (* после вставки корень всегда должен быть черным *)
  let result = insert tree in
  match result with
  | Leaf -> Leaf
  | Node n -> Node { n with color = Black }

(** балансировка дерева после удаления.
    обрабатывает случаи нарушения инвариантов когда удалили черный узел.
    полная реализация всех случаев балансировки *)
let balance_after_deletion tree =
  match tree with
  (* случай 1: левый ребенок черный, правый красный - делаем правый черным и поворачиваем *)
  | Node
      {
        color = Black;
        value = x;
        left = a;
        right =
          Node
            {
              color = Red;
              value = y;
              left = b;
              right = c;
            };
      } ->
    Node
      {
        color = Black;
        value = y;
        left = Node { color = Black; value = x; left = a; right = b };
        right = c;
      }
  (* случай 2: правый ребенок черный, левый красный - делаем левый черным и поворачиваем *)
  | Node
      {
        color = Black;
        value = x;
        left =
          Node
            {
              color = Red;
              value = y;
              left = a;
              right = b;
            };
        right = c;
      } ->
    Node
      {
        color = Black;
        value = y;
        left = a;
        right = Node { color = Black; value = x; left = b; right = c };
      }
  (* случай 3: оба ребенка черные, родитель черный - перекрашиваем правого ребенка в красный *)
  (* но только если это не нарушит черную высоту *)
  | Node
      {
        color = Black;
        value = x;
        left = a;
        right =
          Node
            {
              color = Black;
              value = y;
              left = b;
              right = c;
            };
      } ->
    (* проверяем, нет ли у правого ребенка красных детей *)
    ( match (b, c) with
    | (Node { color = Red; _ }, _) | (_, Node { color = Red; _ }) ->
      (* у ребенка есть красные дети - не перекрашиваем, возвращаем как есть *)
      tree
    | _ ->
      (* перекрашиваем правого ребенка в красный для восстановления черной высоты *)
      Node
        {
          color = Black;
          value = x;
          left = a;
          right = Node { color = Red; value = y; left = b; right = c };
        } )
  (* случай 4: оба ребенка черные, родитель черный - перекрашиваем левого ребенка в красный *)
  (* но только если это не нарушит черную высоту *)
  | Node
      {
        color = Black;
        value = x;
        left =
          Node
            {
              color = Black;
              value = y;
              left = a;
              right = b;
            };
        right = c;
      } ->
    (* проверяем, нет ли у левого ребенка красных детей *)
    ( match (a, b) with
    | (Node { color = Red; _ }, _) | (_, Node { color = Red; _ }) ->
      (* у ребенка есть красные дети - не перекрашиваем, возвращаем как есть *)
      tree
    | _ ->
      (* перекрашиваем левого ребенка в красный для восстановления черной высоты *)
      Node
        {
          color = Black;
          value = x;
          left = Node { color = Red; value = y; left = a; right = b };
          right = c;
        } )
  (* если структура не нарушает инварианты возвращаем как есть *)
  | t -> t

(** поиск минимального элемента в дереве (для удаления узла с двумя детьми) *)
let rec find_min = function
  | Leaf -> None
  | Node { left = Leaf; value; _ } -> Some value
  | Node { left; _ } -> find_min left


(** удаление элемента из мультисета.
    если элемент найден и счетчик > 1 -> счетчик - 1 (без балансировки).
    если счетчик = 1 -> удаляем узел и балансируем дерево *)
let remove x tree =
  (* вспомогательная функция рекурсивного удаления.
     возвращает (результат, нужно_ли_балансировать) *)
  let rec delete = function
    (* элемент не найден *)
    | Leaf -> (Leaf, false)
    (* элемент найден *)
    | Node { color; value = (v, count); left; right } when x = v ->
      if count > 1 then
        (* если счетчик > 1, просто уменьшаем счетчик - балансировка НЕ нужна *)
        (Node { color; value = (v, count - 1); left; right }, false)
      else
        (* если счетчик = 1, удаляем узел - балансировка нужна *)
        ( match (left, right) with
        (* узел - лист *)
        | (Leaf, Leaf) -> (Leaf, true)
        (* узел с одним ребенком (правым) *)
        | (Leaf, right_child) -> (
            match right_child with
            | Node n ->
                (* если удаляемый узел черный, перекрашиваем ребенка в черный *)
                (* если удаляемый узел красный, оставляем цвет ребенка как есть *)
                if color = Black then
                  (Node { n with color = Black }, true)
                else
                  (right_child, true)
            | Leaf -> (Leaf, true) )
        (* узел с одним ребенком (левым) *)
        | (left_child, Leaf) -> (
            match left_child with
            | Node n ->
                (* если удаляемый узел черный, перекрашиваем ребенка в черный *)
                (* если удаляемый узел красный, оставляем цвет ребенка как есть *)
                if color = Black then
                  (Node { n with color = Black }, true)
                else
                  (left_child, true)
            | Leaf -> (Leaf, true) )
        (* узел с двумя детьми: находим преемника (минимум в правом поддереве) *)
        | (left_child, right_child) -> (
            match find_min right_child with
            | None -> (left_child, true)
            | Some (min_val, min_count) ->
              (* заменяем текущий узел на преемника и удаляем преемника из правого поддерева *)
              (* преемник получает цвет удаляемого узла *)
              let (new_right, need_balance_right) = 
                let rec delete_min_node = function
                  | Leaf -> (Leaf, false)
                  | Node { left = Leaf; right; color = min_color; _ } -> 
                      (* если преемник красный, просто удаляем его *)
                      (* если преемник черный, нужна балансировка *)
                      (right, min_color = Black)
                  | Node { color; value; left; right } ->
                      let (new_left, need_balance) = delete_min_node left in
                      if need_balance then
                        ( balance_after_deletion
                            (Node { color; value; left = new_left; right }),
                          true )
                      else
                        (Node { color; value; left = new_left; right }, false)
                in
                delete_min_node right_child
              in
              let result =
                if need_balance_right then
                  balance_after_deletion
                    (Node
                       {
                         color;
                         value = (min_val, min_count);
                         left = left_child;
                         right = new_right;
                       })
                else
                  Node
                    {
                      color;
                      value = (min_val, min_count);
                      left = left_child;
                      right = new_right;
                    }
              in
              (result, need_balance_right) ) )
    (* элемент меньше текущего, идем влево *)
    | Node { color; value = (v, count); left; right } when x < v ->
      let (new_left, need_balance) = delete left in
      if need_balance then
        ( balance_after_deletion
            (Node { color; value = (v, count); left = new_left; right }),
          true )
      else
        (Node { color; value = (v, count); left = new_left; right }, false)
    (* элемент больше текущего, идем вправо *)
    | Node { color; value = (v, count); left; right } ->
      let (new_right, need_balance) = delete right in
      if need_balance then
        ( balance_after_deletion
            (Node { color; value = (v, count); left; right = new_right }),
          true )
      else
        (Node { color; value = (v, count); left; right = new_right }, false)
  in
  (* после удаления корень всегда должен быть черным *)
  let result = fst (delete tree) in
  match result with
  | Leaf -> Leaf
  | Node n ->
      (* убеждаемся, что корень черный *)
      let root = Node { n with color = Black } in
      root


(** функциональные операции *)

(* обход дерева слева направо *)
let rec fold_left f acc = function
  | Leaf -> acc
  | Node { left; value = (elem, count); right; _ } ->
    let acc' = fold_left f acc left in
    let acc'' = List.fold_left f acc' (List.init count (Fun.const elem)) in
    fold_left f acc'' right

(* обход справа налево*)
let rec fold_right f = function
  | Leaf -> fun acc -> acc
  | Node { left; value = (elem, count); right; _ } ->
    fun acc ->
      let acc' = fold_right f right acc in
      let acc'' = List.fold_right f (List.init count (Fun.const elem)) acc' in
      fold_right f left acc''

(** моноид: объединение двух мультисетов *)
let union a b =
  (* объединяем все элементы из a в b *)
  fold_left (fun acc x -> add x acc) b a

(** если элемент не проходит фильтр -> узел полностью удаляется
    иначе -> узел остается, но рекурсивно фильтруются поддеревья *)
let rec filter predicate = function
  | Leaf -> Leaf
  | Node { color; value = (elem, count); left; right } ->
    (* рекурсивно фильтруем левое и правое поддеревья *)
    let filtered_left = filter predicate left in
    let filtered_right = filter predicate right in
    if predicate elem then
      (* элемент проходит фильтр -> создаем новый узел с отфильтрованными детьми *)
      Node { color; value = (elem, count); left = filtered_left; right = filtered_right }
    else
      (* элемент не проходит фильтр -> удаляем узел и объединяем поддеревья *)
      (* объединяем отфильтрованные поддеревья через union *)
      union filtered_left filtered_right

(** применение функции к каждому элементу мультисета.
    после применения функции порядок элементов может измениться,
    поэтому перестраиваем дерево через fold_left + add *)
let map f bag =
  (* обходим дерево и применяем функцию к каждому элементу (с учетом кратности),
     затем перестраиваем дерево из результатов *)
  fold_left
    (fun acc x ->
      (* применяем функцию к элементу и добавляем в новое дерево *)
      add (f x) acc)
    empty bag


let to_list bag = fold_right (fun x acc -> x :: acc) bag []

let of_list lst = List.fold_left (fun acc x -> add x acc) empty lst



let iter f bag = fold_left (fun () x -> f x) () bag

(** проверка что все элементы удовлетворяют предикату *)
let all f bag = fold_left (fun acc x -> acc && f x) true bag

(** проверка что хотя бы один элемент удовлетворяет предикату *)
let any f bag = fold_left (fun acc x -> acc || f x) false bag

(** эффективное сравнение двух мультисетов *)
let equal a b =
  (* сравниваем размеры *)
  if size a <> size b then false
  else
    (* если размеры равны, сравниваем через fold: для каждого элемента в a
       проверяем что его количество равно количеству в b *)
    (* собираем все уникальные элементы из обоих мультисетов *)
    let all_elements = union a b in
    fold_left
      (fun acc x ->
        acc && count x a = count x b)
      true all_elements

(** сравнение двух мультисетов *)
let compare cmp_fun a b =
  let size_a = size a in
  let size_b = size b in
  if size_a < size_b then -1
  else if size_a > size_b then 1
  else
    (* если размеры равны, сравниваем элементы *)
    let rec compare_elements = function
      | Leaf -> 0
      | Node { value = (x, _); left; right; _ } ->
        let cmp = cmp_fun x x in
        if cmp <> 0 then cmp
        else
          let left_cmp = compare_elements left in
          if left_cmp <> 0 then left_cmp else compare_elements right
    in
    compare_elements a
