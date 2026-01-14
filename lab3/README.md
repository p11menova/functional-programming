
## лабораторная работа №3 - реализация потоковых алгоритмов интерполяции
### выполнила Пименова Екатерина, 409342

## Задание
1. **Алгоритмы интерполяции:**
   - Линейная интерполяция (обязательно)
   - Интерполяция Лагранжа 

2. **Потоковая обработка данных:**
   - Входные данные поступают на стандартный ввод в формате CSV (x;y, x\ty или x y)
   - Выходные данные выводятся на стандартный вывод
   - Программа работает в потоковом режиме (ожидает данные на stdin)

3. **Настройки через командную строку:**
   - Выбор алгоритмов интерполяции (--linear, --lagrange)
   - Частота дискретизации (--step <шаг> или --n <количество точек>)
   - Размер окна для групповых алгоритмов (--window <размер>)

4. **Скользящее окно:**
   - Для алгоритмов, работающих с группой точек, используется скользящее окно
   - Большинство окон используется для расчёта одной точки (в центре)
   - Первое и последнее окна используются для расчёта большего количества точек

напоминашка про алгоритмы интерполяции

#### линейная интерполяция — это метод построения промежуточных значений между двумя известными точками по прямой линии.

**формула:**
```
y = y1 + (y2 - y1) / (x2 - x1) * (x - x1)
```

где (x1, y1) и (x2, y2) — известные точки, x — точка, для которой вычисляется значение.


#### интерполяция Лагранжа - метод интерполяции с использованием полинома Лагранжа для построения интерполяционного полинома.

**базисные полиномы Лагранжа:**

для каждой точки (x_i, y_i) базисный полином:
```
L_i(x) = Π(j=0 to n, j≠i) (x - x_j) / (x_i - x_j)
```

**интерполяционный полином Лагранжа:**

```
P(x) = Σ(i=0 to n) y_i * L_i(x)
```

### архитектура моей лабораторной работы

#### структура модулей

```
lab3/
├── lib/
│   ├── interpolation.ml     # реализация самих алгоритмов интерполяции
│   ├── stream.ml            # потоковая обработка данных
│   ├── window.ml            # скользящее окно
│   └── args.ml              # парсинг аргументов командной строки
└── bin/
    └── main.ml             
```

### пайплайн обработки потока данных

```
Стандартный ввод (stdin)
    |
    v
[Парсинг точек] (stream.ml)
    |
    v
[Скользящее окно] (window.ml)
    |
    v
[Алгоритм интерполяции] (interpolation.ml)
    |
    v
[Генератор точек] (stream.ml)
    |
    v
[Форматирование вывода] (stream.ml)
    |
    v
Стандартный вывод (stdout)
```

### Ключевые элементы реализации

#### 1. модуль интерполяции (lib/interpolation.ml)

**линейная интерполяция**

```ocaml
let linear_interpolation points x =
  let rec find_segment = function
    | [] -> None
    | [ (x', y') ] -> if x = x' then Some ((x', y'), (x', y')) else None
    | (x1, y1) :: (x2, y2) :: rest ->
        if x >= x1 && x <= x2 then Some ((x1, y1), (x2, y2))
        else find_segment ((x2, y2) :: rest)
  in
  match find_segment points with
  | None -> None
  | Some ((x1, y1), (x2, y2)) -> 
      Some (linear_interpolate (x1, y1) (x2, y2) x)
```

**описание:**
- Находит отрезок, в который попадает точка x
- Использует рекурсивный поиск по отсортированному списку точек
- Возвращает None, если точка вне диапазона

**интерполяция Лагранжа**

```ocaml
let lagrange_interpolation points x =
  let sorted_points = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) points in
  
  let result = ref 0.0 in
  
  List.iteri
    (fun i (xi, yi) ->
      (* вычисляем L_i(x) = Π(j≠i) (x - x_j) / (x_i - x_j) *)
      let li = ref 1.0 in
      List.iteri
        (fun j (xj, _) ->
          if i <> j then (
            let denominator = xi -. xj in
            if denominator <> 0.0 then
              li := !li *. ((x -. xj) /. denominator)))
        sorted_points;
      (* добавляем y_i * L_i(x) к результату *)
      result := !result +. (yi *. !li))
    sorted_points;
  
  Some !result
```

**описание:**
- Вычисляет базисные полиномы Лагранжа для каждой точки
- Суммирует произведения y_i * L_i(x)
- Работает с произвольным количеством точек (минимум 2)

### 2. модуль скользящего окна (lib/window.ml)

```ocaml
type window = {
  points : point list;
  max_size : int;
}

let add_point window point =
  let new_points = point :: window.points in
  let sorted_points = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) new_points in
  let trimmed_points =
    if List.length sorted_points > window.max_size then
      take window.max_size sorted_points
    else sorted_points
  in
  { window with points = trimmed_points }
```

**Описание:**
- хранит ограниченное количество точек (max_size)
- автоматически сортирует точки по x
- ужаляет старые точки при превышении размера

### 3. модуль потоковой обработки (lib/stream.ml)

#### парсинг входных данных
я решила что ваще по любому у меня можно вводить точки, так что поддерживается ввод и через ; и через \t и через пробел
```ocaml
let parse_point line =
  let parts =
    if String.contains line ';' then String.split_on_char ';' line
    else if String.contains line '\t' then String.split_on_char '\t' line
    else String.split_on_char ' ' line
  in
  match parts with
  | [ x_str; y_str ] ->
      let x = float_of_string (String.trim x_str) in
      let y = float_of_string (String.trim y_str) in
      Some (x, y)
  | _ -> None
```

#### генерация точек для интерполяции

```ocaml
(** генератор точек для интерполяции на основе шага *)
let generate_points x_min x_max step =
  let rec gen acc current =
    if current > x_max then List.rev acc
    else gen (current :: acc) (current +. step)
  in
  gen [] x_min

(** генератор точек для интерполяции на основе количества *)
let generate_n_points x_min x_max n =
  if n <= 1 then [ x_min ]
  else
    let step = (x_max -. x_min) /. float_of_int (n - 1) in
    List.init n (fun i -> x_min +. (float_of_int i *. step))
```

### 4. main (bin/main.ml)

**основной цикл обработки**

```ocaml
let rec process_line windows =
  try
    let line = read_line () in
    match Stream.parse_point line with
    | None -> process_line windows
    | Some point ->
        let new_windows, all_outputs =
          List.fold_left
            (fun (acc_windows, acc_outputs) (alg, window) ->
              let new_window, outputs =
                match alg with
                | Args.Linear -> process_linear config window point
                | Args.Lagrange -> process_lagrange config window point
              in
              ((alg, new_window) :: acc_windows, outputs @ acc_outputs))
            ([], []) windows
        in
        List.iter print_endline all_outputs;
        flush stdout;
        process_line (List.rev new_windows)
  with End_of_file ->
    (* Обработка последних точек *)
    ...
```

**Описание:**
- читает данные построчно из stdin
- обрабатывает каждую точку через все выбранные алгоритмы
- выводит результаты сразу после обработки (потоковый режим)
- обрабатывает последние точки при EOF

### использование программы

#### примеры запуска

1. **линейная интерполяция с шагом 0.7:**
```bash
echo -e "0 0\n1 1\n2 2" | dune exec lab3 -- --linear --step 0.7
```

2. **интерполяция Лагранжа с 4 точками в окне:**
```bash
echo -e "0 0\n1 1\n2 2\n3 3\n4 4" | dune exec lab3 -- --lagrange -n 4 --step 0.5
```

3. **интерполяция Лагранжа:**
```bash
cat data.csv | dune exec lab3 -- --lagrange --window 5 --step 0.1
```
4. **линейная интерполяция с дефолтным шагом (0.2):**
```bash
dune exec lab3 -- --linear
<0 0                                 
<1 1 
>linear: 0.0000 0.0000
>linear: 0.2000 0.2000
>linear: 0.4000 0.4000
>linear: 0.6000 0.6000
>linear: 0.8000 0.8000
>linear: 1.0000 1.0000
```
5. **интерполяция Лагранжа - потоковая обработка**
```bash

dune exec lab3 -- --lagrange --step 0.5   
<0 0                                 
<1 1
<2 2 
<3 4 
>lagrange: 1.5000 1.4375
<5 6
>lagrange: 1.5000 1.4375
<7 8
>lagrange: 1.5000 1.4375
<1 10
>lagrange: 1.0000 11.0000
```