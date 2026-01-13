# Property-Based Testing (PBT)

## Что это такое?

**Property-Based Testing (PBT)** — это подход к тестированию, где вместо написания конкретных примеров (unit-тестов) мы описываем **свойства**, которые должны выполняться для **всех возможных** входных данных.

### Основные концепции:

1. **Генерация данных**: Библиотека (QCheck) автоматически генерирует случайные входные данные
2. **Проверка свойств**: Для каждого сгенерированного набора данных проверяется, что свойство выполняется
3. **Shrinking**: Если свойство не выполняется, библиотека пытается найти минимальный пример, который нарушает свойство

### Преимущества PBT:

- ✅ **Автоматическая генерация тестовых данных** — не нужно писать множество примеров вручную
- ✅ **Находит неожиданные edge cases** — проверяет тысячи комбинаций, которые можно пропустить
- ✅ **Меньше кода** — описываешь свойство один раз, а не множество примеров
- ✅ **Shrinking** — при ошибке автоматически находит минимальный пример, который её вызывает

### Недостатки:

- ⚠️ Тесты могут быть медленнее (генерируется много данных)
- ⚠️ Сложнее отлаживать (нужно понимать, почему свойство не выполняется)
- ⚠️ Нужно правильно формулировать свойства

## Примеры свойств

### Пример 1: Ассоциативность

Вместо проверки конкретных примеров:
```ocaml
(* Unit-тест *)
let test_union_assoc () =
  let a = Rb_bag.of_list [1; 2] in
  let b = Rb_bag.of_list [3; 4] in
  let c = Rb_bag.of_list [5; 6] in
  assert (union (union a b) c = union a (union b c))
```

Мы описываем свойство:
```ocaml
(* Property-based тест *)
let prop_union_assoc =
  QCheck.Test.make ~name:"union is associative"
    QCheck.(triple (list int) (list int) (list int))
    (fun (a, b, c) ->
      let bag_a = Rb_bag.of_list a in
      let bag_b = Rb_bag.of_list b in
      let bag_c = Rb_bag.of_list c in
      let left = Rb_bag.union (Rb_bag.union bag_a bag_b) bag_c in
      let right = Rb_bag.union bag_a (Rb_bag.union bag_b bag_c) in
      Rb_bag.equal left right)
```

QCheck автоматически сгенерирует множество случайных списков `a`, `b`, `c` и проверит свойство для каждого.

## Как запускать Property-Based тесты

### 1. Через Dune (рекомендуется)

```bash
# Запустить все property-based тесты
dune exec test/test_properties.exe

# С дополнительными опциями
dune exec test/test_properties.exe -- --verbose
dune exec test/test_properties.exe -- --seed 12345  # фиксированный seed для воспроизводимости
```

### 2. Через Dune с количеством тестов

```bash
# Запустить с указанием количества тестов (по умолчанию 100)
dune exec test/test_properties.exe -- --tests 1000
```

### 3. Через utop (интерактивно)

```bash
# Запустить utop
dune utop

# В utop:
# #use "test/test_properties.ml";;
# QCheck_runner.run_tests_main [prop_union_assoc];;
```

### 4. Через ocamlrun (если скомпилировано)

```bash
# После компиляции
_build/default/test/test_properties.exe
```

## Структура property-based тестов в проекте

### Файл: `test/test_properties.ml`

Содержит 15 property-based тестов, проверяющих:

#### Моноид свойства:
1. **Ассоциативность union**: `(a union b) union c = a union (b union c)`
2. **Идентичность**: `empty union a = a union empty = a`

#### Базовые операции:
3. **Add и Remove обратны**: `count(x, remove(add(x, bag))) = count(x, bag)`
4. **Размер после add**: `size(add(x, bag)) = size(bag) + 1`
5. **Размер после remove**: `size(remove(x, bag)) = size(bag) - 1` (если элемент был)
6. **Count после add**: `count(x, add(x, bag)) = count(x, bag) + 1`
7. **Count после remove**: `count(x, remove(x, bag)) = count(x, bag) - 1` (если элемент был)

#### Функциональные операции:
8. **Map сохраняет размер**: `size(map(f, bag)) = size(bag)`
9. **Filter уменьшает размер**: `size(filter(p, bag)) <= size(bag)`
10. **Fold для коммутативных операций**: `fold_left (+) 0 bag = fold_right (+) bag 0`
11. **To_list и of_list**: `length(to_list(of_list(lst))) = size(of_list(lst))`

#### Валидность дерева:
12. **Валидность после add**: дерево остается валидным после добавления
13. **Валидность после remove**: дерево остается валидным после удаления
14. **Валидность после union**: дерево остается валидным после объединения
15. **Валидность после map**: дерево остается валидным после map

## Примеры вывода

### Успешный запуск:
```
random seed: 503806122
================================================================================
success (ran 15 tests)
```

### При ошибке:
```
random seed: 91627333

--- Failure --------------------------------------------------------------------

Test tree valid after filter failed (272 shrink steps):

[2; 4; 5]
================================================================================
failure (1 tests failed, 0 tests errored, ran 16 tests)
```

QCheck автоматически нашел минимальный пример `[2; 4; 5]`, который нарушает свойство.

## Настройка генераторов

В файле `test_properties.ml` используются генераторы:

```ocaml
(* Генератор списков целых чисел *)
let int_list_arb = QCheck.(list int)

(* Генератор одного целого числа *)
let int_arb = QCheck.int
```

Можно настроить генераторы для более специфичных случаев:

```ocaml
(* Список из 0-50 элементов, числа от -100 до 100 *)
let int_list_arb = QCheck.(list_size (int_range 0 50) (int_range (-100) 100))

(* Только положительные числа *)
let positive_int_arb = QCheck.(int_range 1 1000)
```

## Полезные команды

```bash
# Запустить все тесты (unit + property-based)
dune runtest

# Запустить только property-based тесты
dune exec test/test_properties.exe

# Запустить только unit-тесты
dune exec test/test_lab2.exe

# Запустить с подробным выводом
dune exec test/test_properties.exe -- --verbose

# Запустить с фиксированным seed (для воспроизводимости)
dune exec test/test_properties.exe -- --seed 12345

# Запустить больше тестов (по умолчанию 100 на свойство)
dune exec test/test_properties.exe -- --tests 1000

# Отключить цвета в выводе
dune exec test/test_properties.exe -- --no-colors

# Включить backtraces (для отладки)
dune exec test/test_properties.exe -- -bt

# Запустить длинные тесты (больше итераций)
dune exec test/test_properties.exe -- --long
```

### Доступные опции QCheck:

- `--verbose` / `-v` — подробный вывод
- `--seed <число>` / `-s <число>` — фиксированный seed для воспроизводимости
- `--tests <число>` — количество тестов на каждое свойство (по умолчанию 100)
- `--long` — запустить длинные тесты (больше итераций)
- `--colors` / `--no-colors` — включить/отключить цвета
- `-bt` — включить backtraces для отладки
- `--debug-shrink <файл>` — сохранить процесс shrinking в файл

## Сравнение с Unit-тестами

| Аспект | Unit-тесты | Property-Based тесты |
|--------|------------|---------------------|
| **Данные** | Конкретные примеры | Автоматически генерируются |
| **Покрытие** | Ограничено примерами | Широкое покрытие |
| **Написание** | Много примеров | Одно свойство |
| **Отладка** | Легко | Сложнее (shrinking помогает) |
| **Скорость** | Быстро | Медленнее (много тестов) |
| **Когда использовать** | Конкретные случаи, edge cases | Математические свойства, инварианты |

## Рекомендации

1. **Используй оба подхода**: Unit-тесты для конкретных случаев, PBT для свойств
2. **Начинай с простых свойств**: Ассоциативность, идентичность, инварианты
3. **Используй shrinking**: QCheck автоматически находит минимальный пример при ошибке
4. **Фиксируй seed при отладке**: Используй `--seed` для воспроизводимости
5. **Проверяй инварианты**: После каждой операции структура должна оставаться валидной

## Дополнительные ресурсы

- [QCheck Documentation](https://github.com/c-cube/qcheck)
- [Property-Based Testing Tutorial](https://hypothesis.works/articles/what-is-property-based-testing/)
- [QuickCheck (оригинальная библиотека для Haskell)](https://hackage.haskell.org/package/QuickCheck)

