# эссе почему я хочу изучать OCaml в рамках курса ФП

главной причиной выбора курса функционального программирования на этот семестр для меня было желание познакомиться с новой для меня парадигмой - функциональному подходу к программированию.

и на мой взгляд язык OCaml отлично подходит для этих целей. 

> OCaml does a great job
of clarifying and simplifying the essence of functional programming in a way that other languages that blend functional
and imperative programming (like Scala) or take functional programming to the extreme (like Haskell) do not.

### какие плюсы OCaml'a я для себя выделила в процессе выбора языка:
  * __Несложность__ *(в сравнении с, например, lisp-подобными языками)* __синтаксиса__ позволяет сфокусироваться на самой сути фукционального подхода, а не на количестве незакрытых скобок в выражении;
  * __Статическая строгая типизация и безопасность типов__ позволяют избегать ошибок типов еще на этапе компиляции, что упрощает отладку и поиск багов в коде;
  * Реализация лучших __идей ООП__ (инкапсуляция, абстракции, полиморфизм) через модульную систему, а не классы - (*вот такой прикольный микс уже знакомого и еще нет*);
  * __Сборка мусора__;
  * __Вывод типов__ - компилятор сам понимает, с каким типом данных взаимодействует (let add x y = x + y - *компилятор сам понимает, что add имеет тип int -> int -> int)*;
  * __Алгебраические типы данных__, позволяющие создавать сложные структуры данных без указателей и ручного управления памятью;
  * __Высокая производительность__ нативного кода;
  * название окамль мне изначально понравилось больше остальных.


### учебные материалы:
1. __книга [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/ocaml_programming.pdf)__;
   > "This textbook, used in a third-semester Cornell course, teaches functional programming and data structures in OCaml,
   >  emphasizing semantics and software engineering. Suitable for students with Python and Java backgrounds,
   >  it requires some imperative language skills and basic discrete mathematics." - вот какие замечательные слова про нее написаны на сайте ocaml.org
2. __[плейлист на ютубе](https://www.youtube.com/playlist?list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU) по этой же книге__;
3. __[упражения с сайта ocaml.org](https://ocaml.org/exercises)__ при недостатке практики в пределах курса.

### инструментарий:
это все я украла с уже полюбившегося мне сайта ocaml.org 

джентельменский набор:

- [UTop](https://github.com/ocaml-community/utop), a modern interactive toplevel (REPL: Read-Eval-Print Loop)
- [Dune](https://dune.build/), a fast and full-featured build system
- [ocaml-lsp-server](https://github.com/ocaml/ocaml-lsp) implements the Language Server Protocol to enable editor support for OCaml, e.g., in VS Code, Vim, or Emacs.
- [odoc](https://github.com/ocaml/odoc) to generate documentation from OCaml code
- [OCamlFormat](https://opam.ocaml.org/packages/ocamlformat/) to automatically format OCaml code

### что насчет 4 лабы:
пока что никакой конкретной идеи нет. но есть желание сделалать что-то максимально характерное для OСaml'a: нечто в сфере финтеха или формальной грамматики (DSL), но это все только идеи. основная моя задача на 4 лабу - максимально прощупать возможности функционального программирования на окамле.

### автор эссе:
#### с уважением, Пименова Екатерина, P3313 🐫 

