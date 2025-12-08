type 'a t

(* базовые операции с мультисетом *)

val empty : 'a t (* пустое множ - нейтральный элемент моноида *)
val is_empty : 'a t -> bool
val size : 'a t -> int
val unique_count : 'a t -> int

(* добавление/удаление *)
val add : 'a -> 'a t -> 'a t
val remove : 'a -> 'a t -> 'a t
(* возвращает новый мультисет с удаленным вхождением [x]. если элемента нет,
   возвращает исходный. *)

val remove_all : 'a -> 'a t -> 'a t
val count : 'a -> 'a t -> int
val contains : 'a -> 'a t -> bool

(* функц операции *)
val filter : ('a -> bool) -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(* моноид *)

val union :
  'a t ->
  'a t ->
  'a t (* объединяет два мультисета (ассоциативная операция моноида) *)

(* утилиты *)
val to_list :
  'a t -> 'a list (* каждый элемент повторяется согласно кратности *)
val of_list : 'a list -> 'a t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit

val all : ('a -> bool) -> 'a t -> bool
val any : ('a -> bool) -> 'a t -> bool
