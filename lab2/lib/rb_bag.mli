open Rb_bag_initialization

type 'a t = 'a rb_node

val empty : 'a t (* пустое множ - нейтральный элемент моноида *)

val is_empty : 'a t -> bool

val size : 'a t -> int (* кол-во элементов в мультисета сумма count_i по всем элементам*)

val count : 'a -> 'a t -> int (* кол-во вхождений элемента в мультисете *)

val unique_count : 'a t -> int (* кол-во значений в мультисете (вне зависимости от числа повторений)*)

val contains : 'a -> 'a t -> bool

val add : 'a -> 'a t -> 'a t

val remove : 'a -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(* моноид *)

val union : 'a t -> 'a t -> 'a t

(* утилиты *)
val to_list : 'a t -> 'a list (* каждый элемент повторяется согласно кратности *)

val of_list : 'a list -> 'a t

val equal : 'a t -> 'a t -> bool

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val iter : ('a -> unit) -> 'a t -> unit

val all : ('a -> bool) -> 'a t -> bool

val any : ('a -> bool) -> 'a t -> bool
