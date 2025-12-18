(** initialization of red-black tree structure *)

type color =
  | Red
  | Black

type 'a rb_node =
  | Leaf
  | Node of {
      color : color;
      value : 'a * int;
          (** in case i implement multiset, i save value + its count*)
      left : 'a rb_node;
      right : 'a rb_node;
    }
