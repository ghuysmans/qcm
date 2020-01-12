type ('a, 'b) t =
  | Abstention
  | Cancelled
  | Filled of ('a, 'b) result
  [@@deriving show]

val map : ('a -> ('b, 'c) result) -> ('a, 'c) t -> ('b, 'c) t
val join : ('a, 'b) t list -> ('a list, 'b list) t
val ( >> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
