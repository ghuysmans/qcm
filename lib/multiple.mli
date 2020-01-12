type 'e t = (bool list, 'e) Answer.t [@@deriving show]

val of_list : bool list -> 'e t
val of_int : size:int -> int -> 'e t
