type 'e t = (int, ([> `More_than_one] as 'e) list) Answer.t

val decode : ?base:int -> (bool list, 'e) Answer.t list -> 'e t
