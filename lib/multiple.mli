type 'e t = (bool list, 'e) Answer.t [@@deriving show]

val decode : bool list -> 'e t
