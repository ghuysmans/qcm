type 'a t = (int, [> `More_than_one] as 'a) Answer.t

val decode : 'a Multiple.t -> 'a t
