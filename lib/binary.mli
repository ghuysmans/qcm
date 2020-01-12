type 'e t = (int, [> `Combined_special] as 'e) Answer.t

val decode : 'e Multiple.t -> 'e t
