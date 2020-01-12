type 'a t = (bool list, [> `Combined_special] as 'a) Answer.t

val or_none_above : 'a Multiple.t -> 'a t
val or_all_above : 'a Multiple.t -> 'a t
