type 'e t = (bool list, 'e) Answer.t [@@deriving show]

val of_list : bool list -> 'e t
val of_int : size:int -> int -> 'e t

module DSL : sig
  type t = string * (bool * string) list [@@deriving yojson]
  val t : string -> bool * string
  val f : string -> bool * string
end
