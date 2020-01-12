(* FIXME mli *)

(* FIXME variance annotations *)
type ('a, 'b) t =
  | Abstention
  | Cancelled
  | Filled of ('a, 'b) result
  [@@deriving show]

let map f = function
  | Abstention -> Abstention
  | Cancelled -> Cancelled
  | Filled (Ok x) -> Filled (f x)
  | Filled (Error e) -> Filled (Error e)
