type t =
  | Valid
  | Invalid
  | Abstention

let of_answer f = function
  | Answer.Abstention | Cancelled -> Abstention
  | Filled (Ok given) when f given -> Valid
  | Filled _ -> Invalid
