type t = Status.t -> float

let make ~good ~bad ~abs = function
  | Status.Valid -> good
  | Invalid -> bad
  | Abstention -> abs

let positive = make ~good:1. ~bad:0. ~abs:0.
let negative = make ~good:1. ~bad:(-1.) ~abs:0.
let sbpm = make ~good:5. ~bad:0. ~abs:2.
