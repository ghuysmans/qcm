type 'e t = (bool list, 'e) Answer.t [@@deriving show]

let of_list = function
  | [] -> failwith "empty answer set"
  | [_] -> failwith "unit answer set"
  | l ->
    if List.for_all (fun x -> x) l then
      Answer.Cancelled
    else if List.for_all (fun x -> not x) l then
      Abstention
    else
      Filled (Ok l)

let%test _ = of_list [false; false] = Answer.Abstention
let%test _ = of_list [false; true] = Answer.Filled (Ok [false; true])
let%test _ = of_list [true; false] = Answer.Filled (Ok [true; false])
let%test _ = of_list [true; true] = Answer.Cancelled

let bits ~size n =
  let rec f size acc n =
    if size = 0 then
      acc
    else if n land 1 = 0 then
      f (size - 1) (false :: acc) (n / 2)
    else
      f (size - 1) (true :: acc) (n / 2)
  in
  f size [] n

let%test _ = bits ~size:2 0b00 = [false; false]
let%test _ = bits ~size:2 0b01 = [false; true]
let%test _ = bits ~size:2 0b10 = [true; false]
let%test _ = bits ~size:2 0b11 = [true; true]

let of_int ~size n = of_list (bits ~size n)
