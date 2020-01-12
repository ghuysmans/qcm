type 'e t = (bool list, 'e) Answer.t [@@deriving show]

let decode = function
  | [] -> failwith "empty answer set"
  | [_] -> failwith "unit answer set"
  | l ->
    if List.for_all (fun x -> x) l then
      Answer.Cancelled
    else if List.for_all (fun x -> not x) l then
      Abstention
    else
      Filled (Ok l)
