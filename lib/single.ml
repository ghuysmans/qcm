type 'a t = (int, [> `More_than_one] as 'a) Answer.t

let index_of x =
  let rec f i = function
    | [] -> raise Not_found
    | x' :: _ when x = x' -> i
    | _ :: t -> f (i + 1) t
  in
  f 0

let decode x =
  Answer.map (fun l ->
    match List.find_all (fun x -> x) l with
    | [_] -> Ok (index_of true l)
    | _ -> Error `More_than_one
  ) x
