type 'e t = (int, ([> `More_than_one] as 'e) list) Answer.t

let decode ?(base = 10) l =
  List.map Single.decode l |>
  Answer.join |>
  Answer.map (fun l ->
    Ok (List.fold_left (fun acc d -> acc * base + d) 0 l)
  )
