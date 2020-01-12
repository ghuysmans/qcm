type 'e t = (int, [> `Combined_special] as 'e) Answer.t

let decode x =
  Special.or_none_above x |>
  Answer.map (fun l ->
    Ok (List.fold_left (fun acc b -> acc * 2 + if b then 1 else 0) 0 l)
  )

let b = Multiple.of_int ~size:3
let%test _ = decode (b 0b000) = Answer.Abstention
let%test _ = decode (b 0b001) = Answer.Filled (Ok 0b00)
let%test _ = decode (b 0b010) = Answer.Filled (Ok 0b01)
let%test _ = decode (b 0b011) = Answer.Filled (Error `Combined_special)
let%test _ = decode (b 0b100) = Answer.Filled (Ok 0b10)
let%test _ = decode (b 0b101) = Answer.Filled (Error `Combined_special)
let%test _ = decode (b 0b110) = Answer.Filled (Ok 0b11)
let%test _ = decode (b 0b111) = Answer.Cancelled
