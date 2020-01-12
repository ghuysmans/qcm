type 'e t = 'e Number.t

let decode x =
  Answer.map (fun l ->
    (* that zero at the end is ignored, but allows cancelling/abstention *)
    Ok (List.fold_left (fun acc b -> acc * 2 + if b then 1 else 0) 0 l / 2)
  ) x

let b = Multiple.of_int ~size:3
let%test _ = decode (b 0b000) = Answer.Abstention
let%test _ = decode (b 0b001) = Answer.Filled (Ok 0b00)
let%test _ = decode (b 0b010) = Answer.Filled (Ok 0b01)
let%test _ = decode (b 0b011) = Answer.Filled (Ok 0b01)
let%test _ = decode (b 0b100) = Answer.Filled (Ok 0b10)
let%test _ = decode (b 0b101) = Answer.Filled (Ok 0b10)
let%test _ = decode (b 0b110) = Answer.Filled (Ok 0b11)
let%test _ = decode (b 0b111) = Answer.Cancelled
(*
TODO use or_none_above!
let%test _ = decode (b 0b011) = Answer.Filled (Error `Combined_special)
let%test _ = decode (b 0b101) = Answer.Filled (Error `Combined_special)
*)
