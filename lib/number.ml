type 'e t = (int, 'e) Answer.t [@@deriving show]

let binary x =
  Answer.map (fun l ->
    (* that zero at the end is ignored, but allows cancelling/abstention *)
    Ok (List.fold_left (fun acc b -> acc * 2 + if b then 1 else 0) 0 l / 2)
  ) x

(* TODO decimal, beware of unwanted abstention *)
