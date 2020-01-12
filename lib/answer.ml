(* FIXME mli *)

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

let%test _ = map (fun x -> Ok x) Abstention = Abstention
let%test _ = map (fun x -> Ok (x + 1)) (Filled (Ok 2)) = Filled (Ok 3)
let%test _ = map (fun x -> Ok (x + 1)) (Filled (Error "z")) = Filled (Error "z")

let join l =
  let f a b =
    match a, b with
    | Filled (Ok h), Filled (Ok t) -> Filled (Ok (h :: t))
    (* abstention, cancellation and errors have priority *)
    | _, Abstention | Abstention, _-> Abstention
    | _, Cancelled | Cancelled, _ -> Cancelled
    | Filled (Error h), Filled (Error t) -> Filled (Error (h :: t))
    | Filled (Error e), Filled (Ok _) -> Filled (Error [e])
    | Filled (Ok _), Filled (Error e) -> Filled (Error e)
  in
  match l with
  | [] -> failwith "empty answer set"
  (* just wrap Ok and Error *)
  | Abstention :: t -> List.fold_right f t Abstention
  | Cancelled :: t -> List.fold_right f t Cancelled
  | Filled (Ok x) :: t -> List.fold_right f t (Filled (Ok [x]))
  | Filled (Error e) :: t -> List.fold_right f t (Filled (Error [e]))

let%test _ = join [Abstention; Filled (Ok 2)] = Abstention
let%test _ = join [Filled (Ok 2); Abstention] = Abstention
let%test _ = join [Filled (Error "e"); Filled (Ok 2)] = Filled (Error ["e"])
let%test _ = join [Filled (Ok 2); Filled (Error "e")] = Filled (Error ["e"])
let%test _ = join [Filled (Ok 2); Filled (Ok 3)] = Filled (Ok [2; 3])

let (>>) a b =
  match a with
  | Cancelled -> b
  | _ -> a

let%test _ = Abstention >> Cancelled = Abstention
let%test _ = Cancelled >> Filled (Ok 1) = Filled (Ok 1)
let%test _ = Filled (Ok 1) >> Filled (Ok 2) = Filled (Ok 1)
let%test _ = Filled (Error "e") >> Filled (Ok 2) = Filled (Error "e")
