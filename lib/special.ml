type 'a t = (bool list, [> `Combined_special] as 'a) Answer.t

let decode special x =
  Answer.map (fun l ->
    match List.rev l with
    | true :: t ->
      if List.exists (fun x -> x) t then
        Error `Combined_special
      else
        Ok (List.map (fun _ -> special) t)
    | false :: t ->
      Ok (List.rev t)
    | [] ->
      failwith "empty answer set"
  ) x

(* TODO build some UI, too! *)
let or_none_above x = decode false x
let or_all_above x = decode true x
