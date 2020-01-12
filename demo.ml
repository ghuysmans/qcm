open Qcm

let bin_of_string = function
  | "0" -> false
  | "1" -> true
  | s -> failwith @@ Printf.sprintf "binary: unexpected %S" s

let () =
  let a = Array.to_list Sys.argv |> List.tl |> List.map bin_of_string in
  if true then
    Multiple.decode a |> Number.binary |>
    Format.printf "%a@." (Number.pp (fun _ () -> ()))
  else if true then
    let pp = Answer.pp Format.pp_print_int (fun fmt -> function
      | `More_than_one -> Format.pp_print_string fmt "multiple"
    ) in
    Format.printf "%a@." pp (Single.decode (Multiple.decode a))
  else if true then
    let pp = Multiple.pp (fun fmt -> function
      | `Combined_special -> Format.pp_print_string fmt "combined"
    ) in
    Format.printf "%a@." pp (Special.or_all_above (Multiple.decode a))
  else
    Format.printf "%a@." (Multiple.pp (fun _ () -> ())) (Multiple.decode a)
