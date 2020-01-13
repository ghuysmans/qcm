open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React

let make l =
  let options = Array.init (List.length l) (fun _ -> S.create false) in
  l |> List.mapi (fun index x ->
    Html.(label (input ~a:[a_input_type `Checkbox; a_onclick (fun e ->
      Js.Opt.iter e##.target (fun t ->
        Js.Opt.iter (Dom_html.CoerceTo.input t) @@ fun t ->
          (snd options.(index)) (Js.to_bool t##.checked)
      );
      true
    )] () :: x))
  ),
  Array.to_list options |> List.map fst |>
  S.merge (fun acc x -> x :: acc) [] |>
  S.map List.rev |>
  S.map Qcm.Multiple.of_list
