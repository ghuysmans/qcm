open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React


let () =
  let open Dom_html in
  ignore @@ Dom_events.listen document Event.domContentLoaded (fun _ _ ->
    let l, s = Qcm_web.Multiple.simple Questions.list in
    let o =
      S.merge (fun acc x -> acc +. Qcm.Grader.negative x) 0. s |>
      S.map (Printf.sprintf "score: %g") |>
      R.Html.txt
    in
    let t = Html.(div (List.append l [o])) |> To_dom.of_element in
    ignore @@ Dom_html.document##.body##appendChild (t :> Dom.node Js.t);
    true
  )
