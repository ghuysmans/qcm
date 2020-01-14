open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React

let () =
  let open Dom_html in
  ignore @@ Dom_events.listen document Event.domContentLoaded (fun _ _ ->
    let answers =
      [true, "bim"; true, "bam"; true, "boum"; false, "banana"] |>
      List.map (fun (v, x) -> v, Html.[txt x])
    in
    let l, s = Qcm_web.Multiple.make [Html.txt "et ça fait..."] answers in
    let show t = Qcm.Grader.negative t |> Printf.sprintf "score: %g" in
    let o = R.Html.txt (S.map show s) in
    let t = Html.(div [l; o]) |> To_dom.of_element in
    ignore @@ Dom_html.document##.body##appendChild (t :> Dom.node Js.t);
    true
  )
