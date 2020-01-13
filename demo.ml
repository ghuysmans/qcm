open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React

let grade ~good ~bad ~abs ?(eq=(=)) expected = function
  | Qcm.Answer.Abstention | Cancelled -> abs
  | Filled (Ok given) when eq given expected -> good
  | Filled _ -> bad

let () =
  let open Dom_html in
  ignore @@ Dom_events.listen document Event.domContentLoaded (fun _ _ ->
    let answers =
      ["bim"; "bam"; "boum"; "banana"] |>
      List.map (fun x -> Html.[txt x])
    in
    let l, s = Qcm_web.Multiple.make answers in
    let l = Html.(List.map (fun x -> li [x]) l |> ul) in
    let _dump = Qcm.Multiple.show (fun _ () -> ()) in
    let ans = [true; true; true; false] in
    let show t =
      grade ~good:1 ~bad:(-1) ~abs:0 ans t |>
      Printf.sprintf "score: %d"
    in
    let o = R.Html.txt (S.map show s) in
    let t = Html.(div [p [txt "et ça fait..."]; l; o]) |> To_dom.of_element in
    ignore @@ Dom_html.document##.body##appendChild (t :> Dom.node Js.t);
    true
  )
