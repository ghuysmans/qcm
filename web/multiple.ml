open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React

let make question answers =
  let checked, update = RArray.make (List.length answers) false in
  let s = checked |> S.map (fun a -> Array.to_list a |> Qcm.Multiple.of_list) in
  Html.(div
    ~a:[R.Html.a_class (s |> S.map (function
      | Qcm.Answer.Cancelled -> ["cancelled"]
      | _ -> []
    ))]
    [
      p question;
      answers |> List.mapi (fun index (answer, x) ->
        Html.(li [label
          ~a:[R.Html.a_class (checked |> S.map (fun checked ->
            if checked.(index) = answer then ["valid"]
            else ["invalid"]
          ))]
          (input ~a:[
            a_input_type `Checkbox;
            a_onclick (fun e ->
              Js.Opt.iter e##.target (fun t ->
                Js.Opt.iter (Dom_html.CoerceTo.input t) @@ fun t ->
                  update (index, Js.to_bool t##.checked)
              );
              true
            )
          ] () :: x)
        ])
      ) |> Html.ul
    ]
  ),
  let expected = List.map fst answers in
  s |> S.map (Qcm.Status.of_answer ((=) expected))


let simple l =
  List.map (fun (question, l) ->
    Html.[txt question],
    List.map (fun (v, x) -> v, Html.[txt x]) l
  ) l |>
  List.map (fun (q, a) -> make q a) |>
  List.split
