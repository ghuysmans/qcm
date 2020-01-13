open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React

let select l =
  let open Html in
  let s, f = S.create Qcm.Answer.Abstention in
  ("" :: l) |>
  List.map (fun t -> option (txt t)) |>
  select ~a:[a_required (); a_onchange (fun e ->
    Js.Opt.iter e##.target (fun t ->
      Js.Opt.iter (Dom_html.CoerceTo.select t) @@ fun t ->
        if t##.selectedIndex = 0 then
          f Qcm.Answer.Abstention
        else
          f (Qcm.Answer.Filled (Ok t##.selectedIndex))
    );
    true
  )],
  s

let range abs_lbl min max =
  let a_s, a_f = S.create false in (* answered? *)
  let v_s, v_f = S.create min in (* value *)
  Html.[
    label [
      txt abs_lbl;
      input ~a:[a_input_type `Checkbox; a_onclick (fun e ->
        Js.Opt.iter e##.target (fun t ->
          Js.Opt.iter (Dom_html.CoerceTo.input t) @@ fun t ->
            a_f (Js.to_bool t##.checked)
        );
        true
      )] ()
    ];
    input ~a:[
      a_input_type `Range;
      a_input_min (`Number min);
      a_input_max (`Number max);
      a_onchange (fun e ->
        Js.Opt.iter e##.target (fun t ->
          Js.Opt.iter (Dom_html.CoerceTo.input t) (fun t ->
            a_f true; (* TODO update the view *)
            v_f (Js.to_string t##.value |> int_of_string)
          )
        );
        true
      )
    ] ()
  ],
  S.bind a_s @@ function
    | true -> v_s |> S.map (fun v -> Qcm.Answer.Filled (Ok v))
    | false -> S.const Qcm.Answer.Abstention
