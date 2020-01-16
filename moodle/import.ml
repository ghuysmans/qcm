open Soup

type question = {
  amorce: string;
  (* trick to get the choices before the answer *)
  ans_a: string;
  ans_b: string;
  ans_c: string;
  ans_d: string;
  ans_e: string;
  bonne: string;
  chapitre: float; (* haha, someone wrote 4.4 *)
  justification: string; (* FIXME? *)
} [@@deriving yojson]

let questions_of_soup soup =
  let sel = ".mod-data-default-template > tbody" in
  delete (soup $ sel); (* the first one *)
  soup $$ sel |> to_list |>
  List.map (fun t ->
    match
      t $$ "> tr > td.lastcol > span" |> to_list |>
      List.map (fun span -> trimmed_texts span |> String.concat "")
    with
    | [amorce; ans_a; ans_b; ans_c; ans_d; ans_e;
       bonne; chapitre; justification] ->
      [{amorce; ans_a; ans_b; ans_c; ans_d; ans_e; bonne;
       chapitre = float_of_string chapitre;
       justification}]
    | l -> Printf.eprintf "%s\n" (List.hd l); []
  ) |>
  List.flatten

let multiple_of_question {amorce; ans_a; ans_b; ans_c; ans_d; ans_e; bonne; _} =
  amorce, [bonne = "A", ans_a;
           bonne = "B", ans_b;
           bonne = "C", ans_c;
           bonne = "D", ans_d;
           bonne = "E", ans_e]


type t = question list [@@deriving yojson]
type m = Qcm.Multiple.DSL.t list [@@deriving yojson] (* FIXME *)

let () =
  let data = read_channel stdin |> parse |> questions_of_soup in
  (match Sys.argv with
  | [| _ |] -> to_yojson data
  | [| _; "-m" |] -> List.map multiple_of_question data |> m_to_yojson
  | _ ->
    Printf.eprintf "usage: %s [-m]\n" Sys.argv.(0);
    exit 1) |>
  Yojson.Safe.to_channel stdout;
  Printf.eprintf "total %d\n" (List.length data);
