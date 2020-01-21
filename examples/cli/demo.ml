let ask (title, answers) =
  Printf.printf "%s\n" title;
  let given =
    List.fold_left (fun (i, acc) (_, a) ->
      (* Printf.printf "%d. %s\n" i a; *)
      Printf.printf "%s ? (y/n) %!" a;
      let ans =
        match Scanf.scanf "%s\n" (fun x -> x) with
        | "y" | "o" | "1" -> true
        | _ -> false
      in
      i + 1, ans :: acc
    ) (1, []) answers |> snd |>
    List.rev
  in
  Printf.printf "\n";
  let expected = List.map fst answers in
  Qcm.Multiple.of_list given |>
  Qcm.Status.of_answer ((=) expected)

let load fn =
  let ch =
    match fn with
    | "-" -> stdin
    | _ -> open_in fn
  in
  Qcm_moodle.parse ch


let () =
  match Sys.argv with
  | [| _ |] ->
    List.map ask Questions.list |>
    Qcm.Grader.(grade negative) |>
    Printf.printf "score: %g\n"
  | [| _; fn |] ->
    let data = load fn in
    (try
      while true do
        let i = Random.int (List.length data) in
        let q = List.nth data i in
        Printf.printf "%s\na) %s\nb) %s\nc) %s\nd) %s\ne) %s\n? %!"
          q.amorce q.ans_a q.ans_b q.ans_c q.ans_d q.ans_e;
        if Scanf.scanf "%s\n" String.uppercase_ascii = q.bonne then
          Printf.printf "bien !\n\n"
        else (
          let bonne_s =
            (Char.code q.bonne.[0] - Char.code 'A') |>
            List.nth (snd (Qcm_moodle.multiple_of_question q)) |>
            snd
          in
          Printf.printf "Non. Selon le chapitre %g, c'était %s : %s\n%s\n\n"
            q.chapitre
            q.bonne
            bonne_s
            q.justification;
          Printf.printf "ok ? %!";
          ignore @@ read_line ();
          Printf.printf "\n"
        )
      done
    with End_of_file ->
      ())
  | [| _; "-d"; fn |] ->
    let data = load fn in
    Qcm_moodle.to_yojson data |> Yojson.Safe.to_channel stdout;
    Printf.eprintf "total %d\n" (List.length data);
  | _ ->
    Printf.eprintf "usage: %s [-m]\n" Sys.argv.(0);
    exit 1
