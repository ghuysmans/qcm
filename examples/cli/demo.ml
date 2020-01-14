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


let () =
  List.map ask Questions.list |>
  Qcm.Grader.(grade negative) |>
  Printf.printf "score: %g\n"
