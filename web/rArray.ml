open React

let make length init =
  let e, f = E.create () in
  let s =
    S.fold (fun t (i, x) ->
      let t' = Array.copy t in
      t'.(i) <- x;
      t'
    ) (Array.make length init) e
  in
  s, f
