module Test(Backend : Language.Semantics) = struct

  open Backend

  macro program () =
    let y = mkint 13 in
    let somechan = channel (Io (int, void)) in
    put_incoming somechan (mkint 4);
    put_incoming somechan (mkint 2);
    let factorial =
      fn int int (fun n ->
        iterate (integer_range (mkint 1) (n+mkint 1)) (mkint 1)
          (fun acc x ->
            x * acc))
    in
    let z = mkint 5 + (y * ?.somechan) + y in
    apply factorial (z - (mkint 60 + ?.somechan))

end

module Program = Test(Dyn)

let () = Printf.printf "%d\n" @@ $(Program.program ())
