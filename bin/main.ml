open Lkprover

let main =
  let args = ref [] in
  let () = Arg.parse [] (fun arg -> args := arg :: !args) "help" in
  let seq =
    match !args with
    | [] -> Parse.from_channel stdin
    | input :: _ -> Parse.from_string input
  in
  let deriv = Prover.prove seq in
  match deriv with
  | Some deriv -> Format.printf "%a" Latex.pp_deriv_bussproof deriv
  | None ->
      Format.eprintf "Not provable in LK";
      exit 1
