open Lkprover

let main =
  let seq = Parse.from_channel stdin in
  let deriv = Prover.prove seq in
  match deriv with
  | Some deriv -> Format.printf "%a" Latex.pp_deriv_bussproof deriv
  | None ->
      Format.eprintf "Not provable in LK";
      exit 1
