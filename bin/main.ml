open Lkprover

let main =
  let seq = Parse.from_channel stdin in
  let deriv = Prover.prove seq in
  (match deriv with
  | Some deriv -> Lk.pp_derivation Format.std_formatter deriv
  | None -> print_string "Not provable");
  print_newline ()
