open Format
open Lkprover

let die =
  let k ppf =
    pp_print_newline ppf ();
    exit 1
  in
  fun fmt -> kfprintf k err_formatter fmt
;;

let main =
  let arg =
    if Array.length Sys.argv = 2
    then Sys.argv.(1)
    else die "Usage: %s SEQUENT" Sys.argv.(0)
  in
  let seq = Parse.from_string arg |> Result.fold ~ok:Fun.id ~error:(die "%s") in
  match Prover.prove seq with
  | Some deriv -> printf "%a" Latex.pp_deriv_bussproof deriv
  | None -> die "Not provable in LK"
;;
