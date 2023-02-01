open Js_of_ocaml
module Lk = Lkprover

let () =
  Js.export_all
    (object%js
       method prove str =
         let str = Js.to_string str in
         let seq = Lk.Parse.from_string str in
         let deriv = Lk.Prover.prove seq in
         match deriv with
         | Some deriv ->
           let latex = Format.asprintf "%a" Lk.Latex.pp_deriv_bussproof deriv in
           object%js
             val success = Js._true
             val data = Js.string latex |> Js.Optdef.return
             val error = Js.Optdef.empty
           end
         | None ->
           object%js
             val success = Js._false
             val data = Js.Optdef.empty
             val error = Js.string "Not provable in LK" |> Js.Optdef.return
           end
    end)
;;
