open Js_of_ocaml
module Lk = Lkprover

(* result monad *)
let ( let* ) r f =
  match r with
  | Ok x -> f x
  | Error e -> Error e
;;

let return x = Ok x

let () =
  let ok value =
    object%js
      val ok = Js._true
      val value = value
      val error = Js.Optdef.empty
    end
  in
  let error msg =
    object%js
      val ok = Js._false
      val value = Js.Optdef.empty
      val error = Js.string msg |> Js.Optdef.return
    end
  in
  Js.export_all
    (object%js
       method prove str =
         (let str = Js.to_string str in
          let* seq = Lk.Parse.from_string str in
          let* deriv =
            Lk.Prover.prove seq |> Option.to_result ~none:"Not provable in LK"
          in
          let latex = Format.asprintf "%a" Lk.Latex.pp_deriv_bussproof deriv in
          return (Js.string latex |> Js.Optdef.return))
         |> Result.fold ~ok ~error
    end)
;;
