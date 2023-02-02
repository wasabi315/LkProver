open Format
open Js_of_ocaml
module Lk = Lkprover

(* result monad *)
let ( let* ) r f =
  match r with
  | Ok x -> f x
  | Error e -> Error e
;;

let return x = Ok x

let obj_of_result = function
  | Ok value ->
    object%js
      val ok = Js._true
      val value = Js.Optdef.return value
      val error = Js.Optdef.empty
    end
  | Error msg ->
    object%js
      val ok = Js._false
      val value = Js.Optdef.empty
      val error = Js.string msg |> Js.Optdef.return
    end
;;

let () =
  Js.export_all
    (object%js
       method prove str =
         (let str = Js.to_string str in
          let* seq = Lk.Parse.from_string str in
          let* deriv =
            Lk.Prover.prove seq |> Option.to_result ~none:"Not provable in LK"
          in
          let latex = asprintf "%a" Lk.Latex.pp_deriv_bussproof deriv in
          return @@ Js.string latex)
         |> obj_of_result
    end)
;;
