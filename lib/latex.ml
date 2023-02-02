open Format
open Lk

module Op_prec = struct
  type t =
    | Bottom (* lowest precedence *)
    | Imp
    (* (p -> q) -> r
       ^^^^^^^^ Imp_r is for surrounding here (-> is not associative) *)
    | Imp_r
    | Or
    | And
    | Not (* highest precedence *)
  [@@deriving ord]

  let ( > ) p1 p2 = compare p1 p2 > 0
end

let pp_prop =
  let paren_if pred pp ppf =
    if pred then fprintf ppf "(";
    fprintf ppf "%t" pp;
    if pred then fprintf ppf ")"
  in
  let rec loop prec = function
    | Prop.Bottom -> dprintf "\\bot"
    | Sym s -> dprintf "%s" s
    | Not p -> dprintf "\\lnot %t" (loop Op_prec.Not p)
    | And (p1, p2) ->
      dprintf "%t \\land %t" (loop And p1) (loop And p2)
      |> paren_if Op_prec.(prec > And)
    | Or (p1, p2) ->
      dprintf "%t \\lor %t" (loop Or p1) (loop Or p2)
      |> paren_if Op_prec.(prec > Or)
    | Imp (p1, p2) ->
      dprintf "%t \\rightarrow %t" (loop Imp_r p1) (loop Imp p2)
      |> paren_if Op_prec.(prec > Imp)
  in
  Fun.flip (loop Bottom)
;;

let pp_prop_set ppf props =
  let props = Prop_set.to_seq props in
  let sep ppf () = fprintf ppf ", " in
  pp_print_seq pp_prop ~pp_sep:sep ppf props
;;

let pp_sequent ppf (psl, psr) =
  fprintf ppf "$";
  if not (Prop_set.is_empty psl) then fprintf ppf "%a " pp_prop_set psl;
  fprintf ppf "\\vdash %a$" pp_prop_set psr
;;

let rec pp_deriv_bussproof_commands ppf = function
  | Derivation.Axiom seq -> pp_axiom ppf seq
  | Not_l (seq, deriv) -> pp_unary_inf ppf "$\\lnot L$" seq deriv
  | Not_r (seq, deriv) -> pp_unary_inf ppf "$\\lnot R$" seq deriv
  | And_l (seq, deriv) -> pp_unary_inf ppf "$\\land L$" seq deriv
  | And_r (seq, deriv1, deriv2) ->
    pp_binary_inf ppf "$\\land R$" seq deriv1 deriv2
  | Or_l (seq, deriv1, deriv2) ->
    pp_binary_inf ppf "$\\lor L$" seq deriv1 deriv2
  | Or_r (seq, deriv) -> pp_unary_inf ppf "$\\lor R$" seq deriv
  | Imp_l (seq, deriv1, deriv2) ->
    pp_binary_inf ppf "$\\rightarrow L$" seq deriv1 deriv2
  | Imp_r (seq, deriv) -> pp_unary_inf ppf "$\\rightarrow R$" seq deriv

and pp_axiom ppf seq =
  fprintf ppf "\\AxiomC{}@,";
  fprintf ppf "\\RightLabel{(axiom)}@,";
  fprintf ppf "\\UnaryInfC{%a}@," pp_sequent seq

and pp_unary_inf ppf rule seq deriv =
  pp_deriv_bussproof_commands ppf deriv;
  fprintf ppf "\\RightLabel{(%s)}@," rule;
  fprintf ppf "\\UnaryInfC{%a}@," pp_sequent seq

and pp_binary_inf ppf rule seq deriv1 deriv2 =
  pp_deriv_bussproof_commands ppf deriv1;
  pp_deriv_bussproof_commands ppf deriv2;
  fprintf ppf "\\RightLabel{(%s)}@," rule;
  fprintf ppf "\\BinaryInfC{%a}@," pp_sequent seq
;;

let pp_deriv_bussproof ppf deriv =
  fprintf ppf "@[<v>\\begin{prooftree}@,";
  pp_deriv_bussproof_commands ppf deriv;
  fprintf ppf "\\end{prooftree}@]@."
;;
