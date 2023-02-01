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
end

let pp_prop =
  let pp_with_paren ppf prec1 prec2 pp =
    let show_paren = Op_prec.compare prec1 prec2 > 0 in
    if show_paren then fprintf ppf "(";
    fprintf ppf "%t" pp;
    if show_paren then fprintf ppf ")"
  in
  let rec loop prec ppf = function
    | Prop.Bottom -> fprintf ppf "\\bot"
    | Sym s -> fprintf ppf "%s" s
    | Not p -> fprintf ppf "\\lnot %a" (loop Op_prec.Not) p
    | And (p1, p2) ->
      dprintf "%a \\land %a" (loop And) p1 (loop And) p2
      |> pp_with_paren ppf prec And
    | Or (p1, p2) ->
      dprintf "%a \\lor %a" (loop Or) p1 (loop Or) p2
      |> pp_with_paren ppf prec Or
    | Imp (p1, p2) ->
      dprintf "%a \\rightarrow %a" (loop Imp_r) p1 (loop Imp) p2
      |> pp_with_paren ppf prec Imp
  in
  loop Bottom

let pp_prop_set ppf props =
  let props = Prop_set.to_seq props in
  let sep ppf () = fprintf ppf ", " in
  pp_print_seq pp_prop ~pp_sep:sep ppf props

let pp_sequent ppf (psl, psr) =
  fprintf ppf "$";
  if not (Prop_set.is_empty psl) then fprintf ppf "%a " pp_prop_set psl;
  fprintf ppf "\\vdash %a$" pp_prop_set psr

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

let pp_deriv_bussproof ppf deriv =
  fprintf ppf "@[<v>\\begin{prooftree}@,";
  pp_deriv_bussproof_commands ppf deriv;
  fprintf ppf "\\end{prooftree}@]@."
