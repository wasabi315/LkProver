open Format

type prec =
  | PBottom
  | PImp
  | PImpR (* → is right associative *)
  | POr
  | PAnd
  | PNot
[@@deriving ord]

let pp_prop =
  let rec loop prec ppf = function
    | Lk.Sym s -> fprintf ppf "%s" s
    | Lk.Not p -> fprintf ppf "\\lnot %a" (loop PNot) p
    | Lk.And (p1, p2) ->
        let show_paren = compare_prec prec PAnd > 0 in
        if show_paren then fprintf ppf "(";
        fprintf ppf "%a \\land %a" (loop PAnd) p1 (loop PAnd) p2;
        if show_paren then fprintf ppf ")"
    | Lk.Or (p1, p2) ->
        let show_paren = compare_prec prec POr > 0 in
        if show_paren then fprintf ppf "(";
        fprintf ppf "%a \\lor %a" (loop POr) p1 (loop POr) p2;
        if show_paren then fprintf ppf ")"
    | Lk.Imp (p1, p2) ->
        let show_paren = compare_prec prec PImp > 0 in
        if show_paren then fprintf ppf "(";
        fprintf ppf "%a \\rightarrow %a" (loop PImpR) p1 (loop PImp) p2;
        if show_paren then fprintf ppf ")"
  in
  loop PBottom

let pp_propset ppf props =
  let props = Lk.PropSet.to_seq props in
  let sep ppf () = fprintf ppf ", " in
  pp_print_seq pp_prop ~pp_sep:sep ppf props

let pp_sequent ppf (Lk.Sequent (psl, psr)) =
  fprintf ppf "$";
  if not (Lk.PropSet.is_empty psl) then fprintf ppf "%a " pp_propset psl;
  fprintf ppf "\\vdash %a$" pp_propset psr

let rec pp_deriv_bussproof_commands ppf = function
  | Lk.Axiom seq -> pp_axiom ppf seq
  | Lk.NotL (seq, deriv) -> pp_unary_inf ppf "$\\lnot L$" seq deriv
  | Lk.NotR (seq, deriv) -> pp_unary_inf ppf "$\\lnot R$" seq deriv
  | Lk.AndL (seq, deriv) -> pp_unary_inf ppf "$\\land L$" seq deriv
  | Lk.AndR (seq, deriv1, deriv2) ->
      pp_binary_inf ppf "$\\land R$" seq deriv1 deriv2
  | Lk.OrL (seq, deriv1, deriv2) ->
      pp_binary_inf ppf "$\\lor L$" seq deriv1 deriv2
  | Lk.OrR (seq, deriv) -> pp_unary_inf ppf "$\\lor R$" seq deriv
  | Lk.ImpL (seq, deriv1, deriv2) ->
      pp_binary_inf ppf "$\\rightarrow L$" seq deriv1 deriv2
  | Lk.ImpR (seq, deriv) -> pp_unary_inf ppf "$\\rightarrow R$" seq deriv

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
  Format.fprintf ppf "@[<v>\\begin{prooftree}@,";
  pp_deriv_bussproof_commands ppf deriv;
  Format.fprintf ppf "\\end{prooftree}@]@."
