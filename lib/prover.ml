open Lk

(* option monad *)
let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

and return x = Some x

(* pop_matched f ({p} ∪ P) = Some (x, P)  (if f p = Some x)
   pop_matched f P = None                 (otherwise) *)
let pop_matched matcher props =
  let matched = ref None in
  let choose_first_match prop =
    match !matched, matcher prop with
    | Some _, _ -> Some prop
    | None, (Some _ as value) ->
      matched := value;
      None
    | None, None -> Some prop
  in
  let props = Prop_set.filter_map choose_first_match props in
  Option.map (fun value -> value, props) !matched
;;

let pop_bottom =
  pop_matched (function
    | Bottom -> Some ()
    | _ -> None)
;;

let pop_not =
  pop_matched (function
    | Not p -> Some p
    | _ -> None)
;;

let pop_and =
  pop_matched (function
    | And (p1, p2) -> Some (p1, p2)
    | _ -> None)
;;

let pop_or =
  pop_matched (function
    | Or (p1, p2) -> Some (p1, p2)
    | _ -> None)
;;

let pop_imp =
  pop_matched (function
    | Imp (p1, p2) -> Some (p1, p2)
    | _ -> None)
;;

let rec prove seq = List.find_map (( |> ) seq) rules

and rules =
  [ axiom1; axiom2; not_l; not_r; and_l; or_r; imp_r; and_r; or_l; imp_l ]

and axiom1 ((lhs, rhs) as seq) =
  if Prop_set.disjoint lhs rhs then None else Some (Derivation.Axiom seq)

and axiom2 ((lhs, _) as seq) =
  let* _ = pop_bottom lhs in
  return (Derivation.Axiom seq)

and not_l ((lhs, rhs) as seq) =
  let* p, lhs = pop_not lhs in
  let seq' = lhs, Prop_set.add p rhs in
  let* deriv = prove seq' in
  return (Derivation.Not_l (seq, deriv))

and not_r ((lhs, rhs) as seq) =
  let* p, rhs = pop_not rhs in
  let seq' = Prop_set.add p lhs, rhs in
  let* deriv = prove seq' in
  return (Derivation.Not_r (seq, deriv))

and and_l ((lhs, rhs) as seq) =
  let* (p1, p2), lhs = pop_and lhs in
  let seq' = Prop_set.(lhs |> add p1 |> add p2), rhs in
  let* deriv = prove seq' in
  return (Derivation.And_l (seq, deriv))

and and_r ((lhs, rhs) as seq) =
  let* (p1, p2), rhs = pop_and rhs in
  let seq1 = lhs, Prop_set.add p1 rhs in
  let* deriv1 = prove seq1 in
  let seq2 = lhs, Prop_set.add p2 rhs in
  let* deriv2 = prove seq2 in
  return (Derivation.And_r (seq, deriv1, deriv2))

and or_l ((lhs, rhs) as seq) =
  let* (p1, p2), lhs = pop_or lhs in
  let seq1 = Prop_set.add p1 lhs, rhs in
  let* deriv1 = prove seq1 in
  let seq2 = Prop_set.add p2 lhs, rhs in
  let* deriv2 = prove seq2 in
  return (Derivation.Or_l (seq, deriv1, deriv2))

and or_r ((lhs, rhs) as seq) =
  let* (p1, p2), rhs = pop_or rhs in
  let seq' = lhs, Prop_set.(rhs |> add p1 |> add p2) in
  let* deriv = prove seq' in
  return (Derivation.Or_r (seq, deriv))

and imp_l ((lhs, rhs) as seq) =
  let* (p1, p2), lhs = pop_imp lhs in
  let seq1 = lhs, Prop_set.add p1 rhs in
  let* deriv1 = prove seq1 in
  let seq2 = Prop_set.add p2 lhs, rhs in
  let* deriv2 = prove seq2 in
  return (Derivation.Imp_l (seq, deriv1, deriv2))

and imp_r ((lhs, rhs) as seq) =
  let* (p1, p2), rhs = pop_imp rhs in
  let seq' = Prop_set.(add p1 lhs, add p2 rhs) in
  let* deriv = prove seq' in
  return (Derivation.Imp_r (seq, deriv))
;;
