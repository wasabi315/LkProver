open Lk

(* option monad *)
let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

and return x = Some x

(*
  pop_matched f ({p} ∪ P) = Some (x, P)  (if f p = Some x)
  pop_matched f P = None                 (otherwise)
*)
let pop_matched (f : Prop.t -> 'a option) (props : Prop_set.t) =
  let value_ref = ref None in
  let g prop =
    match !value_ref, f prop with
    | Some _, _ | None, None -> Some prop
    | None, (Some _ as value) ->
      value_ref := value;
      None
  in
  let props = Prop_set.filter_map g props in
  Option.map (fun value -> value, props) !value_ref

let pop_bottom =
  pop_matched (function
    | Prop.Bottom -> Some ()
    | _ -> None)

let pop_not =
  pop_matched (function
    | Prop.Not p -> Some p
    | _ -> None)

let pop_and =
  pop_matched (function
    | Prop.And (p1, p2) -> Some (p1, p2)
    | _ -> None)

let pop_or =
  pop_matched (function
    | Prop.Or (p1, p2) -> Some (p1, p2)
    | _ -> None)

let pop_imp =
  pop_matched (function
    | Prop.Imp (p1, p2) -> Some (p1, p2)
    | _ -> None)

let rec prove seq = List.find_map (fun rule -> rule seq) rules
and rules = [ axiom1; axiom2; notl; notr; andl; orr; impr; andr; orl; impl ]

and axiom1 ((psl, psr) as seq) =
  if Prop_set.disjoint psl psr then None else Some (Derivation.Axiom seq)

and axiom2 ((psl, _) as seq) =
  let* _ = pop_bottom psl in
  return (Derivation.Axiom seq)

and notl ((psl, psr) as seq) =
  let* p, psl = pop_not psl in
  let seq' = psl, Prop_set.add p psr in
  let* deriv = prove seq' in
  return (Derivation.Not_l (seq, deriv))

and notr ((psl, psr) as seq) =
  let* p, psr = pop_not psr in
  let seq' = Prop_set.add p psl, psr in
  let* deriv = prove seq' in
  return (Derivation.Not_r (seq, deriv))

and andl ((psl, psr) as seq) =
  let* (p1, p2), psl = pop_and psl in
  let psl = Prop_set.add p1 psl in
  let psl = Prop_set.add p2 psl in
  let seq' = psl, psr in
  let* deriv = prove seq' in
  return (Derivation.And_l (seq, deriv))

and andr ((psl, psr) as seq) =
  let* (p1, p2), psr = pop_and psr in
  let seq1 = psl, Prop_set.add p1 psr in
  let* deriv1 = prove seq1 in
  let seq2 = psl, Prop_set.add p2 psr in
  let* deriv2 = prove seq2 in
  return (Derivation.And_r (seq, deriv1, deriv2))

and orl ((psl, psr) as seq) =
  let* (p1, p2), psl = pop_or psl in
  let seq1 = Prop_set.add p1 psl, psr in
  let* deriv1 = prove seq1 in
  let seq2 = Prop_set.add p2 psl, psr in
  let* deriv2 = prove seq2 in
  return (Derivation.Or_l (seq, deriv1, deriv2))

and orr ((psl, psr) as seq) =
  let* (p1, p2), psr = pop_or psr in
  let psr = Prop_set.add p1 psr in
  let psr = Prop_set.add p2 psr in
  let seq' = psl, psr in
  let* deriv = prove seq' in
  return (Derivation.Or_r (seq, deriv))

and impl ((psl, psr) as seq) =
  let* (p1, p2), psl = pop_imp psl in
  let seq1 = psl, Prop_set.add p1 psr in
  let* deriv1 = prove seq1 in
  let seq2 = Prop_set.add p2 psl, psr in
  let* deriv2 = prove seq2 in
  return (Derivation.Imp_l (seq, deriv1, deriv2))

and impr ((psl, psr) as seq) =
  let* (p1, p2), psr = pop_imp psr in
  let psl = Prop_set.add p1 psl in
  let psr = Prop_set.add p2 psr in
  let seq' = psl, psr in
  let* deriv = prove seq' in
  return (Derivation.Imp_r (seq, deriv))
