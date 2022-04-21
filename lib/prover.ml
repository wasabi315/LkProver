(* option monad *)
let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

and return x = Some x

(*
  pop_matched f ({p} ∪ P) = Some x, P  (if f p = Some x)
  pop_matched f P = None, P            (otherwise)
*)
let pop_matched (f : Lk.prop -> 'a option) (props : Lk.PropSet.t) =
  let value_ref = ref None in
  let g prop =
    match !value_ref, f prop with
    | Some _, _ | None, None -> Some prop
    | None, (Some _ as value) ->
        value_ref := value;
        None
  in
  let props = Lk.PropSet.filter_map g props in
  !value_ref, props

let pop_bottom =
  pop_matched (function
      | Lk.Bottom -> Some ()
      | _ -> None)

let pop_not =
  pop_matched (function
      | Lk.Not p -> Some p
      | _ -> None)

let pop_and =
  pop_matched (function
      | Lk.And (p1, p2) -> Some (p1, p2)
      | _ -> None)

let pop_or =
  pop_matched (function
      | Lk.Or (p1, p2) -> Some (p1, p2)
      | _ -> None)

let pop_imp =
  pop_matched (function
      | Lk.Imp (p1, p2) -> Some (p1, p2)
      | _ -> None)

let rec prove seq = List.find_map (fun rule -> rule seq) rules
and rules = [ axiom1; axiom2; notl; notr; andl; orr; impr; andr; orl; impl ]

and axiom1 (Lk.Sequent (psl, psr) as seq) =
  if Lk.PropSet.disjoint psl psr then None else Some (Lk.Axiom seq)

and axiom2 (Lk.Sequent (psl, _) as seq) =
  match pop_bottom psl with
  | None, _ -> None
  | Some (), _ -> Some (Lk.Axiom seq)

and notl (Lk.Sequent (psl, psr) as seq) =
  match pop_not psl with
  | None, _ -> None
  | Some p, psl ->
      let seq' = Lk.Sequent (psl, Lk.PropSet.add p psr) in
      let* deriv = prove seq' in
      return (Lk.NotL (seq, deriv))

and notr (Lk.Sequent (psl, psr) as seq) =
  match pop_not psr with
  | None, _ -> None
  | Some p, psr ->
      let seq' = Lk.Sequent (Lk.PropSet.add p psl, psr) in
      let* deriv = prove seq' in
      return (Lk.NotR (seq, deriv))

and andl (Lk.Sequent (psl, psr) as seq) =
  match pop_and psl with
  | None, _ -> None
  | Some (p1, p2), psl ->
      let psl = Lk.PropSet.add p1 psl in
      let psl = Lk.PropSet.add p2 psl in
      let seq' = Lk.Sequent (psl, psr) in
      let* deriv = prove seq' in
      return (Lk.AndL (seq, deriv))

and andr (Lk.Sequent (psl, psr) as seq) =
  match pop_and psr with
  | None, _ -> None
  | Some (p1, p2), psr ->
      let seq1 = Lk.Sequent (psl, Lk.PropSet.add p1 psr) in
      let* deriv1 = prove seq1 in
      let seq2 = Lk.Sequent (psl, Lk.PropSet.add p2 psr) in
      let* deriv2 = prove seq2 in
      return (Lk.AndR (seq, deriv1, deriv2))

and orl (Lk.Sequent (psl, psr) as seq) =
  match pop_or psl with
  | None, _ -> None
  | Some (p1, p2), psl ->
      let seq1 = Lk.Sequent (Lk.PropSet.add p1 psl, psr) in
      let* deriv1 = prove seq1 in
      let seq2 = Lk.Sequent (Lk.PropSet.add p2 psl, psr) in
      let* deriv2 = prove seq2 in
      return (Lk.OrL (seq, deriv1, deriv2))

and orr (Lk.Sequent (psl, psr) as seq) =
  match pop_or psr with
  | None, _ -> None
  | Some (p1, p2), psr ->
      let psr = Lk.PropSet.add p1 psr in
      let psr = Lk.PropSet.add p2 psr in
      let seq' = Lk.Sequent (psl, psr) in
      let* deriv = prove seq' in
      return (Lk.OrR (seq, deriv))

and impl (Lk.Sequent (psl, psr) as seq) =
  match pop_imp psl with
  | None, _ -> None
  | Some (p1, p2), psl ->
      let seq1 = Lk.Sequent (psl, Lk.PropSet.add p1 psr) in
      let* deriv1 = prove seq1 in
      let seq2 = Lk.Sequent (Lk.PropSet.add p2 psl, psr) in
      let* deriv2 = prove seq2 in
      return (Lk.ImpL (seq, deriv1, deriv2))

and impr (Lk.Sequent (psl, psr) as seq) =
  match pop_imp psr with
  | None, _ -> None
  | Some (p1, p2), psr ->
      let psl = Lk.PropSet.add p1 psl in
      let psr = Lk.PropSet.add p2 psr in
      let seq' = Lk.Sequent (psl, psr) in
      let* deriv = prove seq' in
      return (Lk.ImpR (seq, deriv))
