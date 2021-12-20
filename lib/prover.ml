(* option monad *)
let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

and return x = Some x

let extract_not =
  Lk.PropSet.remove_map (function
      | Lk.Not p -> Some p
      | _ -> None)

let extract_and =
  Lk.PropSet.remove_map (function
      | Lk.And (p1, p2) -> Some (p1, p2)
      | _ -> None)

let extract_or =
  Lk.PropSet.remove_map (function
      | Lk.Or (p1, p2) -> Some (p1, p2)
      | _ -> None)

let extract_imp =
  Lk.PropSet.remove_map (function
      | Lk.Imp (p1, p2) -> Some (p1, p2)
      | _ -> None)

let rec prove seq = List.find_map (fun rule -> rule seq) rules
and rules = [ axiom; notl; notr; andl; orr; impr; andr; orl; impl ]

and axiom (Lk.Sequent (psl, psr) as seq) =
  if Lk.PropSet.disjoint psl psr then None else Some (Lk.Axiom seq)

and notl (Lk.Sequent (psl, psr) as seq) =
  match extract_not psl with
  | None, _ -> None
  | Some p, psl ->
      let seq' = Lk.Sequent (psl, Lk.PropSet.add p psr) in
      let* deriv = prove seq' in
      return (Lk.NotL (seq, deriv))

and notr (Lk.Sequent (psl, psr) as seq) =
  match extract_not psr with
  | None, _ -> None
  | Some p, psr ->
      let seq' = Lk.Sequent (Lk.PropSet.add p psl, psr) in
      let* deriv = prove seq' in
      return (Lk.NotR (seq, deriv))

and andl (Lk.Sequent (psl, psr) as seq) =
  match extract_and psl with
  | None, _ -> None
  | Some (p1, p2), psl ->
      let psl = Lk.PropSet.add p1 psl in
      let psl = Lk.PropSet.add p2 psl in
      let seq' = Lk.Sequent (psl, psr) in
      let* deriv = prove seq' in
      return (Lk.AndL (seq, deriv))

and andr (Lk.Sequent (psl, psr) as seq) =
  match extract_and psr with
  | None, _ -> None
  | Some (p1, p2), psr ->
      let seq1 = Lk.Sequent (psl, Lk.PropSet.add p1 psr) in
      let* deriv1 = prove seq1 in
      let seq2 = Lk.Sequent (psl, Lk.PropSet.add p2 psr) in
      let* deriv2 = prove seq2 in
      return (Lk.AndR (seq, deriv1, deriv2))

and orl (Lk.Sequent (psl, psr) as seq) =
  match extract_or psl with
  | None, _ -> None
  | Some (p1, p2), psl ->
      let seq1 = Lk.Sequent (Lk.PropSet.add p1 psl, psr) in
      let* deriv1 = prove seq1 in
      let seq2 = Lk.Sequent (Lk.PropSet.add p2 psl, psr) in
      let* deriv2 = prove seq2 in
      return (Lk.OrL (seq, deriv1, deriv2))

and orr (Lk.Sequent (psl, psr) as seq) =
  match extract_or psr with
  | None, _ -> None
  | Some (p1, p2), psr ->
      let psr = Lk.PropSet.add p1 psr in
      let psr = Lk.PropSet.add p2 psr in
      let seq' = Lk.Sequent (psl, psr) in
      let* deriv = prove seq' in
      return (Lk.OrR (seq, deriv))

and impl (Lk.Sequent (psl, psr) as seq) =
  match extract_imp psl with
  | None, _ -> None
  | Some (p1, p2), psl ->
      let seq1 = Lk.Sequent (psl, Lk.PropSet.add p1 psr) in
      let* deriv1 = prove seq1 in
      let seq2 = Lk.Sequent (Lk.PropSet.add p2 psl, psr) in
      let* deriv2 = prove seq2 in
      return (Lk.ImpL (seq, deriv1, deriv2))

and impr (Lk.Sequent (psl, psr) as seq) =
  match extract_imp psr with
  | None, _ -> None
  | Some (p1, p2), psr ->
      let psl = Lk.PropSet.add p1 psl in
      let psr = Lk.PropSet.add p2 psr in
      let seq' = Lk.Sequent (psl, psr) in
      let* deriv = prove seq' in
      return (Lk.ImpR (seq, deriv))
