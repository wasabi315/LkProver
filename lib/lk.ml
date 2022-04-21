type prop =
  | Bottom
  | Sym of string
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
[@@deriving show, ord]

module PropSet = struct
  include Set.Make (struct
    type t = prop

    let compare = compare_prop
  end)

  let pp ppf props =
    let props = to_seq props in
    let sep ppf () = Format.fprintf ppf ",@ " in
    Format.fprintf ppf "{%a}" (Format.pp_print_seq pp_prop ~pp_sep:sep) props
end

type sequent = Sequent of PropSet.t * PropSet.t [@@deriving show]

type derivation =
  | Axiom of sequent
  | NotL of sequent * derivation
  | NotR of sequent * derivation
  | AndL of sequent * derivation
  | AndR of sequent * derivation * derivation
  | OrL of sequent * derivation * derivation
  | OrR of sequent * derivation
  | ImpL of sequent * derivation * derivation
  | ImpR of sequent * derivation
[@@deriving show]
