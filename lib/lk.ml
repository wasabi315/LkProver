module Prop = struct
  type t =
    | Bottom
    | Sym of string
    | Not of t
    | And of t * t
    | Or of t * t
    | Imp of t * t
  [@@deriving show, ord]
end

module Prop_set = struct
  include Set.Make (Prop)

  let pp ppf props =
    let props = to_seq props in
    let sep ppf () = Format.fprintf ppf ",@ " in
    Format.fprintf ppf "{%a}" (Format.pp_print_seq Prop.pp ~pp_sep:sep) props
  ;;

  let show props = Format.asprintf "%a" pp props
end

module Sequent = struct
  type t = Prop_set.t * Prop_set.t [@@deriving show]
end

module Derivation = struct
  type t =
    | Axiom of Sequent.t
    | Not_l of Sequent.t * t
    | Not_r of Sequent.t * t
    | And_l of Sequent.t * t
    | And_r of Sequent.t * t * t
    | Or_l of Sequent.t * t * t
    | Or_r of Sequent.t * t
    | Imp_l of Sequent.t * t * t
    | Imp_r of Sequent.t * t
  [@@deriving show]
end
