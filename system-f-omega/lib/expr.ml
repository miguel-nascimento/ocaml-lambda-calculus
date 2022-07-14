open Typ

(* TODO: change param from string to something like Name with a hash *)
type expr =
  | Var of string
  | Lamb of { param : string; param_typ : typ; body : expr }
  | App of { f : expr; arg : expr }
  | L_int of int
  | TE_lamb of { typ_param : string; kind : kind; body : expr }
  | TE_app of { expr : expr; typ : typ }

let rec expr_pp expr =
  match expr with
  | Var name -> name
  | Lamb { param; param_typ = T_var _ as param_typ; body }
  | Lamb { param; param_typ = T_int as param_typ; body } ->
      "λ" ^ param ^ ":" ^ typ_pp param_typ ^ ". " ^ expr_pp body
  | Lamb { param; param_typ; body } ->
      "λ" ^ param ^ ":(" ^ typ_pp param_typ ^ "). " ^ expr_pp body
  | App { f = App _ as f; arg } -> "(" ^ expr_pp f ^ " " ^ expr_pp arg ^ ")"
  | App { f; arg = Lamb _ as arg } ->
      "(" ^ expr_pp f ^ " (" ^ expr_pp arg ^ "))"
  | App { f; arg } -> "(" ^ expr_pp f ^ " " ^ expr_pp arg ^ ")"
  | L_int i -> string_of_int i
  | TE_lamb { typ_param; kind; body } ->
      "Λ" ^ typ_param ^ ": " ^ kind_pp kind ^ ". " ^ expr_pp body
  | TE_app { expr; typ = T_var _ as typ } | TE_app { expr; typ = T_int as typ }
    ->
      expr_pp expr ^ " @" ^ typ_pp typ
  | TE_app { expr; typ } -> expr_pp expr ^ " @(" ^ typ_pp typ ^ ")"
