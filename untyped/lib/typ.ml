type typ =
  | T_lit of typ_lit
  | T_arrow of { param_typ : typ; body_typ : typ }
  | T_var of string
  | T_forall of { typ_var : string; body_typ : typ }

and typ_lit = T_str | T_int | T_bool

let typlit_pp lit =
  match lit with T_str -> "String" | T_int -> "Int" | T_bool -> "Bool"

let rec typ_pp typ =
  match typ with
  | T_lit lit -> typlit_pp lit
  | T_var str -> str
  | T_arrow { param_typ; body_typ } ->
      typ_pp param_typ ^ " -> " ^ typ_pp body_typ
  | T_forall { typ_var; body_typ } ->
      "∀" ^ typ_var ^ "(" ^ typ_pp body_typ ^ ")"

let example_arr = T_arrow { param_typ = T_lit T_str; body_typ = T_lit T_bool }
