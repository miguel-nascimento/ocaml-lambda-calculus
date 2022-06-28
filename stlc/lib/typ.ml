type typ = T_arrow of { param_typ : typ; body_typ : typ } | T_int

let rec typ_pp typ =
  match typ with
  | T_int -> "Int"
  | T_arrow { param_typ; body_typ } ->
      typ_pp param_typ ^ " -> " ^ typ_pp body_typ
