open Typ

(* TODO: change param from string to something like Name with a hash *)
type expr =
  | Var of string
  | Lamb of { param : string; param_typ : typ; body : expr }
  | App of { f : expr; arg : expr }
  | L_int of int
  | T_lamb of { typ_param : string; body : expr }
  | T_app of { expr : expr; typ : typ }
