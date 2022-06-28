open Typ

(* TODO: change param from string to something like Name with a hash *)
type expr =
  | Var of string
  | Lamb of { param : string; param_typ : typ; body : expr }
  | App of { f : expr; arg : expr }
  | Int of int
