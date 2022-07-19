open Expr
open Typ

(* Expr *)
let ( => ) (param, param_typ) (body : expr) = Lamb { param; param_typ; body }

let biglambda (typ_param, kind) body = TE_lamb { typ_param; kind; body }

let app ~f ~arg = App { f; arg }

let int x = L_int x

let var name = Var name

let ( @ ) expr typ = TE_app { expr; typ }

(* Kind *)
let star = K_star

let ( -~> ) param_kind body_kind = K_arrow { param_kind; body_kind }

(* Types *)
let ( --> ) param_typ body_typ = T_arrow { param_typ; body_typ }

let t_var name = T_var name

let t_int = T_int

let forall (name, kind) body = T_forall { name; kind; body }

let t_app ~f ~arg = T_app { f; arg }
