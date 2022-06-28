open Sltc.Eval
open Sltc.Expr
open Sltc.Typ

let int_id = Lamb { param = "x"; param_typ = T_int; body = Var "x" }

let app_int_id = App { f = int_id; arg = Int 1 }

let () = Printf.printf "%s\n" (typ_pp (infer app_int_id TypContext.empty))
