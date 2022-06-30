open Systemf.Expr
open Systemf.Typ
open Systemf.Eval

let id =
  T_lamb
    {
      typ_param = "a";
      body = Lamb { param = "x"; param_typ = T_var "a"; body = Var "x" };
    }

let int_id_f = T_app { expr = id; typ = T_int }

let one = App { f = int_id_f; arg = L_int 1 }

(* Typechecking *)
(* let () = Printf.printf "%s\n" (typ_pp (infer one TypContext.empty)) *)

(* Eval *)
let () =
  let _ = infer one TypContext.empty in
  Printf.printf "%s\n" (value_pp (eval one Context.empty))
