open Untyped.Eval
open Untyped.Expr

(* one = ifelse(true)(1)(2) *)
let one =
  App
    {
      f = App { f = App { f = ifelse; arg = lamb_true }; arg = Lit (L_int 1) };
      arg = Lit (L_int 2);
    }

let () = value_pp (eval one Context.empty)
