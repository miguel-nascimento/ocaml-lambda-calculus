open Untyped.Eval
open Untyped.Expr

(* one = ifelse(lamb_true)(1)(2) *)
let lamb_true =
  Lamb { param = "t"; body = Lamb { param = "f"; body = Var "t" } }

let lamb_false =
  Lamb { param = "t"; body = Lamb { param = "f"; body = Var "f" } }

let ifelse =
  Lamb
    {
      param = "c";
      body =
        Lamb
          {
            param = "if_t";
            body =
              Lamb
                {
                  param = "if_f";
                  body =
                    App
                      {
                        f = App { f = Var "c"; arg = Var "if_t" };
                        arg = Var "if_f";
                      };
                };
          };
    }

let one =
  App
    {
      f = App { f = App { f = ifelse; arg = lamb_true }; arg = Lit (L_int 1) };
      arg = Lit (L_int 2);
    }

let () = value_pp (eval one Context.empty)
