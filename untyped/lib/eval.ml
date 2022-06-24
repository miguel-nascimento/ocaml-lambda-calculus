module Context = Map.Make (String)
open Expr

type value =
  | V_Lit of lit
  | V_Closure of { ctx : value Context.t; param : string; body : expr }

let value_pp value =
  match value with
  | V_Closure _ -> Printf.printf "closure\n"
  | V_Lit x -> lit_pp x

let rec eval expr ctx =
  match expr with
  | Var name -> Context.find name ctx
  | Lit lit -> V_Lit lit
  | Lamb { param; body } -> V_Closure { ctx; param; body }
  | App { f; arg } -> (
      let arg_res = eval arg ctx in
      match eval f ctx with
      | V_Closure { ctx; param; body } ->
          eval body (Context.add param arg_res ctx)
      | _ -> failwith "Non-function application")

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
