open Expr
module Context = Map.Make (String)

type value =
  | V_Int of int
  | V_Closure of { ctx : value Context.t; param : string; body : expr }

let value_pp value =
  match value with V_Closure _ -> "closure\n" | V_Int x -> string_of_int x

let rec eval expr ctx =
  match expr with
  | Var name -> Context.find name ctx
  | L_int x -> V_Int x
  | T_lamb { typ_param = _; body } -> eval body ctx
  | T_app { expr; typ = _ } -> eval expr ctx
  | Lamb { param; param_typ = _; body } -> V_Closure { ctx; param; body }
  | App { f; arg } -> (
      let arg_res = eval arg ctx in
      match eval f ctx with
      | V_Closure { ctx; param; body } ->
          eval body (Context.add param arg_res ctx)
      | _ -> failwith "Non-function application")
