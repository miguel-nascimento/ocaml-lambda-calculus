open Expr
open Typ
module Context = Map.Make (String)
module TypContext = Map.Make (String)

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

let rec infer expr ctx =
  match expr with
  | Var name -> TypContext.find name ctx
  | L_int _ -> T_int
  | T_lamb { typ_param; body } ->
      let typ = infer body ctx in
      T_forall { name = typ_param; body = typ }
  | T_app { expr; typ } -> (
      let expr_typ = infer expr ctx in
      match expr_typ with
      | T_forall { name; body } -> subst body ~from:name ~_to:typ
      | _ -> failwith "Need to be a forall")
  | Lamb { param; param_typ; body } ->
      let new_ctx = TypContext.add param param_typ ctx in
      let return_typ = infer body new_ctx in
      T_arrow { param_typ; body_typ = return_typ }
  | App { f; arg } -> (
      let f_typ, arg_typ = (infer f ctx, infer arg ctx) in
      match f_typ with
      | T_arrow { param_typ; body_typ } ->
          if equal param_typ arg_typ then body_typ
          else failwith "Wrong function type."
      | _ -> failwith "LHS is not a lambda or you forgot to do Type Application"
      )
