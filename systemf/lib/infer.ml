open Expr
open Typ
module TypContext = Map.Make (String)

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
          else (
            Printf.printf
              "Type mismatch:\n\tGot Type: \t%s \n\tExpected Type: \t%s\n\n"
              (typ_pp param_typ) (typ_pp f_typ);
            failwith "Wrong function type.")
      | _ -> failwith "LHS is not a lambda or you forgot to do Type Application"
      )
