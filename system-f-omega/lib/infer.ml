open Expr
open Typ
module TypContext = Map.Make (String)
module KindContext = Map.Make (String)

let rec infer_kind typ ctx =
  match typ with
  | T_int -> K_star
  | T_var name -> (
      match KindContext.find_opt name ctx with
      | Some x -> x
      | None ->
          Printf.printf "\o033[31mKind for T_var '%s' not found.\o033[0m\n" name;
          Printf.printf "Kind Context dump: \n";
          KindContext.iter
            (fun x y -> Printf.printf "  type: %s \tkind: %s\n" x (kind_pp y))
            ctx;
          failwith "Couldn't find type kind")
  | T_lamb { name; kind; body } ->
      let new_ctx = KindContext.add name kind ctx in
      let body_kind = infer_kind body new_ctx in
      K_arrow { param_kind = kind; body_kind }
  | T_app { f; arg } -> (
      let f_kind, arg_kind = (infer_kind f ctx, infer_kind arg ctx) in
      match f_kind with
      | K_arrow { param_kind; body_kind } ->
          if kind_equal param_kind arg_kind then body_kind
          else (
            Printf.printf "\o033[31mKind mismatch:\o033[0m\n\n\t";
            Printf.printf "Got Type: \t%s \n\tExpected Type: \t%s\n\n"
              (kind_pp param_kind) (kind_pp f_kind);
            failwith "Wrong type-level kind type.")
      | _ -> failwith "LHS not type lambda")
  | T_forall { name; kind; body } ->
      let new_ctx = KindContext.add name kind ctx in
      infer_kind body new_ctx
  | T_arrow { param_typ; body_typ } -> (
      match infer_kind param_typ ctx with
      | K_star -> (
          match infer_kind body_typ ctx with
          | K_star -> K_star
          | _ -> failwith "Kind Mismatch")
      | _ -> failwith "Kind Mismatch")

let rec infer expr ctx kind_ctx =
  match expr with
  | Var name -> TypContext.find name ctx
  | L_int _ -> T_int
  | TE_lamb { typ_param; kind; body } ->
      let kind_new_ctx = KindContext.add typ_param kind kind_ctx in
      let typ = infer body ctx kind_new_ctx in
      T_forall { name = typ_param; kind; body = typ }
  | TE_app { expr; typ } -> (
      let expr_typ, te_kind =
        (infer expr ctx kind_ctx, infer_kind typ kind_ctx)
      in
      match expr_typ with
      | T_forall { name; kind = _; body } -> (
          match te_kind with
          | K_star -> subst body ~from:name ~_to:typ
          | _ -> failwith ("Kind of" ^ typ_pp typ ^ "is not K_star"))
      | _ -> failwith "Need to be a forall")
  | Lamb { param; param_typ; body } -> (
      let new_ctx = TypContext.add param param_typ ctx in
      let return_typ = infer body new_ctx kind_ctx in
      match infer_kind param_typ kind_ctx with
      | K_star -> T_arrow { param_typ; body_typ = return_typ }
      | _ ->
          failwith
            "Youre doing something wrong man, why this lambda has kind arrow?")
  | App { f; arg } -> (
      let f_typ, arg_typ = (infer f ctx kind_ctx, infer arg ctx kind_ctx) in
      match f_typ with
      | T_arrow { param_typ; body_typ } ->
          if equal param_typ arg_typ then body_typ
          else (
            Printf.printf "\o033[31mType Mismatch\o033[0m\n";
            Printf.printf "\tGot Type: \t%s \n\tExpected Type: \t%s\n\n"
              (typ_pp param_typ) (typ_pp f_typ);
            failwith "Wrong function type.")
      | _ ->
          Printf.printf "\n'%s' is not a lambda\n\n" (expr_pp f);
          Printf.printf "Trying to app '%s' '%s'\n" (expr_pp f) (expr_pp arg);
          Printf.printf "Type of f: \t%s\n" (typ_pp f_typ);
          Printf.printf "Type of arg: \t%s\n" (typ_pp arg_typ);
          failwith "LHS is not a lambda or you forgot to do Type Application")
