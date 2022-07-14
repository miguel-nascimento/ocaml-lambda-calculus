(* TODO: use hashable name instead of strings *)
type typ =
  | T_var of string
  | T_app of { f : typ; arg : typ }
  | T_lamb of { name : string; kind : kind; body : typ }
  | T_forall of { name : string; kind : kind; body : typ }
  | T_arrow of { param_typ : typ; body_typ : typ }
  | T_int
(* | T_exist of { name : string; body : typ } *)

and kind = K_star | K_arrow of { param_kind : kind; body_kind : kind }

let rec kind_pp kind =
  match kind with
  | K_star -> "*"
  | K_arrow { param_kind; body_kind = K_arrow _ as body_kind } ->
      "(" ^ kind_pp param_kind ^ " -> " ^ kind_pp body_kind ^ ")"
  | K_arrow { param_kind; body_kind } ->
      kind_pp param_kind ^ " -> " ^ kind_pp body_kind

let rec kind_equal x y =
  match (x, y) with
  | K_star, K_star -> true
  | ( K_arrow { param_kind = x_param_kind; body_kind = x_body_kind },
      K_arrow { param_kind = y_param_kind; body_kind = y_body_kind } ) ->
      kind_equal x_param_kind y_param_kind && kind_equal x_body_kind y_body_kind
  | _ -> false

let rec typ_pp typ =
  match typ with
  | T_var x -> x
  | T_int -> "Int"
  | T_arrow { param_typ = T_arrow _ as param_typ; body_typ } ->
      "(" ^ typ_pp param_typ ^ ") -> " ^ typ_pp body_typ
  | T_arrow { param_typ; body_typ } ->
      typ_pp param_typ ^ " -> " ^ typ_pp body_typ
  | T_forall { name; kind; body } ->
      "∀" ^ name ^ ":" ^ kind_pp kind ^ ". " ^ typ_pp body
  | T_app { f = T_app _ as f; arg } -> "(" ^ typ_pp f ^ " " ^ typ_pp arg ^ ")"
  | T_app { f; arg = T_lamb _ as arg } ->
      "(" ^ typ_pp f ^ " (" ^ typ_pp arg ^ "))"
  | T_app { f; arg } -> "(" ^ typ_pp f ^ " " ^ typ_pp arg ^ ")"
  | T_lamb { name; kind; body } ->
      "λ" ^ name ^ ":" ^ kind_pp kind ^ ". " ^ typ_pp body

(* TODO: this is tail recursive? I really dunno *)
let rec subst typ ~from ~_to =
  match typ with
  | T_int -> T_int
  | T_var name -> if name = from then _to else T_var name
  | T_arrow { param_typ; body_typ } ->
      T_arrow
        {
          param_typ = subst param_typ ~from ~_to;
          body_typ = subst body_typ ~from ~_to;
        }
  | T_forall { name; kind; body } ->
      if name = from then T_forall { name; kind; body }
      else T_forall { name; kind; body = subst body ~from ~_to }
  | _ -> typ

(* Unifier *)
let rec equal x y =
  match (x, y) with
  | T_int, T_int -> true
  | T_var x_name, T_var y_name -> x_name = y_name
  | ( T_arrow { param_typ = x_param_typ; body_typ = x_body_typ },
      T_arrow { param_typ = y_param_typ; body_typ = y_body_typ } ) ->
      equal x_param_typ y_param_typ && equal x_body_typ y_body_typ
  | ( T_forall { name = x_name; kind = x_kind; body = x_body },
      T_forall { name = y_name; kind = y_kind; body = y_body } ) ->
      let y_body_with_x_name = subst y_body ~from:y_name ~_to:(T_var x_name) in
      equal x_body y_body_with_x_name && kind_equal x_kind y_kind
  (* TODO: T_lamb | T_app *)
  | _ -> false
