(* TODO: use hashable name instead of strings *)
type typ =
  | T_var of string
  | T_forall of { name : string; body : typ }
  | T_arrow of { param_typ : typ; body_typ : typ }
  | T_int

let rec typ_pp typ =
  match typ with
  | T_var x -> x
  | T_int -> "Int"
  | T_arrow { param_typ; body_typ } ->
      typ_pp param_typ ^ " -> " ^ typ_pp body_typ
  | T_forall { name; body } -> "∀" ^ name ^ ". " ^ typ_pp body

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
  | T_forall { name; body } ->
      if name = from then T_forall { name; body }
      else T_forall { name; body = subst body ~from ~_to }

let rec equal x y =
  match (x, y) with
  | T_int, T_int -> true
  | T_var x_name, T_var y_name -> x_name = y_name
  | ( T_arrow { param_typ = x_param_typ; body_typ = x_body_typ },
      T_arrow { param_typ = y_param_typ; body_typ = y_body_typ } ) ->
      equal x_param_typ y_param_typ && equal x_body_typ y_body_typ
  | ( T_forall { name = x_name; body = x_body },
      T_forall { name = y_name; body = y_body } ) ->
      let y_body_with_x_name = subst y_body ~from:y_name ~_to:(T_var x_name) in
      equal x_body y_body_with_x_name
  | _ -> false
