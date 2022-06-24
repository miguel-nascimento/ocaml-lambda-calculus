(* TODO: change param from string to something like Name with a hash *)
type expr =
  | Var of string
  | Lamb of { param : string; body : expr }
  | App of { f : expr; arg : expr }
  | Lit of lit

and lit = L_int of int | L_str of string | L_bool of bool

let lit_pp lit =
  match lit with
  | L_int i -> Printf.printf "%i\n" i
  | L_bool b -> Printf.printf "%b\n" b
  | L_str str -> Printf.printf "%s\n" str
