let () = print_endline "Test later :p"

(* let id =
  T_lamb
    {
      typ_param = "a";
      body = Lamb { param = "x"; param_typ = T_var "a"; body = Var "x" };
    }

let one = App { f = T_app { expr = id; typ = T_int }; arg = L_int 1 }

(* Polymorphic example *)
let () = Printf.printf "=================================\nId:\n"

(* Typechecking *)
let () = Printf.printf "\nTyp id: \t%s\n" (typ_pp (infer id TypContext.empty))

let () =
  Printf.printf "Typ id @Int: \t%s\n"
    (typ_pp (infer (T_app { expr = id; typ = T_int }) TypContext.empty))

let () =
  let _ = infer one TypContext.empty in
  Printf.printf "Value one: \t%s\n" (value_pp (eval one Context.empty))

(* Pairs *)
let () = Printf.printf "\n=================================\nPairs:\n"

(* Typechecking *)
let () =
  Printf.printf "\nTyp pair_int_int: \t%s\n"
    (typ_pp (infer pair_int_int TypContext.empty))

let () = Printf.printf "Typ fst: \t%s\n" (typ_pp (infer fst TypContext.empty))

let () = Printf.printf "Typ snd: \t%s\n" (typ_pp (infer snd TypContext.empty))

(* Eval *)
let () =
  let _ = infer fst_from_int_pair TypContext.empty in
  Printf.printf "\nValue fst_from_int_pair: \t%s\n"
    (value_pp (eval fst_from_int_pair Context.empty))

let () =
  let _ = infer snd_from_int_pair TypContext.empty in
  Printf.printf "Value snd_from_int_pair: \t%s\n"
    (value_pp (eval snd_from_int_pair Context.empty))

let () = Printf.printf "\n=================================\n" *)
