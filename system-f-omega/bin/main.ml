open System_f_omega.Typ
open System_f_omega.Expr
open System_f_omega.Infer
open Record

let () = Printf.printf "Expr mk_record %s\n" (expr_pp mk_record)

let () = Printf.printf "Type of mk_record %s\n" (typ_pp typ_record)

let () =
  Printf.printf "Type of mk_record %s\n"
    (typ_pp (infer mk_record TypContext.empty KindContext.empty))
