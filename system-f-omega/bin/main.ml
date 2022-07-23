open System_f_omega.Typ
open System_f_omega.Infer
open Record

let () =
  Printf.printf "Type of pack %s\n"
    (typ_pp (infer pack_record TypContext.empty KindContext.empty))
