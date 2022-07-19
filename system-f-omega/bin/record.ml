open System_f_omega.Utils

(* 
  data Record = MkRecord { id :: forall a. a -> a, number :: int }   
  x = MkRecord { id = id, number = 5 }
*)

(* id => number => mk_record => mk_record(id)(number) *)

let typ_record =
  forall ("r", star)
    (forall ("a", star) (t_var "a" --> t_var "a") --> t_int --> t_var "r")

let mk_record =
  biglambda ("e", star)
    (("id", forall ("a", star) (t_var "a" --> t_var "a"))
    => (("number", t_int)
       => biglambda ("r", star)
            (( "record",
               forall ("a", star) (t_var "a" --> t_var "a")
               --> (t_int --> t_var "r") )
            => app
                 ~f:(app ~f:(var "record") ~arg:(var "id"))
                 ~arg:(var "number"))))

let number =
  biglambda ("e", star)
    (("id", forall ("a", star) (t_var "a" --> t_var "a"))
    => (("number", t_int)
       => biglambda ("r", star)
            (( "record",
               forall ("a", star) (t_var "a" --> t_var "a")
               --> (t_int --> t_var "r") )
            => var "number")))
