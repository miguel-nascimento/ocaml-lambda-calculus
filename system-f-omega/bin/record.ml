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

let get_number =
  biglambda ("e", star)
    (("id", forall ("a", star) (t_var "a" --> t_var "a"))
    => (("number", t_int)
       => biglambda ("r", star)
            (( "record",
               forall ("a", star) (t_var "a" --> t_var "a")
               --> (t_int --> t_var "r") )
            => var "number")))

let id = biglambda ("a", star) (("x", t_var "a") => var "x")

let my_record =
  app
    ~f:
      (app
         ~f:
           (mk_record @ forall ("a", star) (t_var "a" --> (t_var "a" --> t_int)))
         ~arg:id)
    ~arg:(int 1)

let pack_record =
  biglambda ("Y", star)
    (( "f",
       forall ("X", star)
         (product_typ (forall ("a", star) (t_var "a") --> t_var "a") t_int)
       --> t_var "y" )
    => app
         ~f:
           (var "f"
           @ (* Q: Isn't supposed to eed the type application?
                Q: What should be the type? Product type only? T_exist? Signature? *)
           t_exist ("x", star)
             (product_typ (forall ("a", star) (t_var "a" --> t_var "a")) t_int)
           )
         ~arg:my_record)
