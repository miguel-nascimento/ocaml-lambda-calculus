open Systemf.Typ
open Systemf.Expr

(* ∀a. ∀b. a -> b -> ∀r. (a -> b -> r) -> r *)
let mk_pair =
  T_lamb
    {
      typ_param = "a";
      body =
        T_lamb
          {
            typ_param = "b";
            body =
              Lamb
                {
                  param = "x";
                  param_typ = T_var "a";
                  body =
                    Lamb
                      {
                        param = "y";
                        param_typ = T_var "b";
                        body =
                          T_lamb
                            {
                              typ_param = "r";
                              body =
                                Lamb
                                  {
                                    param = "p";
                                    param_typ =
                                      T_arrow
                                        {
                                          param_typ = T_var "a";
                                          body_typ =
                                            T_arrow
                                              {
                                                param_typ = T_var "b";
                                                body_typ = T_var "r";
                                              };
                                        };
                                    body =
                                      App
                                        {
                                          f = App { f = Var "p"; arg = Var "x" };
                                          arg = Var "y";
                                        };
                                  };
                            };
                      };
                };
          };
    }

let fst =
  T_lamb
    {
      typ_param = "a";
      body =
        T_lamb
          {
            typ_param = "b";
            body =
              Lamb
                {
                  param = "p";
                  param_typ =
                    T_arrow
                      {
                        param_typ =
                          T_arrow
                            {
                              param_typ = T_var "a";
                              body_typ =
                                T_arrow
                                  {
                                    param_typ = T_var "b";
                                    body_typ = T_var "a";
                                  };
                            };
                        body_typ = T_var "a";
                      };
                  body =
                    App
                      {
                        f = Var "p";
                        arg =
                          Lamb
                            {
                              param = "x";
                              param_typ = T_var "a";
                              body =
                                Lamb
                                  {
                                    param = "y";
                                    param_typ = T_var "b";
                                    body = Var "x";
                                  };
                            };
                      };
                };
          };
    }

let snd =
  T_lamb
    {
      typ_param = "a";
      body =
        T_lamb
          {
            typ_param = "b";
            body =
              Lamb
                {
                  param = "p";
                  param_typ =
                    T_arrow
                      {
                        param_typ =
                          T_arrow
                            {
                              param_typ = T_var "a";
                              body_typ =
                                T_arrow
                                  {
                                    param_typ = T_var "b";
                                    body_typ = T_var "b";
                                  };
                            };
                        body_typ = T_var "b";
                      };
                  body =
                    App
                      {
                        f = Var "p";
                        arg =
                          Lamb
                            {
                              param = "x";
                              param_typ = T_var "a";
                              body =
                                Lamb
                                  {
                                    param = "y";
                                    param_typ = T_var "b";
                                    body = Var "y";
                                  };
                            };
                      };
                };
          };
    }

let pair_int_int =
  App
    {
      f =
        App
          {
            f =
              T_app
                { expr = T_app { expr = mk_pair; typ = T_int }; typ = T_int };
            arg = L_int 1;
          };
      arg = L_int 2;
    }

let fst_from_int_pair =
  App
    {
      f = T_app { expr = T_app { expr = fst; typ = T_int }; typ = T_int };
      arg = T_app { expr = pair_int_int; typ = T_int };
    }

let snd_from_int_pair =
  App
    {
      f = T_app { expr = T_app { expr = snd; typ = T_int }; typ = T_int };
      arg = T_app { expr = pair_int_int; typ = T_int };
    }
