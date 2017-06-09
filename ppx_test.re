/*open Asttypes;*/
/*open Parsetree;*/
/*open Longident;*/

let getenv s =>
  try (Sys.getenv s) {
  | Not_found => ""
};

let make_final_test_block tests => {
  let loc = Location.none;
  open Parsetree;
  [%stri let _ = {
    let (total, errs) = List.fold_left
    (fun (total, errs) (name, fn) => {
      switch (fn ()) {
      | Some err => {
        print_endline err;
        (total + 1, errs + 1)
      }
      | None => (total + 1, errs)
      }
    })
    (0, 0)
    !tests;
    print_endline ("Done! " ^ (string_of_int errs) ^ " failed out of " ^ (string_of_int total))
  }]
  /*
  open Parsetree;
  open Ast_helper;
  Str.eval (
    Exp.apply
    (Exp.ident (Location.mknoloc (Longident.Lident "parse_tree")))
    [("", Exp.constant (Asttypes.Const_string "hello" None))]
  )
  */
};

let getenv_mapper argv =>
  /* Our getenv_mapper only overrides the handling of expressions in the default mapper. */
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: fun mapper items => {
      let loc = Location.none;
      let (backwards, tests) = List.fold_left (fun (results, tests) item => {
        switch (item.Parsetree.pstr_desc) {
        | Pstr_value _ bindings => {
          ([
            [%stri let _ = tests := [("hello", fun () => "Failed"), ...!tests]],
            Ast_mapper.default_mapper.structure_item mapper item,
            ...results
          ], [1, ...tests])
        }
        | _ => ([Ast_mapper.default_mapper.structure_item mapper item, ...results], tests)
        }
      }) ([], []) items;
      switch (tests) {
      | [] => List.rev backwards
      | _ => [[%stri let tests = ref []], ...List.rev [make_final_test_block tests, ...backwards]]
      };
    },
    /*
    expr: fun mapper expr =>
      switch expr {
      /* Is this an extension node? */
      | {
          pexp_desc:
            /* Should have name "getenv". */
            Pexp_extension ({txt: "getenv", loc}, pstr)
        } =>
        switch pstr {
        /* Should have a single structure item, which is evaluation of a constant string. */
        | PStr [
            {
              pstr_desc:
                Pstr_eval 
                  {
                    pexp_loc: loc,
                    pexp_desc: Pexp_constant (Const_string sym None)
                  }
                  _
            }
          ] =>
          /* Replace with a constant string with the value from the environment. */
          Ast_helper.Exp.constant ::loc (Const_string (getenv sym) None)
        | _ =>
          raise (
            Location.Error (
              Location.error ::loc "[%getenv] accepts a string, e.g. [%getenv \"USER\"]"
            )
          )
        } /* Delegate to the default mapper. */
      | x => Ast_mapper.default_mapper.expr mapper x
    }
    */
  };

let () = Ast_mapper.register "getenv" getenv_mapper;
