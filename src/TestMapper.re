
open Migrate_parsetree.Ast_403;
/**
 * I'm wondering if some of these checks ought to be scoped to a specific set of fixtures.
 * That is, should I be able to set up multiple tests for a single function?
 */

let rec count_args {Parsetree.pexp_desc} => Parsetree.(switch pexp_desc {
| Pexp_fun Nolabel _ _ expr => switch (count_args expr) {
  | Some n => Some (1 + n)
  | None => None
}
| Pexp_fun _ => None
| _ => Some 0
});

let getInfo {Parsetree.pvb_pat, pvb_expr} => {
  open Parsetree;
  switch (pvb_pat.ppat_desc) {
  | Ppat_var {txt} => switch (pvb_expr.pexp_desc) {
    | Pexp_fun Nolabel _ _ expr => switch (count_args expr) {
        | Some n => Some (txt, Some (1 + n))
        | None => Some (txt, None)
      }
    | Pexp_fun _ => Some (txt, None)
    | _ => Some (txt, None)
    }
  | _ => None
  }
};

let tests_for_binding mapper binding => {
  open Parsetree;
  let test = Attributes.process binding.pvb_attributes;
  switch (Test.validate test) {
  | None => None
  | Some test => {
    switch (getInfo binding) {
    | None => Utils.fail "test attributes on a non-function"
    | Some (name, args) => {
      let test_name = Utils.optor test.name name;
      switch (Generate.test name test_name args test) {
      | None => None
      | Some str => Some str
      }
    }
    }
  }
  }
};

let tests_for_bindings mapper bindings => {
  List.fold_left
  (fun tests binding => switch (tests_for_binding mapper binding) {
  | None => tests
  | Some test => [test, ...tests]
  })
  []
  bindings
};

let mapper = Parsetree.{
  ...Ast_mapper.default_mapper,
  structure: fun mapper items => {
    let loc = Location.none;
    let (backwards, tests) = List.fold_left (fun (results, tests) item => {
      let mapped = Ast_mapper.default_mapper.structure_item mapper item;
      /** TODO strip off attributes */
      switch (item.Parsetree.pstr_desc) {
      | Pstr_value _ bindings => {
        let test_strs = tests_for_bindings mapper bindings;
        (test_strs @ [mapped, ...results], tests + 1)
      }
      | _ => ([mapped, ...results], tests)
      }
    }) ([], 0) items;
    switch (tests) {
    | 0 => List.rev backwards
    | _ => [[%stri let tests = ref []], ...List.rev [Injected.tap_reporter, ...backwards]]
    };
  },
};
