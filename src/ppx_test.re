
/**
 * let fn a => a + 2
 * [@@test [(in, out), (in, out)]]
 * [@@test.named [(in, out, name), (in, out, name)]]
 * 
 * [@@test.compare fun a b => a == b] /* default */
 * 
 * one of these two is useful (until we have modular implicits)
 * [@@test.diff fun a b => "got %{a} expected %{b}"]
 * [@@test.show fun a => string_of_int a]
 * orrr if we're in js-land, I can do `Js.log`, but how will I know?
 * maybe I'll have a flag that's like "bucklescript"
 * 
 * [@@test.check {
 *   "name of the check";
 *   let x = myfn 10;
 *   if (x > 3) {
 *     Some "wasn't supposed to be greater than 3"
 *   } else {
 *     None
 *   }
 * }]
 * [@@test.name "my special thing"]
 * 
 * [@@test.skip "explanation"] the reason why you're skipping
 * [@@test.todo "explanation"]
 * [@@test.skipif true === true]
 * 
 * ; custom calling, for functions with labels, or things that can't be inferred
 * [@@test.call fun input => myfun input 3]
 * 
 * [@@@test.run {
 *   print_endline "do arbitrary things"
 * }]
 * 
 * main() [@@test.hide];
 */

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

let test_mapper argv => {
  Parsetree.{
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
};

let () = Ast_mapper.register "ppx_test" test_mapper;
