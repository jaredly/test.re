let make_final_test_block tests => {
  let loc = Location.none;
  open Parsetree;
  [%stri let _ = {
    let (total, errs) = List.fold_left
    (fun (total, errs) (name, fns) => {
      print_endline ("section: " ^ name);
      List.fold_left
      (fun (total, errs) fn => {
        let (name, results) = fn ();
        List.fold_left
        (fun (total, errs) (subname, failure) => {
          print_endline ("test " ^ name ^ " : " ^ subname);
          switch failure {
          | None => (total + 1, errs)
          | Some text => {
            /*print_endline name;*/
            /*print_endline subname;*/
            print_endline text;
            (total + 1, errs + 1)
          }
          }
        })
        (total, errs)
        results
      })
      (total, errs)
      fns
    })
    (0, 0)
    !tests;
    print_endline ("Done! " ^ (string_of_int errs) ^ " failed out of " ^ (string_of_int total))
  }]
};

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
 * [@@@test.run {
 *   print_endline "do arbitrary things"
 * }]
 * 
 * main() [@@test.hide];
 */

type test = {
  fixtures: option Parsetree.expression,
  named_fixtures: option Parsetree.expression,
  call: option Parsetree.expression,
  diff: option Parsetree.expression,
  show: option Parsetree.expression,
  compare: option Parsetree.expression,
  checks: list (option string, Parsetree.expression),
  name: option string,
};

exception Badness string;

let fail x => raise (Badness x);

let payload_expr payload => switch payload {
| Parsetree.PStr [{pstr_desc: Pstr_eval expr _}] => Some expr
| _ => None
};

let require_payload_expr payload => switch (payload_expr payload) {
  | None => fail "@@test payload must be an expression"
  | Some expr => expr
};

let require_payload_string payload => switch payload {
| Parsetree.PStr [{pstr_desc: Pstr_eval {pexp_desc: Pexp_constant (Pconst_string value None)} []}] => value
| _ => fail "@@test.name payload must be a string"
};

/* TODO if the first thing is a string literal, grab that out & use it as a name */
let process_check_expr expr => {
  (None, expr)
};

let process_attributes attributes => {
  List.fold_left
  (fun test (name, payload) => {
    switch name.Location.txt {
    | "test" => {
        let expr = require_payload_expr payload;
        switch (test.fixtures) {
        | None => {...test, fixtures: Some expr}
        | _ => fail "multiple @@test annotations found"
        }
      }
    | "test.named" => {
        let expr = require_payload_expr payload;
        switch (test.named_fixtures) {
        | None => {...test, named_fixtures: Some expr}
        | _ => fail "multiple @@test.named annotations found"
        }
      }
    | "test.call" => {
        let expr = require_payload_expr payload;
        switch (test.call) {
        | None => {...test, call: Some expr}
        | _ => fail "multiple @@test.call annotations found"
        }
      }
    | "test.show" => {
        let expr = require_payload_expr payload;
        switch (test.show) {
        | None => {...test, show: Some expr}
        | _ => fail "multiple @@test.show annotations found"
        }
      }
    | "test.diff" => {
        let expr = require_payload_expr payload;
        switch (test.diff) {
        | None => {...test, diff: Some expr}
        | _ => fail "multiple @@test.diff annotations found"
        }
      }
    | "test.compare" => {
        let expr = require_payload_expr payload;
        switch (test.compare) {
        | None => {...test, compare: Some expr}
        | _ => fail "multiple @@test.compare annotations found"
        }
      }
    | "test.name" => {
        let name = require_payload_string payload;
        switch (test.name) {
        | None => {...test, name: Some name}
        | _ => fail "multiple @@test.name annotations found"
        }
      }
    | "test.check" => {
        let expr = require_payload_expr payload;
        {...test, checks: [(process_check_expr expr), ...test.checks]}
      }
    | _ => test
    }
  })
  {fixtures: None, named_fixtures: None, call: None, diff: None, show: None, compare: None, checks: [], name: None}
  attributes
};

let validate test => switch test {
| {fixtures: None, named_fixtures: None, diff: None, show: None, compare: None, checks: [], name: None, call: None} => None
| {fixtures: None, named_fixtures: None, diff: Some _}
| {fixtures: None, named_fixtures: None, show: Some _}
| {fixtures: None, named_fixtures: None, call: Some _}
| {fixtures: None, named_fixtures: None, compare: Some _} => fail "Doesn't make sense to not have fixtures"
| {diff: Some _, show: Some _} => fail "Diff and show together doesn't make sense"
| {fixtures: None, named_fixtures: None, checks: []} => fail "Partial attributes.. doesn't make sense"
| _ => Some test
};

let optor opt orr => switch opt { |None => orr| Some x => x};

let str_exp str => Ast_helper.Exp.constant (Pconst_string str None);

let process_check name i (cname, body) => {
  open Parsetree;
  let loc = Location.none;
  let cname = optor cname (string_of_int i);
  [%expr fun () => ([%e str_exp name], [([%e str_exp cname], [%e body])])]
};

let make_fncall name count => {
  open Parsetree;
  let loc = Location.none;
  [%expr [%e Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident name))] arg1];
};
let make_arg_pattern count => {
  open Parsetree;
  let loc = Location.none;
  [%pat ? arg1];
};

let test_diff test => {
      let loc = Location.none;
  switch (test.diff) {
  | Some expr => expr
  | None => switch (test.show) {
    | Some expr => {
      [%expr {
        let show = [%e expr];
        fun expected result => "Got " ^ (show result) ^ ", expected " ^ (show expected)
      }]
    }
    | None => [%expr fun _ _ => "unexpected output"] /* TODO if bucklescript, to js.log here */
  }
};
};

/* TODO allow multiple fixtures definitions? And just chain them... */
let process_fixtures fixtures named name test_name args test => {
  open Parsetree;
  let loc = Location.none;
  let call = switch (test.call) {
  | Some expr => expr
  | None => switch args {
    | Some args => [%expr fun [%p make_arg_pattern args] => [%e make_fncall name args]]
    | None => fail "Must use @@test.call with functions that have labels"
    }
  };
  let fixture_args = named ? [%pat ? (input, expected, name)] : [%pat ? (input, expected)];
  let fixture_name = named ? [%expr name] : [%expr string_of_int i];
  let compare = switch (test.compare) {
    | Some expr => expr
    | None => [%expr fun expected result => expected == result]
  };
  let diff = test_diff test;

  [%expr fun () => {
    let compare = [%e compare];
    let diff = [%e diff];
    let call = [%e call];
    ([%e str_exp test_name], List.mapi
    (fun i [%p fixture_args] => {
      let result = call input;
      if (compare expected result) {
        /* TODO allow custom "name"s from arguments, test.fixture.name input => string */
        ([%e fixture_name], None)
      } else {
        ([%e fixture_name], Some (diff expected result))
      }
    })
    [%e fixtures])
  }]
};

let rec make_list checks => {
  open Parsetree;
  let loc = Location.none;
  switch checks {
| [] => [%expr []]
| [item, ...rest] => [%expr [[%e item], ...[%e make_list rest]]]
  }
};

let process_test name test_name args test => {
  let loc = Location.none;
  open Parsetree;
  let checks = List.mapi (process_check test_name) test.checks;
  let checks = switch (test.fixtures) {
  | None => checks
  | Some fixtures => [process_fixtures fixtures false name test_name args test, ...checks]
  };
  let checks = switch (test.named_fixtures) {
  | None => checks
  | Some fixtures => [process_fixtures fixtures true name test_name args test, ...checks]
};
  switch checks {
  | [] => None
  | _ => Some [%stri let _ = tests := [([%e str_exp name], [%e make_list checks]), ...!tests]]
  }
};

let rec count_args {Parsetree.pexp_desc} => Parsetree.(switch pexp_desc {
| Pexp_fun Nolabel _ _ expr => switch (count_args expr) {
  | Some n => Some (1 + n)
  | None => None
}
| Pexp_fun _ => None
| _ => Some 0
});

/* TODO allow ppl to just say [@@test.arity 3] if they want explicit, and then it doesn't need to be a function literal */
let getInfo {Parsetree.pvb_pat, pvb_expr} => {
  open Parsetree;
  switch (pvb_pat.ppat_desc) {
  | Ppat_var {txt} => switch (pvb_expr.pexp_desc) {
    | Pexp_fun Nolabel _ _ expr => switch (count_args expr) {
        | Some n => Some (txt, Some (1 + n))
        | None => Some (txt, None)
      }
    | Pexp_fun _ => Some (txt, None)
    | _ => None
    }
  | _ => None
  }
};

let process_bindings mapper bindings => {
  open Parsetree;
  List.fold_left
  (fun tests binding => {
    let test = process_attributes binding.pvb_attributes;
    switch (validate test) {
    | None => tests
    | Some test => {
      switch (getInfo binding) {
      | None => fail "test attributes on a non-function"
      | Some (name, args) => {
        let test_name = optor test.name name;
        switch (process_test name test_name args test) {
        | None => tests
        | Some str => [str, ...tests]
        }
      }
      }
    }
    }
  })
  []
  bindings
};


let getenv_mapper argv => {
  /*print_endline "Argv";*/
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: fun mapper items => {
      let loc = Location.none;
      let (backwards, tests) = List.fold_left (fun (results, tests) item => {
        switch (item.Parsetree.pstr_desc) {
        | Pstr_value _ bindings => {
          let test_strs = process_bindings mapper bindings;
          (test_strs @ [
            /*[%stri let _ = tests := [("hello", fun () => Some "Failed"), ...!tests]],*/
            Ast_mapper.default_mapper.structure_item mapper item,
            ...results
          ], tests + 1)
        }
        | _ => ([Ast_mapper.default_mapper.structure_item mapper item, ...results], tests)
        }
      }) ([], 0) items;
      switch (tests) {
      | 0 => List.rev backwards
      | _ => [[%stri let tests = ref []], ...List.rev [make_final_test_block tests, ...backwards]]
      };
    },
  };
};

let () = Ast_mapper.register "ppx_test" getenv_mapper;
