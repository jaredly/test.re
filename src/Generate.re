
let str_exp str => Ast_helper.Exp.constant (Pconst_string str None);

let process_check name i (cname, body) => {
  open Parsetree;
  let loc = Location.none;
  let cname = Utils.optor cname (string_of_int i);
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
  open Test;
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
  open Test;
  open Parsetree;
  let loc = Location.none;
  let call = switch (test.call) {
  | Some expr => expr
  | None => switch args {
    | Some args => [%expr fun [%p make_arg_pattern args] => [%e make_fncall name args]]
    | None => Utils.fail "Must use @@test.call with functions that have labels"
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

let test name test_name args test => {
  let loc = Location.none;
  open Test;
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
