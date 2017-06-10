
type t = {
  fixtures: option Parsetree.expression,
  named_fixtures: option Parsetree.expression,
  item_name: option Parsetree.expression,
  skipif: option Parsetree.expression,
  skip: option string,
  only: bool,
  todo: option string,
  call: option Parsetree.expression,
  diff: option Parsetree.expression,
  show: option Parsetree.expression,
  compare: option Parsetree.expression,
  checks: list (option string, Parsetree.expression),
  name: option string,
};

let empty = {item_name: None, fixtures: None, named_fixtures: None, call: None, diff: None, show: None, compare: None, checks: [], name: None, skip: None, todo: None, skipif: None, only: false};

let validate test => switch test {
| {fixtures: None, named_fixtures: None, diff: None, show: None, compare: None, checks: [], name: None, call: None} => None
| {fixtures: None, named_fixtures: None, diff: Some _}
| {fixtures: None, named_fixtures: None, show: Some _}
| {fixtures: None, named_fixtures: None, call: Some _}
| {fixtures: None, named_fixtures: None, compare: Some _} => Utils.fail "Doesn't make sense to not have fixtures"
| {diff: Some _, show: Some _} => Utils.fail "Diff and show together doesn't make sense"
| {fixtures: None, named_fixtures: None, checks: []} => Utils.fail "Partial attributes.. doesn't make sense"
| _ => Some test
};
