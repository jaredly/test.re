[@ocaml.ppx.context {cookies: []}];

let tests = ref([]);

[@test [(2, 4), (4, 8), ((-2), (-5))]]
[@test.item_name (input, _) => "input " ++ string_of_int(input)]
[@test.named [(0, 0, "null case"), (1, 2, "simple"), (3, 5, "bad")]]
[@test.show string_of_int]
[@test.check Some("    failure: thing")]
let awesome = n => n * 2;

TestRe.add(
  ~name="awesome",
  ~location="test/test.re:1:0",
  [
    () => ("awesome", [("0", Some("    failure: thing"))]),
    () => {
      let item_name = (i, input, output) =>
        ((input, _) => "input " ++ string_of_int(input))(input, output);
      let compare = (expected, result) => expected == result;
      let diff = {
        let show = string_of_int;
        let (fname, line, col) = ("test/test.re", 1, 7);
        (expected, result) =>
          "    expected: "
          ++ show(expected)
          ++ "\n"
          ++ "    actual:   "
          ++ show(result)
          ++ "\n"
          ++ "    at:       "
          ++ fname
          ++ " "
          ++ string_of_int(line)
          ++ ","
          ++ string_of_int(col);
      };
      let call = arg1 => awesome(arg1);
      (
        "awesome",
        List.mapi(
          (i, (input, expected)) => {
            let result = call(input);
            if (compare(expected, result)) {
              (item_name(i, input, expected), None);
            } else {
              (item_name(i, input, expected), Some(diff(expected, result)));
            };
          },
          [(2, 4), (4, 8), ((-2), (-5))]
        )
      );
    },
    () => {
      let item_name = (i, input, output) =>
        ((input, _) => "input " ++ string_of_int(input))(input, output);
      let compare = (expected, result) => expected == result;
      let diff = {
        let show = string_of_int;
        let (fname, line, col) = ("test/test.re", 3, 13);
        (expected, result) =>
          "    expected: "
          ++ show(expected)
          ++ "\n"
          ++ "    actual:   "
          ++ show(result)
          ++ "\n"
          ++ "    at:       "
          ++ fname
          ++ " "
          ++ string_of_int(line)
          ++ ","
          ++ string_of_int(col);
      };
      let call = arg1 => awesome(arg1);
      (
        "awesome",
        List.mapi(
          (i, (input, expected, name)) => {
            let result = call(input);
            if (compare(expected, result)) {
              (name, None);
            } else {
              (name, Some(diff(expected, result)));
            };
          },
          [(0, 0, "null case"), (1, 2, "simple"), (3, 5, "bad")]
        )
      );
    }
  ]
);

[@test [(2, 4)]]
[@test.diff
  (a, b) => "wanted " ++ string_of_int(a) ++ " instead of " ++ string_of_int(b)
]
let other = n => 3 * n;

TestRe.add(
  ~name="other",
  ~location="test/test.re:8:0",
  [
    () => {
      let item_name = (i, _, _) => "fixture " ++ string_of_int(i);
      let compare = (expected, result) => expected == result;
      let diff = {
        let message = (a, b) =>
          "wanted " ++ string_of_int(a) ++ " instead of " ++ string_of_int(b);
        let (fname, line, col) = ("test/test.re", 8, 7);
        (expected, result) =>
          "    custom message: "
          ++ message(expected, result)
          ++ "\n"
          ++ "    at:             "
          ++ fname
          ++ " "
          ++ string_of_int(line)
          ++ ","
          ++ string_of_int(col);
      };
      let call = arg1 => other(arg1);
      (
        "other",
        List.mapi(
          (i, (input, expected)) => {
            let result = call(input);
            if (compare(expected, result)) {
              (item_name(i, input, expected), None);
            } else {
              (item_name(i, input, expected), Some(diff(expected, result)));
            };
          },
          [(2, 4)]
        )
      );
    }
  ]
);

[@test [((1, 2), 2), ((3, 4), 12), ((5, 6), 7)]]
let multi = (a, b) => a * b;

TestRe.add(
  ~name="multi",
  ~location="test/test.re:12:0",
  [
    () => {
      let item_name = (i, _, _) => "fixture " ++ string_of_int(i);
      let compare = (expected, result) => expected == result;
      let diff = {
        let (fname, line, col) = ("test/test.re", 12, 7);
        (_, _) =>
          "    unexpected output: (add @@test.show to display)\n"
          ++ "    at:                "
          ++ fname
          ++ " "
          ++ string_of_int(line)
          ++ ","
          ++ string_of_int(col);
      };
      let call = ((arg1, arg2)) => multi(arg1, arg2);
      (
        "multi",
        List.mapi(
          (i, (input, expected)) => {
            let result = call(input);
            if (compare(expected, result)) {
              (item_name(i, input, expected), None);
            } else {
              (item_name(i, input, expected), Some(diff(expected, result)));
            };
          },
          [((1, 2), 2), ((3, 4), 12), ((5, 6), 7)]
        )
      );
    }
  ]
);

let _ = {
  print_endline("TAP version 13");
  let si = string_of_int;
  let (total, errs) =
    List.fold_right(
      ((name, fns), (total, errs)) => {
        print_endline("# " ++ name);
        List.fold_left(
          ((total, errs), fn) => {
            let (name, results) = fn();
            List.fold_left(
              ((total, errs), (subname, failure)) => {
                let description = string_of_int(total + 1) ++ " " ++ subname;
                switch failure {
                | None =>
                  print_endline("ok " ++ description);
                  (total + 1, errs);
                | Some(text) =>
                  print_endline("not ok " ++ description);
                  print_endline("  ---");
                  print_endline(text);
                  print_endline("  ...");
                  (total + 1, errs + 1);
                };
              },
              (total, errs),
              results
            );
          },
          (total, errs),
          fns
        );
      },
      tests^,
      (0, 0)
    );
  print_endline("1.." ++ si(total));
  print_endline("# tests " ++ si(total));
  print_endline("# pass " ++ si(total - errs));
  print_endline("# fail " ++ si(errs));
};
