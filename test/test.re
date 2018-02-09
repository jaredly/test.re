/**
 * Ok, here's what I think I want:
 *
 * -> basic fixtures, where I provide named or unnamed test cases
 *  [
 *    (input, output, name)
 *  ]
 *  and a compare function (default (=))
 *  and a print function (default () => "cannot print") orr I do basic constant analysis of the fixtures.
 *  if I can't infer, I could have them specify the type of the output, like `@test.autoprint: int`
 *
 * -> this turns into
 * TestRe.add(
 *   ~name,
 *   ~location,
 *   ~print=(either a user-supplied printer, or an inferred printer, or a "cannot print sry" printer),
 *   ~fixtures=[],
 *   ~named=[],
 *   ~customs=[],
 *   ~bools=[],
 * )
 *
 *   [all of the fixtures you know])
 *
 * The fixtures look like:
 *
 * [
 *
 * ]
 *
 * So, can supply
 * [@test.print x => Format.fprintf(x, "%d")]
 * [@test.print Fmt.str]
 * [@test.autoprint: string]
 *
 * So that's the basic "test" thing.
 * [@test.name "thing"]
 *
 * [@test.custom 2 == 2 ? None : Some("math is broken")]
 * [@test.bool 3 == 4]
 */


[@test [(2, 4), (4, 8), ((-2), (-5))]]
[@test.item_name (input, _) => "input " ++ string_of_int(input)]
[@test.named [(0, 0, "null case"), (1, 2, "simple"), (3, 5, "bad")]]
[@test.show string_of_int]
[@test.check Some("    failure: thing")]
let awesome = (n) => n * 2;

[@test [(2, 4)]]
[@test.diff (a, b) => "wanted " ++ string_of_int(a) ++ " instead of " ++ string_of_int(b)]
let other = (n) => 3 * n;

[@test [((1, 2), 2), ((3, 4), 12), ((5, 6), 7)]]
let multi = (a, b) => a * b;
