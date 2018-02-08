/***
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
let () = Ast_mapper.register("ppx_test", (args) => Ppx_test_lib.TestMapper.mapper);
