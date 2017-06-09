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
};

/**
 * let fn a => a + 2
 * [@@test [(in, out), (in, out)]]
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

let process_attributes attributes => {

};

type test = {
  fixtures: option Parsetree.expression,
  diff: option Parsetree.expression,
  show: option Parsetree.expression,
  compare: option Parsetree.expression,
  checks: list Parsetree.expression,
  name: option string,
};

/*
let process_bindings mapper bindings => {
  List.fold_left
  (fun tests binding => {
    let (name, fixtures, ) = process_attributes binding.pvb_attributes;
    switch (getInfo binding) {
    | None => switch (name, body) {
      | (None, None) => tests
      | _ => raise "bad ness"
    }
    | Some (fname, args) => {
        let name = switch (name) { |None => fname | Some name => name};
      }
    }
    switch (test.body) {
    | None => tests
    | Some body => {
      let name = switch (test.name) {
      | Some name => name
      | None => binding.
      }
    }
    }
  })
  []
  bindings
}
*/

let getenv_mapper argv => {
  /*print_endline "Argv";*/
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: fun mapper items => {
      let loc = Location.none;
      let (backwards, tests) = List.fold_left (fun (results, tests) item => {
        switch (item.Parsetree.pstr_desc) {
        | Pstr_value _ bindings => {
          let test_strs = [];
          /*process_bindings mapper bindings;*/
          (test_strs @ [
            [%stri let _ = tests := [("hello", fun () => Some "Failed"), ...!tests]],
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
