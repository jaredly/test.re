let tests = ref [];

let awesome n => n * 2;

let _ = tests := [("hello", fun () => Some "Failed"), ...!tests];

let _ = {
  let (total, errs) =
    List.fold_left
      (
        fun (total, errs) (name, fn) =>
          switch (fn ()) {
          | Some err =>
            print_endline err;
            (total + 1, errs + 1)
          | None => (total + 1, errs)
          }
      )
      (0, 0)
      !tests;
  print_endline ("Done! " ^ string_of_int errs ^ " failed out of " ^ string_of_int total)
};
