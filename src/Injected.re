

let tap_reporter = {
  let loc = Location.none;
  open Parsetree;
  [%stri let _ = {
    let si = string_of_int;
    let (total, errs) = List.fold_left
    (fun (total, errs) (name, fns) => {
      print_endline ("# section: " ^ name);
      List.fold_left
      (fun (total, errs) fn => {
        let (name, results) = fn ();
        List.fold_left
        (fun (total, errs) (subname, failure) => {
          let description = (string_of_int (total + 1)) ^ " " ^ name ^ ": " ^ subname;
          /*print_endline ("test " ^ name ^ " : " ^ subname);*/
          switch failure {
          | None => {
            print_endline ("ok " ^ description);
            (total + 1, errs)
          }
          | Some text => {
            print_endline ("not ok " ^ description);
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
    print_endline ("1.." ^ (si (total)));
    print_endline ("# tests " ^ (si total));
    print_endline ("# pass " ^ (si (total - errs)));
    print_endline ("# fail " ^ (si (errs)));
  }]
};

/*let make_final_test_block tests => {
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
};*/
