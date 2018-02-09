
let module S_ = {
  include Src.Something;
};

/* Src.Something.getNums("awe123", 1); */
print_endline("Hello");
TestRe.report();

print_endline(string_of_int(List.length(TestRe.tests^)))
