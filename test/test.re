
let awesome n => n * 2
[@@test [
  (2, 4),
  (4, 8),
  (-2, -5),
]]
[@@test.named [
  (0, 0, "null case"),
  (1, 2, "simple"),
  (3,5, "bad")
]]
[@@test.show string_of_int]
[@@test.check {
  Some "failure"
}]
and other n => 3 * n
[@@test [
  (2, 4),
]]
[@@test.diff fun a b => ("wanted " ^ (string_of_int a) ^ " instead of " ^ (string_of_int b))]
;