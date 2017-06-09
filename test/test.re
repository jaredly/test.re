
let awesome n => n * 2
[@@expect [
  (2, 4),
  (4, 8),
  (-2, -8),
]]
and other n => 3 * n
[@@expect [
  (2, 6),
]]
;