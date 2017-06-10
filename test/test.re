
let awesome n => n * 2
[@@test [
  (2, 4),
  (4, 8),
  (-2, -4),
]]
and other n => 3 * n
[@@test [
  (2, 6),
]]
;