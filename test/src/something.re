
[@test [
  (("123abc", 0), 3),
  (("a123abc", 1), 3),
  (("abc", 1), 1),
  (("abc", 3), 3),
]]
let rec getNums = (text, pos) => {
  if (pos < String.length(text)) {
    switch (text.[pos]) {
    | '0'..'9' => getNums(text, pos + 1)
    | _ => pos
    }
  } else {
    pos
  }
};


