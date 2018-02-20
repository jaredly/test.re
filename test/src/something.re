
[@test [
  (("123abc", 0), 3),
  (("a123abc", 1), 4),
  (("abc", 1), 2),
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

[@test [
  ("hello", "hello folks"),
  ("whats kickin", "what is kickin folks"),
]]
[@test.print (fmt, x) => Format.fprintf(fmt, "%S", x)]
let willFail = text => {
  text ++ " folks"
};


