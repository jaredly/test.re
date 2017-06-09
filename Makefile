main:
	jbuilder build ppx_test.exe

test:
	refmt -p ml test/test.re > out.ml
	ocamlc -dsource -ppx ./_build/default/ppx_test.exe out.ml 2> res.ml
	refmt --parse ml res.ml > test/test.actual.re
	rm res.ml out.* a.out


.PHONY: test main