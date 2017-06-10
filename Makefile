main:
	jbuilder build ppx_test.exe

watch:
	watchexec -i _build jbuilder build test/test.output

test:
	jbuilder build test/test.output

.PHONY: test main
