main:
	jbuilder build ppx_test.exe

test:
	jbuilder build test/test.output

.PHONY: test main