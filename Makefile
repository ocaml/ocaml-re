JBUILDER ?= jbuilder

all:
	@$(JBUILDER) build @install @DEFAULT

test:
	@$(JBUILDER) runtest

check: test

clean:
	@$(JBUILDER) clean

.PHONY: check test all clean
