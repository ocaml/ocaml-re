JBUILDER ?= jbuilder

all:
	@$(JBUILDER) build

test:
	@$(JBUILDER) runtest

check: test

clean:
	@$(JBUILDER) clean

.PHONY: check test all clean
