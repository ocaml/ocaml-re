DUNE ?= dune

all:
	@$(DUNE) build

test:
	@$(DUNE) runtest

check:
	@$(DUNE) build @runtest @check

.PHONY: check test all clean

.PHONY: release
release: ## Release on Opam
	dune-release distrib --skip-build --skip-lint --skip-tests
	dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
