.PHONY: build run test clean grammar install fmt FORCE

DUNE_FLAGS := -j auto

build:
	dune build $(DUNE_FLAGS)

RUN_ARGS := $(or $(FILE),$(filter-out run,$(MAKECMDGOALS)))

run:
ifeq ($(strip $(RUN_ARGS)),)
	dune exec $(DUNE_FLAGS) ./bin/main.exe
else
	dune exec $(DUNE_FLAGS) ./bin/main.exe -- $(RUN_ARGS)
endif

# Swallow extra positional goals so `make run path/to/file` works
FORCE:

%: FORCE
	@:

test:
	dune test $(DUNE_FLAGS)

install:
	dune install

fmt:
	dune build @fmt --auto-promote $(DUNE_FLAGS)

grammar:
	cd grammar && bnfc --ocaml Lang.cf

clean:
	dune clean
