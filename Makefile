# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: all clean

default: all test

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
# The flag "--profile release" is passed to avoid warnings-as-errors

all:
	dune build --profile release @install 
	@test -L main.native || ln -s _build/install/default/bin/main.native main.native
	@test -L printanalysis.native || ln -s _build/install/default/bin/printanalysis.native printanalysis.native

test: main.native
	./main.native --test

# Clean up
clean:
# Remove files produced by dune.
	dune clean
# Remove remaining files/folders ignored by git as defined in .gitignore (-X).
	git clean -dfXq

utop:
	dune utop . --profile release
