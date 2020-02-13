PACKAGE-NAME=rackjure

# Racket 6.1 adds pkg dep checking.
ifeq ($(findstring "$(RACKET_VERSION)", "6.0", "6.0.1"),)
	DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps
else
	DEPS-FLAGS=
endif

all: setup

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Note: Also builds docs (if any) and checks deps.
setup:
	raco setup --tidy --avoid-main $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

# Suitable for both day-to-day dev and CI
test:
	raco test -x -p $(PACKAGE-NAME)
