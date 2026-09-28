# Common development commands. Run `just` to list them.
#
# The scripts under scripts/ activate the project Flox environment themselves,
# so these recipes work from any shell.

default:
    @just --list

# Fetch the Cardano Ledger checkout and build the native crypto libraries.
setup:
    flox activate -d . -- scripts/setup-cache

# Build the executable and the test suites.
build:
    flox activate -d . -- cabal build --enable-tests all

# GHCi on the executable, for trying an expression.
repl:
    flox activate -d . -- cabal repl cbor

# GHCi on every component, including the test modules.
repl-all:
    flox activate -d . -- cabal repl --enable-multi-repl cardano-cbor-dataset

# Run the unit and normalization suites.
test:
    scripts/test

# Check the whole dataset before pushing it. Slow: it reads every sample.
integration:
    scripts/test --integration

# Run the unit suite, optionally filtered: `just unit 'Conway reachability'`.
unit filter='':
    #!/bin/sh
    set -eu
    if [ -n "{{filter}}" ]; then
      scripts/test --unit --test-options="--match \"{{filter}}\""
    else
      scripts/test --unit
    fi

# Check the committed normalization vectors through Docker.
vectors:
    scripts/check-normalization-vectors

# Format the Haskell sources in place.
format:
    scripts/format

# Fail if any source file is not formatted.
format-check:
    scripts/format --check

# Lint the library, executable, and tests with HLint.
lint:
    flox activate -d . -- hlint app exe test

# Regenerate both corpora, their conformance reports and the coverage page.
refresh-dataset:
    scripts/refresh-dataset

# Open the Tricorder watcher ui.
tricorder:
    flox activate -d . -- scripts/tricorder ui

# Print the latest Tricorder test results without the ui.
test-results:
    flox activate -d . -- scripts/tricorder test-results
