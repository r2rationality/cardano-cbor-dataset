# Common development commands. Run `just` to list them.
#
# The scripts under scripts/ activate the project Flox environment themselves,
# so these recipes work from any shell.

default:
    @just --list

# Fetch the Cardano Ledger checkout and build the native crypto libraries.
setup:
    flox activate -d . -- scripts/setup-cache

# Build the executable and both test suites.
build:
    flox activate -d . -- cabal build --enable-tests all

# GHCi on the executable, for trying an expression.
repl:
    flox activate -d . -- cabal repl cbor

# GHCi on every component, including the test modules.
repl-all:
    flox activate -d . -- cabal repl --enable-multi-repl cardano-cbor-dataset

# Run every test suite.
test:
    scripts/test

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

# Regenerate both corpora and the coverage page.
refresh-dataset:
    scripts/refresh-dataset

# Open the Tricorder watcher ui.
tricorder:
    flox activate -d . -- scripts/tricorder ui

# Print the latest Tricorder test results without the ui.
test-results:
    flox activate -d . -- scripts/tricorder test-results
