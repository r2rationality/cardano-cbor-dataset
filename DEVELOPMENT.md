## Local development with Flox

A [`flox`](https://flox.dev) environment is provided with the project.

That environment installs on your shell:

 - GHC 9.6.7
 - Haskell Language Server (HLS 2.10.0.0)
 - Cabal
 - [Tricorder](https://github.com/tweag/tricorder)
 - [just](https://just.systems), the command runner for the recipes below.

From the repository root, you can activate the environment with:

```sh
flox activate

# the following executables should be available upon activation
ghc --version
haskell-language-server-wrapper --version
tricorder --help
```

Then call `just setup` to fetch a checkout of the cardano ledger files and build some native libraries.

### Common commands

A `justfile` collects the usual commands. Flox installs `just`. Run `just` to list them:

```sh
just setup                       # fetch the ledger checkout and build the native libraries
just build                       # build the executable and both test suites
just repl                        # GHCi on the executable
just repl-all                    # GHCi on the executable and tests
just test                        # run every test suite
just unit                        # run the unit suite only
just unit 'Conway reachability'  # run the unit tests that match a pattern
just format                      # format the Haskell sources
just format-check                # fail if a source file is not formatted
just vectors                     # check the normalization vectors through Docker
just refresh-dataset             # regenerate both corpora and the coverage page
just tricorder                   # open the Tricorder ui
```

Each recipe calls the matching script under `scripts/`, which activates the Flox environment itself.
You do not need an activated shell to run them.

`just refresh-dataset` needs a built `cbor` binary and the ledger generator under `.cache/`.
Generation is the slow half, so `ERAS=conway just refresh-dataset` keeps a session to the era you are changing.

### VS Code

For VSCode, install the recommended [`haskell.haskell` extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
Then start VSCode from the shell that you activated with `flox`:
```
code .
```

### Formatting

Flox installs Ormolu as the Haskell formatter. Then, in VSCode, you can format a file with:

 - **Ormolu: Format current file**
 - **Ormolu: Format project**

### Tricorder

Tricorder watches `app` and `test` components. You can open the Tricorder ui in flox-activated shell to observe compilation errors and test failures:
```sh
just tricorder
```

#### For coding agents

A Tricorder MCP server can be configured for this project using a prompt such as:

> Compile and check Haskell through the Tricorder MCP server, never with `cabal build` or `cabal test`.
> Call `status` first; if the daemon is stopped, call `start` and wait for the first build to finish.
> Read compiler errors and warnings from `status`, test failures from `test_results`,
> and `-- >>>` expression results from `eval_comments`.
> Never call `restart` or `stop` while a build or a Cabal command is in flight.

Use it instead of `cabal build`. If a build is already running in the tricorder daemon, then a second one blocks on the Cabal lock.

### GHCi

To try an expression without Tricorder or HLS, start a REPL on the executable:

```sh
just repl
```

That is `cabal repl cbor`. Naming the component matters: the application modules belong to the test
suites as well, so a bare module name is an ambiguous Cabal target.

To reach the test modules too, load every component at once with `just repl-all`.

Both read `.ghci`, which only quiets incomplete-pattern and deprecation warnings at the prompt.
Source modules keep the project's warnings-as-errors settings.

Notes:

 -[Relude](https://hackage.haskell.org/package/relude) is the project prelude, so `Text`, `Map` and `readMaybe` are in scope without an import.
 - GHCi prints a `Show` value on one line. For a readable nested value, use `prettyEval` from `app/Eval.hs`:

```haskell
prettyEval (eraReachability "conway" ["TransactionBody"])
```
