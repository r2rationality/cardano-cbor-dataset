## Local development with Flox

A [`flox`](https://flox.dev) environment is provided with the project.

That environment installs on your shell:

 - GHC 9.6.7
 - Haskell Language Server (HLS 2.10.0.0, the packaged fallback)
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
just build                       # build the executable and the three test suites
just repl                        # GHCi on the executable
just repl-all                    # GHCi on the executable and tests
just test                        # run the unit and normalization-vectors suites
just integration                 # run the integration suite: slow, it reads the whole dataset
just unit                        # run the unit suite only
just unit 'Container lengths'    # run the unit tests that match a pattern
just format                      # format the Haskell sources
just format-check                # fail if a source file is not formatted
just lint                        # lint the library, executable, and tests with HLint
just vectors                     # check the normalization vectors through Docker
just refresh-dataset             # regenerate both corpora, their reports and the coverage page
just tricorder                   # open the Tricorder ui
just test-results                # print the latest Tricorder test results
```

Each recipe calls the matching script under `scripts/`, which activates the Flox environment itself.
You do not need an activated shell to run them.

`just refresh-dataset` needs a built `cbor` binary and the ledger generator `generate-cbor`. `cbor generate` calls the
generator through `PATH`. The script uses the `generate-cbor` on `PATH` if there is one, else the newest build under
`.cache/cardano-ledger/dist-newstyle`. To build it, run this command in `.cache/cardano-ledger`:

```sh
flox activate -d ../.. -- cabal build cardano-ledger-api:exe:generate-cbor
```

To call `cbor generate` yourself, put the generator on `PATH`:

```sh
generator=$(cd .cache/cardano-ledger && flox activate -d ../.. -- cabal list-bin cardano-ledger-api:exe:generate-cbor)
PATH="$(dirname "$generator"):$PATH" cbor generate --era conway dataset
```

Generation is the slow half, so `ERAS=conway just refresh-dataset` keeps a session to the era you are changing.

### VS Code

For VSCode, install the recommended [`haskell.haskell` extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
Then start VSCode from the shell that you activated with `flox`:
```
code .
```

The workspace uses `scripts/hls-local` to run HLS 2.15.0.0 with the Flox compiler.
On the first launch, the script builds HLS from source and saves it under `.cache/`.
This needs network access and takes several minutes. Later launches reuse the binary.
If the compiler changes, the script rebuilds HLS to match it.

To complete this build before you open the editor, run:

```sh
scripts/hls-local --version
```

The workspace loads the library, executable, and test components as their files open.
After an HLS upgrade, run **Haskell: Restart Haskell LSP Server** from the VS Code command palette.

### Formatting

Flox installs Ormolu as the Haskell formatter. Then, in VSCode, you can format a file with:

 - **Ormolu: Format current file**
 - **Ormolu: Format project**

### Tricorder

Tricorder watches the `app`, `exe` and `test` directories. It runs the unit and normalization-vectors suites after each
clean build, not the slow integration suite. You can open the Tricorder ui in flox-activated shell to observe compilation errors and test failures:
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

The GHCi console can be started with
```sh
just repl
```

or, if you want the test modules as well:
```sh
just repl-all
```


Notes:

 - [Relude](https://hackage.haskell.org/package/relude) is the project prelude, so `Text`, `Map` and `readMaybe` are in scope without an import.
 - `just repl` reads the `.ghci` configuration which quiets `incomplete-pattern` and deprecation warnings.
 - GHCi prints a `Show` value on one line. For a readable nested value, use `prettyEval` from `app/Base/Eval.hs`.
   The example below uses `obligationsFor`, a helper of the `ObligationsSpec` test module, so it needs `just repl-all`:

```haskell
prettyEval ((`obligationsFor` Name "anchor") <$> eraRoot "conway")
```
