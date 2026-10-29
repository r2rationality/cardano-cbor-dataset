# Cardano CBOR dataset

This project generates deterministic CBOR corpora for Conway and Dijkstra eras from the Cardano Ledger Huddle/CDDL generator.
This test data can be used by any Cardano node implementation to check its conformance with respect to the decoding of on-chain data.

The two intended users for this project are:

 1. Dataset maintainers

They are responsible for making sure that the latest generated data covers as much as possible of the specification
for Cardano data but also any edge cases that could see different node implementations diverge in their decoding of
the data.

 2. Dataset users

They are node implementors who want to generate conformance reports and attest that their node implementation decodes
all the test data correctly.

## How the dataset is made

The `cbor` executable of this project builds and checks each corpus in five steps:

 1. **Generation.** `generate-cbor`, an executable of the Cardano Ledger, produces random CBOR values for a rule of the
    era's HuddleSpec, the Haskell form of the CDDL specification. It also produces mutated values, named "zaps".
 2. **Classification.** The Haskell node decoder of the rule decides where each value goes: `valid` when it decodes,
    `invalid-zap-<n>` when it is refused. Hand-written samples go in `manual-valid` and `manual-invalid`.
 3. **Verification.** `cbor verify` checks that the Haskell node decodes or refuses every sample as its directory says,
    and that each accepted sample re-encodes to its `.expected.cbor` file.
 4. **Coverage.** `cbor generate` derives obligations from the specification, for example "a map with a duplicate key
    must be refused", and checks that samples meet each of them. The result is saved in `<era>/coverage.json`.
 5. **Reporting.** `cbor report` writes an HTML page from the `coverage.json` files, and `scripts/make-report.py`
    writes a Markdown summary of each verification.

The details are in [GENERATION.md](GENERATION.md), [VERIFICATION.md](VERIFICATION.md) and [COVERAGE.md](COVERAGE.md).

## Cardano CBOR dataset for maintainers

### Installation

You can either use the provided `flox` environment (see [DEVELOPMENT.md](DEVELOPMENT.md)) or build the `cbor` executable
with Docker.
From the repository root build the `cbor` image:
```
docker build --platform=linux/amd64 -t cbor .
```

Then you can run `cbor` commands with Docker:
```
docker run --rm --platform=linux/amd64 cbor --help
docker run --rm --platform=linux/amd64 cbor list-eras
docker run --rm --platform=linux/amd64 cbor list-rules --era conway
```

### Generate data

Create a writable `dataset` directory and generate samples for the Conway era:
```
docker run --rm --platform=linux/amd64 -v "$PWD/dataset:/output" cbor generate --era conway /output
```

In the other examples on this page, we will omit the Docker part of the command:
```
cbor generate --era conway /output
```

The output will be in the host directory: `$PWD/dataset/conway`.

The Docker image contains `generate-cbor`. When you run the `cbor` binary outside Docker, `generate` calls
`generate-cbor` through `PATH`, so you must build it first (see [DEVELOPMENT.md](DEVELOPMENT.md)).

The options for the `generate` command are

| Option                   | Description                                                                          |
|--------------------------|--------------------------------------------------------------------------------------|
| `--era ERA`              | `conway` or `dijkstra`.                                                              |
| `--config FILE`          | A configuration file, see [Generation configuration](#generation-configuration).     |
| `--only RULE`            | Only generate data for this rule. Repeat the option for several rules.               |
| `--no-verify`            | Do not verify the new corpus before it replaces the previous one.                    |
| `--no-coverage-check`    | Do not measure the specification coverage of the new corpus.                         |
| `--accept-coverage-loss` | Replace the previous corpus even if the new one covers less of the specification.    |
| `<OUTPUT>`               | The output directory (argument).                                                     |

The corpus is built in a staging directory next to `<OUTPUT>/<era>`, then checked:

 1. `verify` checks every sample (see [Verification](#verification)).
 2. The coverage of the specification is measured and written to `<OUTPUT>/<era>/coverage.json`.
    If the corpus already exists, the new measurement must not lose coverage against its `coverage.json`.

The new corpus replaces `<OUTPUT>/<era>` only if both checks pass. Then the verification results are written to
`<OUTPUT>/reports/<era>/haskell-latest.json`.

When `<OUTPUT>/<era>` already exists, `generate` regenerates it in place:

 - The `manual-valid` and `manual-invalid` samples of every rule are kept.
 - With `--only`, the rules that are not named are kept unchanged.
 - Without `--config`, the `corpus.json` of the existing corpus is used.

See [COVERAGE.md](COVERAGE.md) and [Coverage reporting](#coverage-reporting) for more information about the dataset
coverage.

#### Generation configuration

The parameters driving the generation are read from a json file named `corpus.json` located inside the dataset directory.
For example `dataset/conway/corpus.json` for the `conway` dataset.
If there is no corpus yet and no `config` option, `generate` uses default parameters and writes them to `corpus.json`.

The `corpus.json` file contains the following fields:
```json
{
  "seed": 123,
  "samples": 100,
  "rules": {
    "%constr<plutus_data>": 800
  },
  "divergences": [
    {
      "kind": "verified-by-parent",
      "rule": "certificates",
      "path": "validator.gen.choice[0:tag258].tag258",
      "obligation": {"tag": "TooFew", "contents": 1},
      "samples": ["certificates/manual-invalid/bare-empty"],
      "reason": "OSet decodes an empty set. The transaction body decoder refuses it at key 4."
    }
  ]
}
```

 - `seed` is the non-negative decimal seed every rule's sample stream is derived from.
 - `samples` is how many samples each rule gets in each generated category (`valid`, `invalid-zap-<n>`).
 - `rules` is an optional field that can be used to modify the number of samples for specific rules.
 - `zapGiveUpAfter` is optional, 3 by default. For a mutated (`invalid-zap-<n>`) category, the generator can report
   that a rule has too few decision points to mutate. After this number of such batches without a single sample,
   generation gives up on the category instead of spending its whole budget of attempts.
 - `batchSize` is optional, 128 by default. It is the largest number of samples asked from `generate-cbor` in one
   call. Each call loads the specification again, so larger batches make fewer calls.
 - `divergences` is an optional list of the places where the Haskell node and the specification disagree (see
   [Divergences](#divergences)).

##### Divergences

A divergence records a place where the Haskell node and the specification disagree, and why. Its fields are:

 - `kind`: how they disagree. The kind decides which other fields the divergence needs:

   | Kind                      | Specification | Node    | `rule`, `path`, `obligation` | `samples`      |
   | ------------------------- | ------------- | ------- | ---------------------------- | -------------- |
   | `verified-by-parent`      | refuses       | accepts | required                     | manual-invalid |
   | `incorrect-specification` | refuses       | accepts | required                     | manual-valid   |
   | `decoder-exception`       | accepts       | refuses | none                         | manual-invalid |
   | `unstated-constraint`     | accepts       | refuses | optional                     | manual-invalid |

   - `verified-by-parent`: another decoder enforces the constraint, for example the decoder of the transaction body.
     A decoder for the rule can check the constraint directly, so the samples are in `manual-invalid`. The Haskell
     decoder of the rule accepts them, and `verify` checks that it does.
   - `incorrect-specification`: nothing enforces the constraint, and the specification is wrong to ask for it.
   - `decoder-exception`: the decoder raises an exception rather than a decode error. The node refuses the bytes all
     the same, and `verify` counts the exception as a refusal.
   - `unstated-constraint`: the decoder checks a condition that the specification does not state, for example a
     denominator that is not zero. When the condition refuses a form that an obligation asks the node to accept, such
     as an indefinite array, the divergence names that obligation and settles it.
 - `rule`: the rule that states the obligation.
 - `path`: where the rule states the obligation, written as the coverage report writes it, for example
   `validator.gen.choice[0:tag258].tag258` (see [Paths](COVERAGE.md#paths)).
 - `obligation`: the obligation that the divergence settles, in its JSON form, for example
   `{"tag": "TooFew", "contents": 1}` for "fewer than 1 entry".
 - `samples`: at least one manual sample that shows the divergence, named `<rule>/<sample type>/<stem>`, for example
   `certificates/manual-invalid/bare-empty`. The kind decides the sample type (see the table above).
 - `reason`: why the node and the specification disagree. The coverage report shows it.

The coverage report gives a waived obligation the verdict **Waived**, not **Outstanding** (see
[COVERAGE.md](COVERAGE.md#how-coverage-is-checked)), and lists every divergence with its reason.

The measurement also compares the divergences with the manual samples. A manual-invalid sample that meets no obligation
to reject of its rule is one that the specification accepts. A manual-valid sample that meets one is one that the
specification refuses. The run stops if such a sample has no divergence, if a divergence names a sample that the
specification does not judge as its kind says, or if a divergence names an obligation that its rule does not state.
`verify` fails if a divergence names a sample that does not exist.

An implementation never needs `corpus.json` to run the samples: `valid` and `manual-valid` samples must decode, and the
others must be refused. The directory says what the Haskell node does, except for a constraint verified by a parent,
where it says what a decoder for the rule must do. Only the verification of the Haskell node reads the divergences.

#### Manual samples

An edge case might be difficult to generate, either because the Huddle generator is not set up to target that case or it
might take a great number of samples to generate it.

In this situation we can add individual samples in the `manual-valid` and `manual-invalid` directories for a given rule.
For example `conway/voting_procedures/manual-invalid/empty.input.cbor` contains an example of
`voting_procedures` with an empty map.

#### Coverage reporting

`generate` measures the coverage of each corpus and writes it to `<era>/coverage.json`, inside the corpus.
For each rule, it derives "obligations" (see [COVERAGE.md](COVERAGE.md)) then checks if there is at least one sample
validating the
obligation.

The `report` command creates an HTML page from the `coverage.json` file of every corpus under `DATASET_DIR`.
It does not measure anything, so it is fast:
```sh
cbor report /output --page /output/reports/coverage.html
```

The default page is `dataset/reports/coverage.html`.

#### Coverage regressions

A regression is coverage lost: some existing obligations aren't fulfilled any more.
If we derive a new obligation that isn't fulfilled yet by any sample, this is not a regression.

When `generate` replaces a corpus, it compares the new measurement with the `coverage.json` of the previous corpus.
It compares the obligations one by one, and identifies each obligation by its path and its demand. If the text of a
path or a demand changes, the obligation shows as lost.
If coverage is lost, `generate` lists the losses and keeps the previous corpus.
If the loss is expected (because the generation of obligations changed for example), run `generate` again with
`--accept-coverage-loss`.

#### Refreshing the dataset

`just refresh-dataset` runs `generate` for each era in `dataset/`, rewrites `dataset/reports/<era>/haskell-latest.md`,
then runs `report`. Use the `ERAS` variable to refresh only some eras, for example `ERAS=conway just refresh-dataset`.

### Verification

The Haskell node serves as the reference implementation for decoding on-chain data. Any divergence with what the Haskell node
decodes would trigger a fork in the network. Even if we use the Haskell decoders to classify the generated data, we still
need to verify that the Haskell code passes the whole corpus, because:

 - There are manual cases in the corpus.
 - The Haskell node code might have evolved.
 - Some rules ask for an exact re-encoding of the data (`transaction_body` for example).
 - This is additional evidence that the Haskell node conforms to the dataset with no exceptions (or acknowledged ones in the form of divergences).

`verify` checks every sample of a corpus with the Haskell node decoders:

```
cbor verify dijkstra /output/dijkstra
```

 - A sample in `valid` or `manual-valid` must decode, and its re-encoding must match its `.expected.cbor` file:
   - For most rules, the re-encoding is normalized before the comparison (see
     [VERIFICATION.md](VERIFICATION.md#normalization)).
   - For the rules that the Haskell node re-encodes byte for byte (`transaction_body` for example), the re-encoding must
     be the input bytes themselves, and the `.expected.cbor` file holds these bytes.
 - Any other sample must be rejected.
 - The `divergences` of `corpus.json` record the rare cases where the HuddleSpec and the Haskell node disagree (see
   [Divergences](#divergences)).

`generate` runs the same checks on a new corpus before it replaces the previous one.

The command prints each sample that fails as a `FAIL <path> (<reason>)` line on standard error, then a summary.
In the container, standard error goes to `failures.log`, in the run directory under `/output/coverage`
(or `CBOR_COVERAGE_DIR`).

When the corpus passes, the command writes the result of each sample to a JSON file, in the format given in
[Report output format](#report-output-format):

 - In the container, the results go to `reports/<corpus>/haskell-latest.json`, beside the corpus, with a Markdown
   summary in `haskell-latest.md` (see [For the Haskell node](#for-the-haskell-node)).
 - When you run the `cbor` binary directly, you must give the path of the JSON file as the last argument:
   `cbor verify dijkstra dataset/dijkstra results.json`.

#### Required environment variables

The report names the two commits that produced it. `verify` and `generate` read them from the environment and stop with
an error if one is missing or is not a full hash of 40 hexadecimal digits.

| Variable                     | Content                                                           |
| ---------------------------- | ----------------------------------------------------------------- |
| `CBOR_DATASET_COMMIT`        | The commit of this repository that holds the samples.             |
| `CBOR_IMPLEMENTATION_COMMIT` | The commit of the Cardano Ledger code that decodes the samples.   |

How to set them depends on how you run the command:

 - **In the container**, the image sets `CBOR_IMPLEMENTATION_COMMIT` already. It is the Cardano Ledger revision that the
   `Dockerfile` pins with `ARG CBOR_IMPLEMENTATION_COMMIT`, and the image has a label of the same name, which
   `docker inspect` shows. You must set `CBOR_DATASET_COMMIT`:
   ```sh
   docker run --rm -e CBOR_DATASET_COMMIT=$(git rev-parse HEAD) -v "$PWD/dataset:/output" cbor verify conway /output/conway
   ```
 - **With `just refresh-dataset`**, you set nothing. The script reads `ARG CBOR_IMPLEMENTATION_COMMIT` in the
   `Dockerfile` and runs `git rev-parse HEAD` for the dataset.
 - **With the `cbor` binary directly**, export both variables in your shell:
   ```sh
   export CBOR_DATASET_COMMIT=$(git rev-parse HEAD)
   export CBOR_IMPLEMENTATION_COMMIT=$(git -C .cache/cardano-ledger rev-parse HEAD)
   ```

## Cardano CBOR dataset for users

Users of this repository are expected to:

 1. Download the corpus for which they want to check the conformance.
 2. Implement their tests using the corpus as an input.
 3. Each test must check, for every `*.input.cbor` file found by one recursive scan of the corpus, that:
    1. Every case whose directory is named `valid` or `manual-valid` can be decoded.
    2. Every other case is rejected, at every severity including `invalid-zap-0`.
    3. Re-encoded data match the bytes of the `.expected.cbor` file beside the sample (see
       [VERIFICATION.md](VERIFICATION.md) for more details about checking re-encoded data).
 4. Output the results of the tests as a JSON file (see [Report output format](#report-output-format)) to a stable URL
    (for example as a GitHub artifact).

### Report output format

Node implementors must produce a JSON report with the following format. The file
[schema/report.schema.json](schema/report.schema.json) is a JSON Schema for it.

```json
{
  "version": "2",
  "era": "conway",
  "generated_at": 1793210818483,
  "protocol_version": "11.0",
  "dataset_commit": "f7b3b4e7f8cbf9b585f82153d60b3f6065a5b513",
  "implementation_commit": "6176e413b2be9880c23cedbdaa899f9752693b22",
  "results": {
    "auxiliary_data/valid/00003-2f2bacfd41c291d4.input.cbor": true,
    "auxiliary_data/invalid-zap-1/00001-example.input.cbor": "decode: succeeded when expected to fail"
  }
}
```

All fields are required:

| Field                   | Content                                                                               |
| ----------------------- | ------------------------------------------------------------------------------------- |
| `version`               | The version of this format: the string `"2"`. A change of the format changes it.      |
| `era`                   | The era of the dataset that was tested, in lower case: `conway` or `dijkstra`.        |
| `generated_at`          | When the report was made, in milliseconds since the Unix epoch.                       |
| `protocol_version`      | The protocol version that the node used to decode, as `<major>.<minor>`.              |
| `dataset_commit`        | The full hash of the commit of this repository that holds the samples.                |
| `implementation_commit` | The full hash of the commit of the node code that ran the samples.                    |
| `results`               | The outcome of every sample.                                                          |

The commits tell a reader which dataset and which node produced the report. Both are lowercase hexadecimal hashes of
40 digits: a branch name or a short hash is not enough.

See [Required environment variables](#required-environment-variables) for how to find the two commits.

`results` is a single object keyed by corpus-relative sample paths, using forward slashes and including the
`.input.cbor` suffix. Every sample is either `true` when it meets its expectation or an error message string (see
[VERIFICATION.md](VERIFICATION.md) for a more precise discussion on expectations).

Error strings use the format `<category>: <message>`, with the following categories:

 - `decode` for decoding or input-read failures.
 - `encode` for normalization, re-encoding comparisons or reference-read failures.

Version 1 was the `results` object alone, with no other field. `scripts/make-report.py` does not read it.

#### Markdown report

You can generate a Markdown report for the verification with:
```sh
python3 scripts/make-report.py dataset/conway dataset/reports/conway/haskell-latest.json > dataset/reports/conway/haskell-latest.md
```

This report shows valid/invalid success percentages and counts per rule, plus totals and the number of failed
or missing results. Missing results count against the success rate; result paths absent from the corpus are rejected.

#### For the Haskell node

Since the Haskell node is a reference implementation for the CBOR datasets, we keep a copy of its reports in this repository.

A completed container `verify` run writes two files beside the corpus, under `reports/<corpus>/`:
- `haskell-latest.json`: the report, with the commits that made it and the result of every sample.
- `haskell-latest.md`: a Markdown summary by `scripts/make-report.py`, with the same commits.

The `turbocardano-latest.json` files beside them are reports from another implementation, TurboCardano. They were made
on 2026-10-03 against an older corpus, so many of their paths are no longer in the dataset, and `make-report.py`
refuses them.
