# Coverage

We can leverage two notions of "coverage" to assess the quality of our CBOR conformance dataset:

 1. Does the generated data "cover" all the rules? Is there at least one sample for each relevant part of each rule?

 2. Does the executed code "cover" all the rules? Is there some decoding code that is executed that:
    1. Should be reflected in the specification.
    2. Should not be executed even if there is no sample making that code fail.

## Specification coverage

The specification we take as a starting point for the dataset generation is the [`HuddleSpec`](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/cddl/lib/Cardano/Ledger/Conway/HuddleSpec.hs)
for a given era.

The `HuddleSpec` provides a list of rules representing the structure and the constraint for each data type that can be
decoded on-chain. From each rule we derive a list of **obligations**: concrete demands that the corpus should fulfill.

Every obligation falls in one of two categories:

 - **Accept**: a sample of this shape must decode. Answered by a sample under `valid` or `manual-valid`.
 - **Reject**: a sample of this shape must be refused. Answered by a sample under an `invalid-*` directory.

An obligation is the smallest thing a corpus can be right or wrong about. "This map must refuse a duplicate key" is one.
"This array may hold three entries" is another. The corpus fulfills an obligation when at least one sample shows what it
asks for, and the decoder gave the answer that the specification requires. The directory of a sample records that
answer: the Haskell node decoder classified the sample when it was generated.

### Where obligations come from

Each obligation records four things:

 - The **path** where it is stated, such as `map{15}.occur0.value`.
 - The **subject**, the path of the value that a sample is checked on. It is the same as the path, except for a required
   map key: that obligation is stated at the key, and checked on the map.
 - The **kind**, which is what it demands (`AboveRange`, `AcceptCbor`, and more).
 - The **origin**, when the demand was inlined from another rule that has no corpus of its own.

### Paths

A path is the list of steps from the top of a rule down to where an obligation is stated, joined by dots. The path at
the top of the rule is `.`.

| Step                          | Meaning                                                                                      |
| ----------------------------- | -------------------------------------------------------------------------------------------- |
| `choice[1:map]`               | The alternative at index 1 of a choice, with the shape that tells it apart from the others   |
| `array[2]`, `group[2]`        | The element at position 2 of an array or of a group                                          |
| `array[4/11]`                 | The element at position 4 of an array of 11 elements, after an optional element.             |
| `map{14}`                     | The map entry with the literal key 14                                                        |
| `map[0]`                      | The map entry at position 0, when its key is not a literal                                   |
| `map[3?0,1,2]`                | An entry with an open key, found because its key is none of the keys its siblings name       |
| `key`, `value`                | The key or the value of a map entry                                                          |
| `occur0`                      | An entry that appears at most once (`?`), with its lower bound                               |
| `repeat0`                     | Every occurrence of an entry that can appear more than once (`*`, `+`), with its lower bound |
| `tag258`                      | The value under tag 258                                                                      |
| `control`, `gen`, `validator` | The target of a control, a custom generator or a custom validator                            |

The shape in a choice step is one of:

- `array`
- `array(3)` (an array that starts with 3)
- `map`
- `tag258`
- a primitive such as `uint` or `nil` (`prim(array)` and `prim(map)` for the containers)
- `=0` (one value)
- `0..255` (a range)
- `(uint/nil)` (a choice, where `tag121..127` is a run of consecutive tags)
- `any`
- `ambiguous-array` (the shape of an alternative that a sibling shares, so a term of that shape reaches both)

Literals are written as in CDDL: `14`, `-1`, `"text"`, `h'0a1b'`, `true`, `1.5`, and `12n` for a bignum.

### How each rule states its obligations

Each row below specifies one possible aspect of a rule. The sections are in the same order as in
[`ObligationsSpec`](test/unit/Coverage/Obligations/ObligationsSpec.hs), which has one `describe` block for each section:

| Section        | Construct                              | Obligations stated                                                                                                                                                                                                                                             |
| -------------- | -------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1. Literals    | Literal                                | `AcceptLiteral v`, `WrongLiteral v`                                                                                                                                                                                                                            |
|                | Primitive (`uint`, `bytes`, `text`, …) | `AcceptType p`, `WrongType p`, and `AcceptBoundary` at each end of the type's range when nothing else bounds it                                                                                                                                                |
| 2. Ranges      | Range `a .. b`                         | `AcceptBoundary a`, `AcceptBoundary b`, `InsideRange`, `BelowRange`, `AboveRange`, `WrongType int`                                                                                                                                                             |
| 3. Tags        | Tag `#6.n(x)`                          | `AcceptTag n`, `WrongTag n`, `MissingTag n` (except for bignums), and the payload walked, or only its length when CBOR fixes its type (bignums)                                                                                                                |
| 4. Choices     | Choice `a / b`                         | `NoBranchMatches`, `NoTagMatches`, `NoDiscriminantMatches`, `NoReferenceMatches`, per branch `AcceptBranch` plus that branch's own obligations, and for number alternatives `BelowRange`, `AboveRange`, `InsideGap` for the numbers that no alternative admits |
|                | Overlapping array alternatives         | `AcceptElements`, `NoElementsMatch`, `WrongCombination`                                                                                                                                                                                                        |
| 5. Arrays      | Array `[...]`                          | `WrongType array`, `AcceptDefinite`, `AcceptIndefinite`, plus the length rows below                                                                                                                                                                            |
|                | Container length                       | one `AcceptArity n` per permitted length, `TooFew low`, `TooMany high`, for arrays and maps                                                                                                                                                                    |
|                | Open entry (`*`, `+`)                  | one `AcceptArity`, at one more than the width with each open entry taken once, for arrays and maps                                                                                                                                                             |
|                | Optional entry (`?`)                   | `AcceptPresent`, or `AcceptReference r` when the entry names a rule, for arrays and maps                                                                                                                                                                       |
| 6. Maps        | Map `{...}`                            | `WrongType map`, `AcceptDefinite`, `AcceptIndefinite`, `DuplicateKey`, the length rows of arrays, and `NoKeyMatches` when every entry has a literal key                                                                                                        |
|                | Required map entry                     | `AcceptRequiredKey k`, `MissingRequiredKey k`                                                                                                                                                                                                                  |
| 7. Groups      | Group `(...)`                          | nothing of its own: its entries are spliced into the container that holds it, see [References to other rules](#references-to-other-rules)                                                                                                                      |
| 8. References  | Reference to a rule                    | `AcceptReference r`, `WrongReference r`, unless the rule is folded or inlined, see [References to other rules](#references-to-other-rules)                                                                                                                     |
| 9. Controls    | `.size` on `bytes` or `text`           | `AcceptSize low high`, `ViolateSizeBelow`, `ViolateSizeAbove`                                                                                                                                                                                                  |
|                | `.size` on `uint`, `nint`, `int`       | `AcceptBoundary (256^n - 1)`, `ViolateHighBound`                                                                                                                                                                                                               |
|                | `.cbor`                                | `AcceptCbor`, `ViolateCbor`, and `WrongEmbedded r` when the control names a rule `r`                                                                                                                                                                           |
|                | `.le`                                  | `AcceptBoundary high`, `ViolateHighBound high`                                                                                                                                                                                                                 |
|                | Any other control                      | an error: the derivation stops and names the operator                                                                                                                                                                                                          |
| 10. Validators | Custom validator                       | `ValidatorSucceeds`, `ValidatorFails`                                                                                                                                                                                                                          |

#### What each obligation means

An **Accept** obligation is answered by a sample that must decode, a **Reject** obligation by one that must be refused.

| Obligation                 | Category | Meaning                                                                                                 | Example rule               |
| -------------------------- | -------- | ------------------------------------------------------------------------------------------------------- | -------------------------- |
| `AboveRange hi`            | Reject   | A value above the upper end                                                                             | `vote`                     |
| `AcceptArity n`            | Accept   | A length the container may have                                                                         | `auxiliary_data_map`       |
| `AcceptBoundary n`         | Accept   | A value set exactly on bound `n`                                                                        | `vote`                     |
| `AcceptBranch`             | Accept   | One sample takes this alternative                                                                       | `auxiliary_data`           |
| `AcceptCbor`               | Accept   | A byte string whose contents decode as CBOR                                                             | `script_ref`               |
| `AcceptDefinite`           | Accept   | The container in definite form                                                                          | `vkeywitness`              |
| `AcceptElements shapes`    | Accept   | An array whose elements have these shapes (this is used when alternatives overlap)                      | `account_balance_interval` |
| `AcceptIndefinite`         | Accept   | The same container in indefinite form                                                                   | `vkeywitness`              |
| `AcceptLiteral v`          | Accept   | A literal named by the specification                                                                    | `script_pubkey`            |
| `AcceptPresent`            | Accept   | A sample in which an optional entry is present                                                          | `transaction_witness_set`  |
| `AcceptReference r`        | Accept   | A valid value for a given field.                                                                        | `transaction_body`         |
| `AcceptRequiredKey k`      | Accept   | A map carrying the entry keyed `k`                                                                      | `transaction_body`         |
| `AcceptSize lo hi`         | Accept   | A value whose length a `.size` control admits                                                           | `url`                      |
| `AcceptTag n`              | Accept   | The tag specified by the rule                                                                           | `big_uint`                 |
| `AcceptType p`             | Accept   | A value of the primitive type the specification names                                                   | `big_int`                  |
| `BelowRange lo`            | Reject   | A value below the lower end                                                                             | `vote`                     |
| `DuplicateKey`             | Reject   | A map carrying the same key twice                                                                       | `withdrawals`              |
| `InsideGap lo hi`          | Reject   | A value strictly between two alternatives that are numbers, which no alternative admits                 | `nonzero_int64`            |
| `InsideRange lo hi`        | Accept   | A value strictly between the ends of a range                                                            | `redeemer`                 |
| `MissingRequiredKey k`     | Reject   | The same map with that entry left out                                                                   | `transaction_body`         |
| `MissingTag n`             | Reject   | A value without the tag `n`                                                                             | `unit_interval`            |
| `NoBranchMatches shapes`   | Reject   | A value matching none of the alternatives                                                               | `drep`                     |
| `NoDiscriminantMatches ns` | Reject   | An array opening with a value that discriminates none of the alternatives                               | `drep`                     |
| `NoElementsMatch combos`   | Reject   | An array of the right width matching no combination any alternative admits                              | `account_balance_interval` |
| `NoKeyMatches ks`          | Reject   | A map carrying none of those keys                                                                       | `transaction_body`         |
| `NoReferenceMatches s rs`  | Reject   | A value of shape `s`, which alternatives naming the rules `rs` share, that is valid for none of them    | `guards`                   |
| `NoTagMatches tags`        | Reject   | A tagged value carrying none of the tags the alternatives name: right shape, wrong number               | `big_int`                  |
| `TooFew n`                 | Reject   | Fewer entries than the lower bound allows                                                               | `vkeywitness`              |
| `TooMany n`                | Reject   | More entries than the upper bound allows                                                                | `vkeywitness`              |
| `ValidatorFails`           | Reject   | A sample refused by the rule validator                                                                  | `transaction_body`         |
| `ValidatorSucceeds`        | Accept   | A sample accepted by the rule validator                                                                 | `transaction_body`         |
| `ViolateCbor`              | Reject   | A byte string whose contents do not decode as valid CBOR                                                | `script_ref`               |
| `ViolateHighBound n`       | Reject   | A value over the ceiling a control sets (for example `.le`)                                             | `port`                     |
| `ViolateSizeAbove hi`      | Reject   | A value longer than the control allows                                                                  | `url`                      |
| `ViolateSizeBelow lo`      | Reject   | A value shorter than the control allows                                                                 | `vrf_vkey`                 |
| `WrongCombination shapes`  | Reject   | An array each of whose elements is admitted at its own position, in a combination no alternative admits | `account_balance_interval` |
| `WrongEmbedded r`          | Reject   | A byte string whose contents decode as CBOR, but are not a valid `r`                                    | `script_ref`               |
| `WrongLiteral v`           | Reject   | Any other value than `v`                                                                                | `script_pubkey`            |
| `WrongReference r`         | Reject   | A value at that site that is not valid for the rule named there                                         | `vkeywitness`              |
| `WrongTag n`               | Reject   | Any other tag than `n`                                                                                  | `big_uint`                 |
| `WrongType t`              | Reject   | A value of another type than `t`                                                                        | `vkeywitness`              |


 - Each example names a rule that is tested in [`test/unit/Coverage/Obligations/ObligationsSpec.hs`](test/unit/Coverage/Obligations/ObligationsSpec.hs) where you can read, in context, how it is used.
 - `account_balance_interval` is a Dijkstra rule, the rest are Conway.

#### What is deliberately **not** derived from the specification

Some obligations are *not* generated:

 - There is no `AcceptArity` obligation for a **fixed-length** array. Any valid sample with `AcceptDefinite` or `AcceptIndefinite` already
   records that the length was reached. E.g. `"transaction_input"`, a fixed array of two.

 - A **bound** states its endpoints, never "some value within it". Any valid sample already carries a value inside the
   bound, so that obligation would be met for free and could never fail. The endpoints can fail: a decoder that reads a
   `uint` into a signed 64 bit integer can handle small values and breaks near the top of the range.
   E.g. `"port"`, which states `65535` and nothing below it.

 - For the same reason `InsideRange` is stated only where the range has an interior, since otherwise the endpoints already answer it.
   E.g. `"redeemer"`, whose `redeemer_tag` span of `0 .. 5` has one.

 - A **range admits integers only**, so it also states `WrongType int`: a value such as `null` is refused wherever the
   range is read. E.g. `"cost_models"`, whose costs are `int64`. A range that is an alternative of a choice does not
   state it, because the choice picks that alternative for an integer only. `NoBranchMatches` at the choice covers it.

 - **`WrongReference` has no accepted counterpart.** A specification-valid sample has valid sub-terms everywhere already.
   E.g. `"script_pubkey"`, which denies its `addr_keyhash` and never demands a good one.

 - A branch that **cannot be told from its sibling** states nothing of its own.
   E.g. `"account_balance_interval"`, where `[coin, exclusive_upper_bound / nil]` and
   `[inclusive_lower_bound / nil, coin]` are both "an array of two". `[5, 5]` is valid under both branches
   so we only generate obligations for the first case.

 - **Specific refusals are stated separately from catch-alls.** `NoTagMatches` means "right shape and wrong tag number".
   This catches a decoder that never reads the tag. `NoBranchMatches` is different: any value at all fulfills that one.
   E.g. `"big_int"`, which names tags 2 and 3.
   In the same way, a choice whose alternatives are numbers states the numbers that no alternative admits:
   `BelowRange`, `AboveRange` and `InsideGap`. E.g. `"nonzero_int64"`, which refuses 0.

#### References to other rules

Rules are referenced by other rules in the specification. We use 3 different strategies to generate obligations for references:
splicing, folding and inlining.

| Question                                                     | Splicing              | Folding                 | Inlining              |
| ------------------------------------------------------------ | --------------------- | ----------------------- | --------------------- |
| Which rules does it apply to?                                | Groups                | `foldedRules`           | `inlinedRules`        |
| Who decides?                                                 | The CDDL              | We do                   | We do                 |
| Does the rule keep a row of its own?                         | No                    | No                      | Yes                   |
| Does the reference get `AcceptReference` / `WrongReference`? | No                    | No                      | Yes                   |
| Where does the reference get the rule's obligations from?    | Each entry, flattened | The rule body, moved    | The rule body, copied |
| What changes?                                                | The array shape       | The obligation location | Nothing is removed    |

Examples:

 - **Splicing**: `pool_params` has 9 entries. It is spliced into `pool_registration_cert`, so that array has 10 positions.
 - **Folding**: `%nonempty_list<vkeywitness>` has no dataset entry of its own. The `transaction_witness_set` field just states
   the non-empty obligation.
 - **Inlining**: `script_ref` has its own dataset entry. Each reference to `script_ref` also gets a copy of its obligations.

The lists of folded and inlined rules are in
[`app/Coverage/Obligations/Obligations.hs`](app/Coverage/Obligations/Obligations.hs).

#### Examples

The `ObligationsSpec` specification contains a lot more examples but here are two examples showing how obligations are
created from rules.

`port = uint .le 65535` states four obligations, two accept and two reject:

```
control  AcceptBoundary 65535     the largest permitted value must decode
control  ViolateHighBound 65535   one above it must be refused
control  AcceptType uint          a uint must decode
control  WrongType uint           a value of another type must be refused
```

The path is `control`, because the demand is stated at the `.le` control rather than at the rule's root. Note what the
range does **not** state: no obligation asks for an ordinary value such as 42, since any accepted sample carries one.
Only the end of the range catches a decoder that reads the value into a type too narrow to hold it.

`script_ref = #6.24(bytes .cbor script)` shows nesting:

```
.              AcceptTag 24 / WrongTag 24          the tag number is checked
.              MissingTag 24                       a value without the tag must be refused
tag24.control  AcceptCbor / ViolateCbor            the embedded bytes must decode as CBOR
tag24.control  WrongEmbedded script                the decoded value must be a valid script
tag24.control  AcceptType bytes / WrongType bytes  the payload must be a byte string
```

### How coverage is checked

Each obligation is evaluated against the corpus samples for the obligation rule. The evaluation raises four verdicts:

| Verdict         | Meaning                                                                                                  |
| --------------- | -------------------------------------------------------------------------------------------------------- |
| **Fulfilled**   | A sample of the right category shows what the obligation asks for. This sample is the witness            |
| **Outstanding** | No sample fulfills the obligation. We either need to generate more samples or add manual ones            |
| **Waived**      | A divergence in `corpus.json` lifts the obligation, because the Haskell node and the specification disagree on it |
| **Untested**    | The obligation could not even be properly tested.                                                        |

An **Untested** verdict carries why the obligation could not be tested:

 - `no samples`: no sample of the category that the obligation is about (accepted or rejected).
   More samples of that category settle it.
 - `not reached`: samples of that category exist, but none of them reaches the obligation's path.
   Usually the path goes through an optional entry, and a sample that holds this entry settles it.
   Sometimes no sample can reach the path, for example an alternative that cannot be told from another one.

A **Waived** verdict carries several kinds:

 - `verified-by-parent`: the constraint is enforced by another rule referencing that one. The
   obligation still holds for every implementation, but the Haskell node chooses to run that check somewhere else.
   E.g. `withdrawals` must be non-empty but the Haskell node checks that in the `transaction_body` decoder.

 - `incorrect-specification`: the specification asks for something the ledger does not enforce anywhere. For example `metadatum`
   is specified as a map with unique keys in the `HuddleSpec` but the Haskell node models `metadatum` as a list of pairs
   where keys can be repeated.

 - `unstated-constraint`: the Haskell node refuses a form that the specification allows, so the obligation to accept
   it can never be fulfilled. E.g. the records that the ledger decodes with `RecD`, such as `anchor`, refuse an
   indefinite array.

A divergence names its obligation by its path and its kind, for example `choice[1:map]` and `{"tag": "DuplicateKey"}`
(see the README). A divergence that matches no obligation of its rule stops the run: either the specification changed
and the divergence is out of date, or the divergence was never right.

### The baseline

`cbor generate` measures each corpus it produces and keeps the measurement in `<era>/coverage.json`, inside the corpus.
This measurement is the baseline the next generation of the corpus is compared against. `cbor report` only renders the
page from these files.

A regression is coverage **lost**, never coverage not yet gained. Deriving a new obligation raises what a rule owes
without lowering what it has met, so work on the derivation never trips the gate, while a sample that stops counting
always does. On a loss `generate` lists each one, keeps the previous corpus and exits non-zero. Re-run with
`--accept-coverage-loss` once the loss is understood and intended.

## Code coverage

The Docker image exposes the Haskell executable through a small entrypoint that automatically produces code-coverage
reports for verification commands.

Every `verify` invocation writes to `/output/coverage/<dataset-name>-<UTC-timestamp>`. For example, verifying `conway`
writes to `/output/coverage/conway-20260901T113859Z`.

Each run contains:

- `cbor.tix`: raw GHC HPC execution counts.
- `report.txt`: per-module expression, alternatives, and boolean conditions (guards, `if` conditions, and qualifiers).
- `report.xml`: the same coverage data in HPC's XML format.
- `haskell-results.json` and `haskell-report.md`: raw results and the rendered conformance report, when produced successfully.
- `html/hpc_index.html`: the main source-coverage report.
- `html/hpc_index_alt.html`: alternative coverage by module.
- `html/hpc_index_exp.html`: expression coverage by module.
- `html/hpc_index_fun.html`: declaration coverage by module.

Reports are generated even when verification finds failures, after which the container returns the verifier's exit
status. Set `CBOR_COVERAGE_DIR` to put reports elsewhere. The selected directory must be writable and outside the
dataset being verified. Concurrent runs receive separate directories.

Every `verify` invocation also writes its error messages to `failures.log` inside that invocation's coverage directory.
The verifier statistics and the failure-log path are printed to standard output. Individual errors remain in
the log instead of overwhelming the console output.
