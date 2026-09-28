## Generation process

### Generated values

For each rule, the generator produces one batch of samples generated from the specification by randomly selecting values
specified by the rule. On top of that generated value, the generator selects a serialization format: definite or indefinite.
Then, if the Haskell decoder accepts the serialized value, the sample goes to the `valid` directory and if it rejects that
value, the sample goes to `invalid-zap-0` (meaning that no mutation was necessary to generate that invalid value).

A refused value gets a second chance: the generator writes it again with definite-length strings, then with
definite-length containers. If one of these forms decodes and is new, it also goes to `valid`.

### Mutated values

Then the generator produces one mutated batch per mutation severity. Samples are then tested against the rule decoder.
A mutation that is indeed invalid is written to the `invalid-zap-<n>` directory where `n` is the mutation severity.
A mutation that still decodes is dropped, and the final summary gives the number of dropped mutations. The generator
also drops any sample whose bytes it has already produced for the same rule and category.
By default, a batch receives an attempt budget of three times its requested size. If the budget cannot produce enough unique samples,
the final summary reports the shortfall.

### Expected values

The generator then writes one `.expected.cbor` file per `valid` and `manual-valid` sample, beside it and under the same
name. The file contains the value decoded with the Haskell decoder then re-encoded with the Haskell encoder:

 - For most rules, the re-encoding is normalized (see [VERIFICATION.md](VERIFICATION.md#normalization)).
 - For the byte-exact rules, such as `transaction_body`, the re-encoding must be the input bytes, so the file holds
   these bytes.

## Manual samples

Random data generation is not always able to generate all of the specification obligations (see
[COVERAGE.md](COVERAGE.md)).
In that case, we can specify manual cases that are marked as either `manual-valid` or `manual-invalid`.

## Output layout

The generated and manual files are written to different directories based on their validity

```
dataset/<era>/
  corpus.json
  coverage.json
  <rule>/
    valid/00001-<sha256-prefix>.input.cbor
    valid/00001-<sha256-prefix>.expected.cbor
    invalid-zap-0/00001-<sha256-prefix>.input.cbor
    invalid-zap-1/00001-<sha256-prefix>.input.cbor
    invalid-zap-2/00001-<sha256-prefix>.input.cbor
    invalid-zap-3/00001-<sha256-prefix>.input.cbor
    manual-valid/<name>.input.cbor
    manual-valid/<name>.expected.cbor
    manual-invalid/<name>.input.cbor
```

The `<sha256-prefix>` is the first 16 hexadecimal characters of the SHA-256 of the sample bytes. Manual samples keep the
name that their author gives them.

Severity zero is the one that is not a mutation: it holds the generator's own samples, unaltered, that the decoder
rejects. The higher severities hold mutations of increasing aggressiveness. Each directory numbers its files from
`00001` independently.

A severity directory can be missing. The generator creates all four, but leaves one empty for a rule whose mutations
the decoder never rejects, and version control does not track an empty directory. Verification treats an absent
severity as zero samples rather than failing.
