# Verification

The purpose of the CBOR conformance dataset is to give node implementors some assurance that they will be able to properly decode
on-chain data for various eras. In order to do this, we generate data for different part of the Cardano data model and
we expect each node implementation to succeed in decoding valid samples while rejecting invalid samples.

However guaranteeing that a node implementation fully conforms to the specification is non-trivial for several reasons:

 1. Code boundaries
 2. Data preservation
 3. Test execution

## Code boundaries

The verification that decoding a given piece of data is "correct" is somewhat arbitrary:

 - A node might decide to reject some values not at decoding time but during the execution of the ledger rules.
   This does not fundamentally change the behavior of the node.
 - A node might decide to decode a list of elements in a given decoder but check their uniqueness from inside another decoder at their point of use.

The stance taken by this project is to impose some restriction on node implementors. The `HuddleSpec` that is driving the
data generation is the closest thing that we have as a specification. The way its rules are named and specified impose
a similar structure to the decoders implemented in each node. If a node implementation does not naturally follow the exact same
structure it will have to adapt its test infrastructure to show that the rule obligations are still fulfilled.

**Hence, it is recommended that node implementations follow the dataset structure as closely as possible.**

Note that this is the case for the Haskell node itself (which is the de facto specification). This is why there are some
"waived" obligations mentioned in the specification coverage (see [COVERAGE.md](COVERAGE.md)).

## Data preservation

Showing that decoding succeeds on valid samples and fails on invalid samples is not enough to guarantee that the node decoding
behavior is correct:

 - A node might succeed in decoding data yet drop elements for a field that is supposed to contain a list of elements.
 - A node might succeed in decoding two fields of identical types but accidentally swap their values.

In order to check that no data is dropped or accidentally swapped we expect each node to re-encode the successfully decoded
data and compare the result to an expected reference value. This is unfortunately not easy. For example:

 - The serialization format for Cardano data is lenient. For example arrays can be serialized both as definite and indefinite arrays.
 - Rational numbers don't have to be represented in irreducible form.

In short, there is no canonical data representation for Cardano data.

**We address this concern by requiring node implementations to prove, via code, that the re-encoded data is isomorphic
to expected values (see [Normalization](#normalization))**.

## Test execution

This repository only defines input data, expected data and report format to be used by node implementations when running their decoders.

At no point we execute the production code of a node to get an independent proof that decoders are passing all the tests. It is then not excluded
that:

 - A node implementation fakes its full conformance by just producing a successful report.
 - A node implementation has a bug in its test implementation that hides an actual decoding bug. This is all the more
   possible if the code boundaries for the node decoders are slightly different than the ones specified by the dataset.

**This means that conformance must be understood as the fact that a node implementation has done its best to check its decoding behavior. It cannot be taken as a definitive proof that the node decoders are flawless.**

## Normalization

There can be two main sources of differences between the data re-encoded by a node and the expected data in the dataset:

 1. Serialization differences (definite vs indefinite for example).
 2. Original bytes preservation

### Serialization

#### Normalization specification for the serialization

Here is the specification of what needs to be normalized in order to remove serialization differences between re-encoded data
and expected data. Apply recursively to every nested item in the CBOR output:

| from                                                   | to                                                         |
| ------------------------------------------------------ | ---------------------------------------------------------- |
| indefinite-length array                                | definite-length, same elements in the same order           |
| indefinite-length map                                  | definite-length, same entries in the same order            |
| chunked byte string                                    | one definite byte string, chunks concatenated              |
| chunked text string                                    | one definite text string, chunks concatenated              |
| non-minimal integer head                               | the minimal head encoding the same value                   |
| bignum (#6.2/#6.3) whose magnitude fits a native head  | the equivalent uint/nint                                   |
| bignum that does not fit                               | kept, leading zero bytes dropped from the magnitude        |
| rational                                               | a reduced rational with minimal numerator and denominator  |

An example of the normalization algorithm in Haskell can be found in this repository [here](app/Generation/Normalize.hs).

#### Testing the serialization normalization

`dataset/normalization/` holds 20 small hand-verified pairs, one `.input.cbor` and one `.expected.cbor` per case. Your
normalizer is correct on this set when normalizing each `<n>.input.cbor` gives bytes identical to `<n>.expected.cbor`. Eight of
the cases must come out unchanged, which is what keeps a content bug failing rather than passing quietly.

[dataset/normalization/README.md](dataset/normalization/README.md) lists every pair as reviewable hex and says what each
case is for. That table is the
authority: the committed files were checked against it by hand rather than taken from an implementation.

You can check the reference implementation with:
```
scripts/check-normalization-vectors
```

### Original bytes preservation

#### What must be checked exactly

Some entities must be exactly re-encoded:

 - `transaction_body` because transactions are propagated across nodes and a transaction hash must match its serialized body exactly.
 - `header` because the block hash is computed over its bytes.
 - `redeemers` and `plutus_data` because their bytes are used to check the script integrity.
 - `native_script` and `auxiliary_data` because a script hash and the auxiliary data hash are computed over their bytes.
 - `transaction_witness_set` because the Haskell node keeps its original bytes and re-encodes them unchanged.

The list of these rules is in [`app/Generation/EraSpec.hs`](app/Generation/EraSpec.hs), where they use
`mkByteExactRule` or `mkByteExactAnnotatorRule`.

For those types we require each node implementation to compare their re-encoded bytes exactly to the expected values.

#### Memoization discrepancies

The Haskell node chooses to memoize the original bytes of some entities in order to make sure that any re-encoding
and hashing does not end up with a different hash. Another implementation might decide to do differently. For example:

 - A set can be written with or without the tag `#6.258`. Outside the byte-exact rules listed above, an implementation
   can choose to drop that information.
 - The exact arrival order of elements in metadata is irrelevant but is kept in expected data containing metadata.

As a result we require each node implementation to prove, via some code, that their re-encoded data is equal to the
expected data once we account for all the non-semantically meaningful differences.
