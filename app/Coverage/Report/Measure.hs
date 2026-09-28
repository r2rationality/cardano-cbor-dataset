module Coverage.Report.Measure where

import Base.Paths
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Divergence
import Coverage.Obligations.Obligation
import Coverage.Obligations.SpecRule (HuddleSpec, requireRule)
import Coverage.Report.Samples
import Coverage.Report.SpecCoverage
import Coverage.Report.TermChecker
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable
import Generation.DatasetConfig
import Generation.SampleType
import Path

-- | Measure every rule directory in a dataset against the specification of its era.
measureSpecCoverage :: HuddleSpec -> Path Abs Dir -> IO SpecCoverage
measureSpecCoverage huddleSpec datasetDir = do
  requireRealDirectory "corpus directory" datasetDir
  ruleDirs <- listDirectoryNames datasetDir <&> map Name
  config <- readCorpusConfig datasetDir
  let divergences = configDivergences config
  reports <- for ruleDirs $ measureRuleCoverage (newTermChecker huddleSpec) datasetDir divergences
  pure
    SpecCoverage
      { rules = reports,
        declaredDivergences = divergences,
        unmatchedDivergences = findUnmatchedDivergences reports divergences,
        divergenceMismatches = findDivergenceMismatches reports divergences
      }

-- | Measure the coverage of a single rule, from the rule samples and the specification
measureRuleCoverage :: TermChecker -> Path Abs Dir -> [Divergence] -> Name -> IO RuleCoverage
measureRuleCoverage checker datasetDir allDivergences ruleName = do
  _ <- requireRule (checkerSpecification checker) ruleName

  let obligations = obligationsOf checker ruleName
  let divergences = filter (waivesRule ruleName) allDivergences
  samples <- loadSamples datasetDir ruleName
  pure $ assessCoverage checker ruleName obligations divergences samples

-- | Assess the coverage of a single rule, given its obligations, divergences, and samples.
--
-- A manual sample that the specification contradicts witnesses nothing: a manual-valid sample that the specification
-- refuses proves no obligation to accept, whatever the node does with it.
assessCoverage :: TermChecker -> Name -> [Obligation] -> [Divergence] -> [Sample] -> RuleCoverage
assessCoverage checker rule obligations divergences samples =
  RuleCoverage
    { ruleCoverageName = rule,
      ruleCoverageSamples = map sampleName samples,
      ruleCoverageObligations = classify checker obligations divergences witnesses <$> obligations,
      ruleCoverageContradicted = map sampleName contradicted
    }
  where
    contradicted = filter contradicts samples
    witnesses = filter (\sample -> not (sampleType sample == ManualValid && contradicts sample)) samples
    contradicts sample = case sampleType sample of
      ManualInvalid -> meetsAll sample
      ManualValid -> not (meetsAll sample)
      _ -> False
    -- The derivation states what the specification refuses, validators included, so a sample that meets no obligation
    -- to reject is one that the specification accepts.
    meetsAll sample = meetsObligations checker rule (sampleTerm sample)

-- | Classify an obligation as fulfilled, outstanding, waived, or untested, given the other obligations of its rule,
-- the samples and the waivers.
classify :: TermChecker -> [Obligation] -> [Divergence] -> [Sample] -> Obligation -> ObligationCoverage
classify checker obligations divergences allSamples obligation =
  ObligationCoverage obligation $ case waived obligation divergences of
    Just (kind, why) -> WaivedBy kind why
    Nothing -> Sampled (foundIn answers)
  where
    answers =
      [ (sampleKey sample, answerFor checker obligations obligation (sampleTerm sample))
      | sample <- candidates allSamples obligation
      ]

-- | The state that the answers of the candidates reach. The lists are lazy, so the scan stops at the first sample that
-- meets the obligation.
foundIn :: [(SampleKey, Maybe Bool)] -> Found
foundIn answers = case ([name | (name, Just True) <- answers], [() | (_, Just False) <- answers]) of
  (name : _, _) -> Met name
  ([], _ : _) -> Unmet
  ([], [])
    | null answers -> NoCandidate
    | otherwise -> Undecided

candidates :: [Sample] -> Obligation -> [Sample]
candidates samples obligation = case sampleCategoryFor obligation of
  Accept -> accepted samples
  Reject -> rejected samples

accepted :: [Sample] -> [Sample]
accepted = filter (isValid . sampleType)

rejected :: [Sample] -> [Sample]
rejected = filter (isInvalid . sampleType)

-- | The divergences that waive an obligation that their rule does not state. A rule with no directory in the corpus
-- was not measured and has no obligations here, so a divergence for @certificate@, where @certificates@ was meant, is
-- reported and not ignored.
findUnmatchedDivergences :: [RuleCoverage] -> [Divergence] -> [Divergence]
findUnmatchedDivergences reports divergences =
  [ divergence
  | divergence <- divergences,
    Just named <- [waivedObligation (divergenceKind divergence)],
    not (any (matches divergence) (Map.findWithDefault [] (waivedRule named) obligationsByRule))
  ]
  where
    obligationsByRule =
      Map.fromListWith
        (<>)
        [(ruleCoverageName report, map coverageObligation (ruleCoverageObligations report)) | report <- reports]

-- | Compare the divergences that a corpus declares with the manual samples that the specification contradicts. Each
-- such sample needs a divergence that gives the reason, and each divergence must name samples that the specification
-- judges as its kind says.
findDivergenceMismatches :: [RuleCoverage] -> [Divergence] -> [DivergenceMismatch]
findDivergenceMismatches reports divergences =
  map UndeclaredDivergence (toList (contradicted `Set.difference` divergentSamples divergences))
    <> [ MisdeclaredDivergence sample (divergenceKind divergence)
       | divergence <- divergences,
         sample <- toList (divergenceSamples divergence),
         specificationAccepts (divergenceKind divergence) /= accepts sample
       ]
  where
    contradicted =
      Set.fromList
        [SampleKey (ruleCoverageName report) name | report <- reports, name <- ruleCoverageContradicted report]
    -- What the directory says, unless the specification contradicts it.
    accepts sample = (sampleNameType (sampleKeyName sample) == ManualValid) /= (sample `Set.member` contradicted)
