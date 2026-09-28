module Generation.GenerationChecks where

import Base.Extra (throwEitherText)
import Base.Paths
import Coverage.Report.CoverageData
import Coverage.Report.Measure (measureSpecCoverage)
import Coverage.Report.Regressions
import Generation.EraSpec
import Path

data GenerationChecks = GenerationChecks
  { -- | Whether to perform verification checks.
    checkVerification :: Bool,
    -- | Whether to check coverage.
    checkCoverage :: Bool,
    -- | Whether to accept coverage loss.
    acceptCoverageLoss :: Bool
  }
  deriving (Eq, Show)

-- | Measure the coverage of a new corpus and keep the measurement inside it. Stop when the new corpus covers less than
-- the corpus it replaces, unless the loss is accepted.
checkCoverageOf :: EraSpec -> GenerationChecks -> Path Abs Dir -> Path Abs Dir -> IO ()
checkCoverageOf era checks previous staging = do
  huddleSpec <- throwEitherText (eraSpecRoot era)
  measured <- measureSpecCoverage huddleSpec staging
  let coverage = summarizeCoverage (eraSpecEra era) huddleSpec measured
  writeCorpusCoverage staging coverage
  recorded <- readCorpusCoverage previous
  case maybe [] (`regressionsAgainst` coverage) recorded of
    [] -> putTextLn "Coverage holds against the previous corpus"
    losses -> do
      warn "coverage lost against the previous corpus:"
      for_ losses (warn . ("  " <>) . renderRegression)
      unless (acceptCoverageLoss checks) $
        abort "run with --accept-coverage-loss once the loss is understood and intended; the previous corpus is unchanged"
