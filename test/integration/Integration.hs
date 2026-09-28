module Main (main) where

import Base.Extra
import Control.Concurrent.Async (forConcurrently)
import Coverage.Obligations.Divergence (divergentSamples)
import Coverage.Obligations.SpecRule (HuddleSpec, eraRoot, isCddlValid)
import Coverage.Report.Samples
import Coverage.Report.SpecCoverage (SpecCoverage (divergenceMismatches))
import Coverage.Report.TermChecker
import Data.Set qualified as Set
import Generation.DatasetConfig (configDivergences, readCorpusConfig)
import Generation.EraSpec (Era (..), renderEra)
import Generation.SampleType
import Path.IO (resolveDir')
import Test.Hspec

-- | This test makes sure that we have enough obligations to judge the CDDL subset of the specification
--   We check every sample against the obligations derived from the HuddleSpec (including the custom validators)
--   and against the CDDL schema.
--   If the CDDL rejects a sample that no obligation rejects, we know that there is a gap in the obligations.
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "We check the obligations coverage of a rule against the CDDL schema for that rule" $
  for_ [Conway, Dijkstra] $ \era ->
    it ("judge every sample of the " <> show era) $
      disagreements era >>= (`shouldBe` [])

-- | Return the list of samples for an era where the CDDL schema and the obligations disagree, with the reason.
disagreements :: Era -> IO [Text]
disagreements era = do
  specification <- throwEitherText (eraRoot (renderEra era))
  let checker = newTermChecker specification
  directory <- resolveDir' ("dataset/" <> toString (renderEra era))
  divergent <- divergentSamples . configDivergences <$> readCorpusConfig directory
  samples <- loadDatasetSamples directory
  -- We check samples concurrently because each sample is independent
  fmap catMaybes . forConcurrently samples $ \sample ->
    evaluateNF $ (\reason -> renderSampleKey (sampleKey sample) <> ": " <> reason) <$> checkSample checker specification divergent sample

-- | The reason why the CDDL schema and the obligations disagree on a sample, if they do.
checkSample :: TermChecker -> HuddleSpec -> Set SampleKey -> Sample -> Maybe Text
checkSample checker specification divergent sample
  | not isCddlOk && areObligationsMet = Just "cddl refuses it, but it breaks no obligation"
  | isCddlOk && not areObligationsMet && not explained =
      Just "it breaks an obligation, but cddl and the node accept it, and it is not divergent"
  | otherwise = Nothing
  where
    key = sampleKey sample
    rule = sampleKeyRule key
    term = sampleTerm sample
    isCddlOk = isCddlValid specification rule term
    areObligationsMet = meetsObligations checker rule term
    -- The obligations can be stricter than cddl (repeated keys for example), when the node also refuses the sample
    explained = isRejected sample || Set.member key divergent
