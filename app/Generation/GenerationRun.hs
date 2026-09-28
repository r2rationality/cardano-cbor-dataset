module Generation.GenerationRun where

import Data.Set qualified as Set
import Generation.DatasetConfig
import Generation.Rule as Rule
import Generation.SampleType (SampleType (..))

-- | Represents the state of a generation run for a specific rule and sample type.
data GenerationRun = GenerationRun
  { -- | Total number of samples to generate for this run.
    samplesNb :: Int,
    -- | Samples written for the current sample type.
    written :: Int,
    -- | Samples routed to severity zero because the decoder rejected them.
    rejected :: Int,
    -- | Mutations dropped for being decodable.
    dropped :: Int,
    -- | Current number of attempts made in this run
    attempts :: Int,
    -- | Current batch number within this run.
    batch :: Int,
    -- | Number of times the run has exhausted its batch without producing accepted samples.
    exhaustions :: Int,
    -- | The digests seen so far. A dropped mutation still joins them, so an identical one later in the run costs a
    -- lookup rather than another decode.
    seen :: Set.Set Text
  }
  deriving stock (Eq, Show, Generic)

-- | Create an initial generation run for the given dataset configuration and rule.
newGenerationRun :: DatasetConfig -> GenerationRule -> GenerationRun
newGenerationRun config rule =
  GenerationRun
    { samplesNb = numberOfSamplesFor config rule,
      written = 0,
      rejected = 0,
      dropped = 0,
      attempts = 0,
      batch = 0,
      exhaustions = 0,
      seen = Set.empty
    }

-- | Add a new digest to the set of seen digests.
addDigest :: Text -> GenerationRun -> GenerationRun
addDigest digest run = run {seen = Set.insert digest (seen run)}

-- | The number the next sample written to this sample type's directory (counting from one).
--
-- Severity zero is never generated directly: it only receives the @valid@ samples the decoder rejects, so it has its
-- own count.
getPositionFor :: GenerationRun -> SampleType -> Int
getPositionFor run = \case
  Zap 0 -> rejected run + 1
  _ -> written run + 1

-- | Count one sample written to this sample type's directory. Severity zero only receives the @valid@ samples the
-- decoder rejects, so it has its own count.
increaseWritten :: SampleType -> GenerationRun -> GenerationRun
increaseWritten sampleType run = case sampleType of
  Zap 0 -> run {rejected = rejected run + 1}
  _ -> run {written = written run + 1}

-- | Count one mutation dropped because the decoder accepts it.
increaseDropped :: GenerationRun -> GenerationRun
increaseDropped run = run {dropped = dropped run + 1}

-- | What became of one sample.
data Outcome = Written SampleType | Dropped

-- | Count one sample and remember its digest, so the same bytes are never counted twice.
record :: Text -> Outcome -> GenerationRun -> GenerationRun
record digest outcome run = addDigest digest $ case outcome of
  Written sampleType -> increaseWritten sampleType run
  Dropped -> increaseDropped run

-- | Return true if the run has enough accepted samples or has exhausted its attempts.
isRunFinished :: DatasetConfig -> GenerationRun -> Bool
isRunFinished config run =
  accepted run >= samplesNb run || attempts run >= maxAttempts run || shouldGiveUp config run

-- | Return true if the run should give up due to repeated exhausted batches and no accepted samples.
shouldGiveUp :: DatasetConfig -> GenerationRun -> Bool
shouldGiveUp config run = accepted run == 0 && exhaustions run >= configZapGiveUpAfter config

-- | Attempts a run may spend, whatever came back from them.
maxAttempts :: GenerationRun -> Int
maxAttempts run = 3 * samplesNb run

-- | Samples kept, whether in the category asked for or routed to severity zero.
accepted :: GenerationRun -> Int
accepted run = written run + rejected run

generatedFor :: GenerationRun -> Text -> Text
generatedFor run label =
  "generated "
    <> show (accepted run)
    <> "/"
    <> show (samplesNb run)
    <> " samples for "
    <> label
    <> " after "
    <> show (attempts run)
    <> " attempts in "
    <> show (batch run)
    <> " batches"

gaveUpZappingFor :: GenerationRun -> Text -> Text
gaveUpZappingFor run label =
  "gave up zapping "
    <> label
    <> " after "
    <> show (exhaustions run)
    <> " batches with too few decision points to zap"

exhaustedSearchFor :: GenerationRun -> Text -> Text
exhaustedSearchFor run label =
  "retried "
    <> label
    <> " after "
    <> show (exhaustions run)
    <> " generator search exhaustions"

droppedMutationsFor :: GenerationRun -> Text -> Text
droppedMutationsFor run label =
  "dropped "
    <> show (dropped run)
    <> " "
    <> label
    <> " mutations the decoder accepts"
