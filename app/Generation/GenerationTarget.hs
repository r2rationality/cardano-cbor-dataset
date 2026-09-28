module Generation.GenerationTarget where

import Generation.EraSpec
import Generation.Rule
import Generation.SampleType
import Path

-- | The rule and sample type that one run generates, and where its samples go.
data GenerationTarget = GenerationTarget
  { targetEra :: EraSpec,
    targetRule :: GenerationRule,
    targetSampleType :: SampleType,
    targetRuleDir :: Path Abs Dir
  }
  deriving (Eq, Show)

-- | How a target reads in a message: @rule/sample-type@.
targetLabel :: GenerationTarget -> Text
targetLabel target = ruleNameText (targetRule target) <> "/" <> sampleDirectory (targetSampleType target)

targetDirectoryName :: GenerationTarget -> Text
targetDirectoryName = sampleDirectory . targetSampleType

targetRuleName :: GenerationTarget -> Text
targetRuleName = ruleNameText . targetRule
