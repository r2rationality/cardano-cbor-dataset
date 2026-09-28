{-# LANGUAGE DeriveAnyClass #-}

-- | Why a sample fails, in a module of its own so that a rule can say why it cannot give the reference of a sample.
module Generation.VerificationFailure where

-- | A value describing why the verification of a sample failed
data VerificationFailure = VerificationFailure FailureKind Text deriving (Eq, Show)

instance Exception VerificationFailure

formatVerificationFailure :: VerificationFailure -> Text
formatVerificationFailure (VerificationFailure kind message) = category kind <> ": " <> message
  where
    category DecodeFailed = "decode"
    category DecodeSucceeded = "decode"
    category SampleUnreadable = "decode"
    category EncodeFailed = "encode"
    category ReferenceMismatch = "encode"
    category ByteExactMismatch = "encode"
    category ReferenceUnreadable = "encode"

-- | The category and diagnostic of a sample failure.
data FailureKind
  = DecodeFailed
  | DecodeSucceeded
  | ReferenceMismatch
  | ByteExactMismatch
  | ReferenceUnreadable
  | SampleUnreadable
  | EncodeFailed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)
