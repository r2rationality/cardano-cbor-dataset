module Base.Extra where

import Control.Exception

-- | If the condition is true perform the action otherwise return the empty element
whenMonoid :: (Monad m, Monoid a) => Bool -> m a -> m a
whenMonoid condition action =
  if condition then action else pure mempty

-- | Throw an exception if the 'Either' is 'Left', otherwise return the 'Right' value.
throwEither :: (Exception a) => Either a b -> IO b
throwEither (Left err) = throwIO err
throwEither (Right val) = pure val

-- | Throw an exception with a 'Text' message if the 'Either' is 'Left', otherwise return the 'Right' value.
throwEitherText :: Either Text b -> IO b
throwEitherText (Left err) = throwIO (TextException err)
throwEitherText (Right val) = pure val

newtype TextException = TextException Text
  deriving (Show)

instance Exception TextException

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate
