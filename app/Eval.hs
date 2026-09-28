-- | Helpers for expression evaluation in HLS 2.10.
module Eval (prettyEval) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Text.Lazy qualified as TL
import Text.Pretty.Simple (pShowNoColor)

-- | Display a 'Show' value on multiple lines in an evaluation comment.
-- HLS 2.10 captures exception text, but not standard output. This helper
-- deliberately throws an exception and is only for interactive evaluation.
prettyEval :: (Show a) => a -> IO ()
prettyEval value = throwIO $ ErrorCall $ TL.unpack $ pShowNoColor value
