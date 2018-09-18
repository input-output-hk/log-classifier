module LogAnalysis.Exceptions
    ( LogAnalysisException(..)
    ) where

import Universum

-- | Exceptions that can occur during log analysis
data LogAnalysisException =
      LogReadException
    -- ^ Failed to read the log file because the log file was corrupted
    | NoKnownIssueFound
    -- ^ No known issue was found
    | JSONDecodeFailure String
    deriving Show

instance Exception LogAnalysisException