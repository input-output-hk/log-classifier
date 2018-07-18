module LogAnalysis.Exceptions
    ( LogAnalysisException(..)
    ) where

import Universum

data LogAnalysisException =
      LogReadException
    | NoIssueFound
    deriving Show

instance Exception LogAnalysisException