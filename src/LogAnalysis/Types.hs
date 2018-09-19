{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.Types
       ( Analysis
       , Knowledge (..)
       , setupAnalysis
       ) where

import           Universum
import           Prelude (Show (..))

import qualified Data.Map.Strict as Map


-- | Record identifying the issue
data IssueID = IssueID Text Int deriving (Eq, Ord)

data Knowledge = Knowledge
    {  kErrorText :: !Text
    -- ^ Text used for matching error lines
    ,  kIssueID :: !IssueID
    -- ^ IssueID 
    } deriving (Eq, Ord)

--type CSVHeader = [Text]
instance Show IssueID where
    show (IssueID proj issueID) = Prelude.show proj ++ "-" ++ Prelude.show issueID

instance Show Knowledge where
    show Knowledge{..} =
        "{  errorText  = " <> Prelude.show kErrorText   <>
        ",  issueID  = " <> Prelude.show kIssueID   <>
        "}"

-- | Sorted accoring to knowledgebase.

-- | Map used to collect error lines
type Analysis = Map Knowledge [Text]

-- | Create initial analysis environment
setupAnalysis :: [Knowledge] -> Analysis
setupAnalysis kbase = Map.fromList $ map (\kn -> (kn, [])) kbase

