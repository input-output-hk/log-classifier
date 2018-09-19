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
data IssueID = IssueID Text Int

data Knowledge = Knowledge
    {  kErrorText :: !Text
    -- ^ Text used for matching error lines
    ,  kIssueID :: !IssueID
    -- ^ IssueID 
    }

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

instance Eq Knowledge where
    e1 == e2 = (kIssueID proj issueID) e1 == (kIssueID proj' issueID') e2

instance Ord Knowledge where
    compare e1 e2 = compare (kIssueID) (kIssueID)

instance Ord IssueID where
    compare (IssueID p1 n1) (IssueID p2 n2)
        | (p1 == p2) & (n1 == n2) = EQ
        | (p1 == p2) & (n1 > n2) = GT
        | otherwise = LT 
