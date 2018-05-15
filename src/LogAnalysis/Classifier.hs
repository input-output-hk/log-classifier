{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LogAnalysis.Classifier
       ( extractErrorCodes
       , extractIssuesFromLogs
       , prettyFormatAnalysis
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.Text.Encoding.Error (ignore)
import           Data.Text.Lazy (isInfixOf)

import           LogAnalysis.Types (Analysis, Knowledge (..), renderErrorCode)

-- | Number of error texts it should show
numberOfErrorText :: Int
numberOfErrorText = 3

-- | Analyze each log file based on the knowlodgebases' data.
extractIssuesFromLogs :: [LByteString] -> Analysis -> Either Text Analysis
extractIssuesFromLogs files analysis = filterAnalysis $ foldl' runClassifiers analysis files

-- | Run analysis on given file
runClassifiers :: Analysis -> LByteString -> Analysis
runClassifiers analysis logfile =
    let logLines = lines $ decodeUtf8With ignore (LBS.toStrict logfile)
    in foldl' analyzeLine analysis (toLText <$> logLines)

-- | Analyze each line
analyzeLine :: Analysis -> LText -> Analysis
analyzeLine analysis str = Map.mapWithKey (compareWithKnowledge str) analysis

-- | Compare the line with knowledge lists
compareWithKnowledge :: LText -> Knowledge -> [ LText ] -> [ LText ]
compareWithKnowledge str Knowledge{..} xs =
    if kErrorText `isInfixOf` str
    then str : xs
    else xs

-- | Filter out any records that are empty (i.e couldn't catch any string related)
filterAnalysis :: Analysis -> Either Text Analysis
filterAnalysis as = do
    let filteredAnalysis = Map.filter (/=[]) as
    if null filteredAnalysis
    then Left "Cannot find any known issues"
    else return $ Map.map (take numberOfErrorText) filteredAnalysis

extractErrorCodes :: Analysis -> [ Text ]
extractErrorCodes as = map (\(Knowledge{..}, _) -> renderErrorCode kErrorCode) $ Map.toList as

-- | We need to fix this, this will pick the last issue found.
prettyFormatAnalysis :: Analysis -> LText
prettyFormatAnalysis as =
    foldr (\(Knowledge{..}, _) _ ->
    "Dear user," <>
    "\n" <>
    "\n" <>
        "Thank you for contacting the IOHK Technical Support Desk. We appologize for the delay in responding to you. We have analyzed the log that you submitted and it appears that your issue is addressed in the Daedalus FAQ in Issue " <> kFAQNumber <> ". Please go to https://daedaluswallet.io/faq/ and check Issue " <> kFAQNumber <> " to resolve your problem." <>
    "\n" <>
    "Please let us know if your issue is resolved. If you are still having trouble please reply back to this email and attach a new log file so that we can work with you to fix your problem." <>
    "\n" <>
    "Thanks, " <>
    "\n" <>
    "The IOHK Technical Support Desk Team" <>
    "\n") LT.empty aList
  where
    aList  = Map.toList as

-- | The formatting we use to respond as the analysis result. In the @ZenDesk@ case,
-- this is the comment.
_prettyOldFormatAnalysis :: Analysis -> LT.Text
_prettyOldFormatAnalysis as =
    let aList = Map.toList as
    in foldr (\(Knowledge{..}, txts) acc ->
                "\n" <> show kErrorCode
             <> "\n" <> kProblem
             <> "\n **" <> kSolution
             <> "** \n"
             <> foldr1 (\txt ts -> "\n" <> txt <> "\n" <> ts) txts  -- List errors
             <> "\n" <> acc
             <> "\n\n"
             ) "" aList
