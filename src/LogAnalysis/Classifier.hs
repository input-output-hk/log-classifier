{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LogAnalysis.Classifier
       ( extractErrorCodes
       , extractIssuesFromLogs
       , prettyFormatAnalysis
       , prettyFormatNoIssues
       , prettyFormatLogReadError
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.Text (isInfixOf)
import           Data.Text.Encoding.Error (ignore)

import           LogAnalysis.Types (Analysis, Knowledge (..), renderErrorCode)
import           Types (TicketInfo (..))

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
    if kErrorText `isInfixOf` (show str)
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


printTicketUrl :: TicketInfo -> Text
printTicketUrl TicketInfo{..} = "https://iohk.zendesk.com/agent/tickets/" <> show ticketId

prettyHeader :: Text
prettyHeader =
    "Dear user," <>
    "\n" <>
    "\n" <>
    "Thank you for contacting the IOHK Technical Support Desk. We appologize for the delay in responding to you. " <>
    "\n"

prettyFooter :: TicketInfo -> Text
prettyFooter ticketInfo =
    "\n\n" <>
        "Please be patient since we have a large number of such requests to respond to. We will respond as soon as we can, but in the meantime, if you want to check the status of your ticket you can do so here - " <> printTicketUrl ticketInfo <>
    "\n\n" <>
    "Please let us know if your issue is resolved. If you are still having trouble please reply back to this email and attach a new log file so that we can work with you to fix your problem." <>
    "\n\n" <>
    "Thanks, " <>
    "\n" <>
    "The IOHK Technical Support Desk Team" <>
    "\n"

-- | We need to fix this, this will pick the last issue found.
prettyFormatAnalysis :: Analysis -> TicketInfo -> Text
prettyFormatAnalysis as ticketInfo =
    prettyHeader <>
    prettyIssueComment <>
    foundIssues <>
    prettyFooter ticketInfo
  where
    prettyIssueComment  = "We have analyzed the log that you submitted and it appears that your issue is addressed in the Daedalus FAQ Please go to https://daedaluswallet.io/faq/ and check FAQ Issue(s) listed below to resolve your problem:"
    foundIssues         =
        foldr (\(Knowledge{..}, _) acc -> acc <> "\n- " <> kFAQNumber) "" $ Map.toList as

prettyFormatNoIssues :: TicketInfo -> Text
prettyFormatNoIssues ticketInfo =
    prettyHeader <>
    "We have analyzed the log that you submitted and it appears that you do not have an identifiable technical issue. Please be patient while our developers analyze this issue further since we have a large number of such requests to respond to." <>
    prettyFooter ticketInfo

prettyFormatLogReadError :: TicketInfo -> Text
prettyFormatLogReadError ticketInfo =
    prettyHeader <>
    "We tried to analyze the log that you submitted and it appears that your log cannot be processed. Please try sending the log file once again. Please go to https://daedaluswallet.io/faq/ and see Issue 200 for instructions. Please reply to this email with when you respond." <>
    prettyFooter ticketInfo

