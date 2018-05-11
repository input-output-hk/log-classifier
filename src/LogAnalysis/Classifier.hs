{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LogAnalysis.Classifier
       ( extractIssuesFromLogs
       , extractErrorCodes
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

-- | TODO (Hiroto): Format the text in better way
prettyFormatAnalysis :: Analysis -> LText
prettyFormatAnalysis as =
    let aList = Map.toList as
    in foldr (\(Knowledge{..}, txts) acc ->
         "\n" <> show kErrorCode
      <> "\n" <> kProblem
      <> "\n **" <> kSolution
      <> "** \n"
      <> foldr1 (\txt ts -> "\n" <> txt <> "\n" <> ts) txts -- List errors
      <> "\n" <> acc
      <> "\n\n"
      ) "" aList
