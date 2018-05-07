{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LogAnalysis.Classifier
       ( extractIssuesFromLogs
       , extractErrorCodes
       , prettyFormatAnalysis
       ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.List                (foldl')
import qualified Data.Map.Strict          as Map
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           LogAnalysis.Types        (Analysis, Knowledge (..),
                                           toTag)

-- | Number of error texts it should show
numberOfErrorText :: Int
numberOfErrorText = 3

-- | Analyze each log file based on the knowlodgebases' data.
extractIssuesFromLogs :: [LBS.ByteString] -> Analysis -> Either String Analysis
extractIssuesFromLogs files analysis = filterAnalysis $ foldl' runClassifiers analysis files

-- | Run analysis on given file
runClassifiers :: Analysis -> LBS.ByteString -> Analysis
runClassifiers analysis logfile =
    let logLines = LT.lines $ LT.decodeUtf8With ignore logfile
    in foldl' analyzeLine analysis logLines

-- | Analyze each line
analyzeLine :: Analysis -> LT.Text -> Analysis
analyzeLine analysis str = Map.mapWithKey (compareWithKnowledge str) analysis

-- | Compare the line with knowledge lists
compareWithKnowledge :: LT.Text -> Knowledge -> [ LT.Text ] -> [ LT.Text ]
compareWithKnowledge str Knowledge{..} xs =
    if kErrorText `LT.isInfixOf` str
    then str : xs
    else xs

-- | Filter out any records that are empty (i.e couldn't catch any string related)
filterAnalysis :: Analysis -> Either String Analysis
filterAnalysis as = do
    let filteredAnalysis = Map.filter (/=[]) as
    if null filteredAnalysis
      then Left "Cannot find any known issues"
      else return $ Map.map (take numberOfErrorText) filteredAnalysis

extractErrorCodes :: Analysis -> [ Text ]
extractErrorCodes as = map (\(Knowledge{..}, _) -> toTag kErrorCode) $ Map.toList as

prettyFormatAnalysis :: Analysis -> LT.Text
prettyFormatAnalysis as =
    let aList = Map.toList as
    in foldr (\(Knowledge{..}, txts) acc ->
         "\n" <> LT.pack (show kErrorCode)
      <> "\n" <> kProblem
      <> "\n **" <> kSolution
      <> "** \n"
      <> foldr (\txt ts -> "\n" <> txt <> "\n" <> ts) LT.empty txts -- List errors
      <> "\n" <> acc
      <> "\n\n"
      ) LT.empty aList
