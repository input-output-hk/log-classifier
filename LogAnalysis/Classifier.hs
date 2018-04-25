{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module LogAnalysis.Classifier
       (
         extractIssuesFromLogs
       , extractLogsFromZip
       ) where

import qualified Codec.Archive.Zip        as Zip
import qualified Data.ByteString.Lazy     as LBS
import           Data.List                (foldl')
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import           GHC.Stack                (HasCallStack)

import           LogAnalysis.Types        (Analysis, Knowledge (..))

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
compareWithKnowledge :: LT.Text -> Knowledge -> [LT.Text] -> [LT.Text]
compareWithKnowledge str Knowledge{..} xs =
    if kErrorText `LT.isInfixOf` str
    then str : xs
    else xs

-- | Filter out any records that are empty (i.e couldn't catch any string related)
filterAnalysis :: Analysis -> Either String Analysis
filterAnalysis as = do
    let filteredAnalysis = Map.filter (/=[]) as
    if null filteredAnalysis
      then Left "No issue found"
      else return filteredAnalysis

readZip :: HasCallStack => LBS.ByteString -> Either String (Map FilePath LBS.ByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left err      -> Left err
    Right archive -> Right $ finishProcessing archive
  where
    finishProcessing :: Zip.Archive -> Map FilePath LBS.ByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LBS.ByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)

-- | Extract log file from given zip file
extractLogsFromZip :: Int -> LBS.ByteString -> Either String [LBS.ByteString]
extractLogsFromZip numberOfFiles file = do
    zipMap <- readZip file                             -- Read File
    let extractedLogs = Map.elems $ Map.take numberOfFiles zipMap        -- Extract selected logs
    return extractedLogs
