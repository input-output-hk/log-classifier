module Util
    ( extractLogsFromZip
    , tshow
    , readZip
    ) where

import           Universum

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as LBS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Extract log file from given zip file
extractLogsFromZip :: Int -> LBS.ByteString -> Either Text [LBS.ByteString]
extractLogsFromZip numberOfFiles file = do
    zipMap <- readZip file                             -- Read File
    let extractedLogs = Map.elems $ mTake numberOfFiles zipMap        -- Extract selected logs
    return extractedLogs
  where
    mTake n = Map.fromDistinctAscList . take n . Map.toAscList

readZip :: LBS.ByteString -> Either Text (Map FilePath LBS.ByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left err      -> Left (toText err)
    Right archive -> Right $ finishProcessing archive
  where
    finishProcessing :: Zip.Archive -> Map FilePath LBS.ByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LBS.ByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)
