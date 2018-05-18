module Util
       ( extractLogsFromZip
       , readZip
       ) where

import           Universum

import qualified Codec.Archive.Zip as Zip
import qualified Data.Map.Strict as Map

-- | Extract log file from given zip file
-- TODO(ks): What happens with the other files? We just ignore them?
extractLogsFromZip :: Int -> LByteString -> Either Text [LByteString]
extractLogsFromZip numberOfFiles file = do
    zipMap <- readZip file  -- Read File
    let extractedLogs = Map.elems $ mTake numberOfFiles zipMap  -- Extract selected logs
    return extractedLogs
  where
    mTake :: Int -> Map k a -> Map k a
    mTake n = Map.fromDistinctAscList . take n . Map.toAscList

-- | Read zipe file
readZip :: LByteString -> Either Text (Map FilePath LByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left err      -> Left (toText err)
    Right archive -> Right $ finishProcessing archive
  where
    finishProcessing :: Zip.Archive -> Map FilePath LByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)
