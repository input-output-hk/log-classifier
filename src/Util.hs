module Util
       ( extractLogsFromZip
       , readZip
       ) where

import           Universum

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

import           Exceptions (ZipFileExceptions (..))

-- | Extract log file from given zip file
-- TODO(ks): What happens with the other files? We just ignore them?
extractLogsFromZip :: Int -> LByteString -> Either ZipFileExceptions [ByteString]
extractLogsFromZip numberOfFiles file = do
    zipMap <- readZip file  -- Read File
    let extractedLogs :: [ByteString]
        extractedLogs = map LBS.toStrict . Map.elems . mTake numberOfFiles $ zipMap
    return extractedLogs
  where
    mTake :: Int -> Map k a -> Map k a
    mTake n = Map.fromDistinctAscList . take n . Map.toAscList

-- | Read zipe file
-- toArchiveOrFail is a partial function, so be careful.
readZip :: LByteString -> Either ZipFileExceptions (Map FilePath LByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left _        -> Left ReadZipFileException
    Right archive -> return $ finishProcessing archive
  where
    finishProcessing :: Zip.Archive -> Map FilePath LByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)
