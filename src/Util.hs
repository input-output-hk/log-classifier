module Util
       ( extractLogsFromZip
       , readZip
       ) where

import           Universum

import qualified Codec.Archive.Zip as Zip
import qualified Data.Map.Strict as Map
import Control.Exception.Safe (tryDeep)

import Exceptions (ClassifierExceptions(..))

-- | Extract log file from given zip file
-- TODO(ks): What happens with the other files? We just ignore them?
extractLogsFromZip :: (MonadCatch m, MonadIO m) => Int -> LByteString -> m [LByteString]
extractLogsFromZip numberOfFiles file = do
    zipMap <- readZip file  -- Read File
    let extractedLogs = Map.elems $ mTake numberOfFiles zipMap  -- Extract selected logs
    return extractedLogs
  where
    mTake :: Int -> Map k a -> Map k a
    mTake n = Map.fromDistinctAscList . take n . Map.toAscList

-- | Read zipe file
-- toArchiveOrFail is a partial function, we need to use tryDeep to catch the exception and throw it 
-- upwards
-- Why tryDeep instead of try? It's because we need to fully evaluate bytestrings in order to catch
-- the decompression issue.
readZip :: (MonadCatch m, MonadIO m) => LByteString -> m (Map FilePath LByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left _      -> throwM ReadZipFileException
    Right archive -> do
      eArchive <- tryDeep (pure $ finishProcessing archive)
      case eArchive of
        Left (_ :: SomeException) -> throwM DecompressionException
        Right files -> return files 
  where
    finishProcessing :: Zip.Archive -> Map FilePath LByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)
