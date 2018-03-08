{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Codec.Archive.Zip
                 (Entry, eRelativePath, eUncompressedSize, fromEntry,
                 toArchive, zEntries)
import qualified Data.Array                      as Array
import qualified Data.ByteString.Lazy            as LBS
import           Data.Either                     (either)
import           Data.Reflection                 (Given, give, given)
import           Text.Regex.Base.RegexLike
                 (MatchArray, RegexOptions (blankCompOpt, blankExecOpt))
import qualified Text.Regex.Base.RegexLike       as Regex
import qualified Text.Regex.TDFA                 as TDFA
import qualified Text.Regex.TDFA.ByteString.Lazy as TDFA
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map

type HasRegexCache = Given RegexCache
type Tag = Text
type ErrorName = Text

withRegexCache :: RegexCache -> (HasRegexCache => r) -> r
withRegexCache = give

getRegexCache :: HasRegexCache => RegexCache
getRegexCache = given

type RegexString = LBS.ByteString

type Regex = LBS.ByteString -> [MatchArray]

compileRegex :: RegexString -> Either String Regex
compileRegex rx = Regex.matchAll <$> TDFA.compile blankCompOpt blankExecOpt rx

data RegexCache = RegexCache (Map ErrorName (Regex, ErrorHandler))

data ErrorHandler = ErrorHandler {
    errorHandlerFileRegex :: !Regex
  , errorHandlerRegex   :: !Regex
  , errorHandlerMessage :: !(Maybe Text)
  , errorHandlerTags    :: ![Tag]
}

compileAllTheRegex :: Either String RegexCache
compileAllTheRegex = do
  staleLockFile <- compileRegex "Wallet-1.0/open.lock: Locked by [0-9]+: resource busy"
  connRefused <- compileRegex "\"message\": \"connect ECONNREFUSED [^\"]*\""
  pure (RegexCache {..})

main :: IO ()
main = do
  rawzip <- LBS.readFile "logs.zip"
  regexCache <- either fail pure compileAllTheRegex
  withRegexCache regexCache $ do
    classifyLogs rawzip

classifyLogs :: HasRegexCache => LBS.ByteString -> IO ()
classifyLogs rawzip = do
  let archive = toArchive rawzip
  let entries = zEntries archive
  mapM_ parseFile entries

parseFile :: HasRegexCache => Entry -> IO ()
parseFile entry = do
  let rawlog = fromEntry entry
  let name = eRelativePath entry
  print name
  let
    result1 = connRefused getRegexCache rawlog
    result2 = staleLockFile getRegexCache rawlog
  print result1
  print result2
