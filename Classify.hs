{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Classify (classifyZip, ErrorCode(..), ErrorName, postProcessError, ConfirmedError(..)) where

import qualified Codec.Archive.Zip    as Zip

import           Control.Monad        (forM)


import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC

import           Regex                (Regex, MatchWithCaptures, RegexString, RegexCompileError, compileRegex, matchTest, matchAll)

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           GHC.Stack            (HasCallStack)
import           Data.List            (foldl')
import qualified Data.Text.Lazy       as LT
import qualified Data.Text.Lazy.Encoding as LT

type ErrorName = Text

data ErrorHandler
  = ErrorHandler
    { errorHandlerFileRegex :: !Regex
    , errorHandlerRegex     :: !Regex
    , errorHandlerMessage   :: LBS.ByteString -> [MatchWithCaptures] -> (Maybe ErrorCode)
    }

data ErrorCode =
    ConnectionRefused Int
  | StateLockFile LT.Text
  | FileNotFound [ LBS.ByteString ]
  | NetworkError [ LBS.ByteString ]
  deriving Show

data ConfirmedError =
    ConfirmedStaleLockFile LT.Text

newtype Behavior
  = Behavior (Map ErrorName ErrorHandler)

compileBehavior :: [(ErrorName, (RegexString, RegexString, LBS.ByteString -> [MatchWithCaptures] -> (Maybe ErrorCode)))]
                -> Either RegexCompileError Behavior
compileBehavior input = do
  pairs <- forM input $ \(errName, (frx, rx, toErrorCode)) -> do
    frxCompiled <- compileRegex frx
    rxCompiled  <- compileRegex rx
    let errHandler = ErrorHandler
                     { errorHandlerFileRegex = frxCompiled
                     , errorHandlerRegex     = rxCompiled
                     , errorHandlerMessage   = toErrorCode
                     }
    pure (errName, errHandler)
  pure (Behavior (Map.fromList pairs))

runBehavior :: HasCallStack => Behavior
            -> Map FilePath LBS.ByteString
            -> IO (Map FilePath [(ErrorName, ErrorCode)])
runBehavior (Behavior behavior) files = do
  let
    checkFile :: FilePath -> LBS.ByteString -> IO (Maybe [(ErrorName, ErrorCode)])
    checkFile fp contents = do
      hits <- Map.traverseMaybeWithKey (checkBehaviorOnFile fp contents) behavior
      if length hits > 0 then
        pure $ Just $ Map.toList hits
      else
        pure Nothing
    checkBehaviorOnFile :: FilePath -> LBS.ByteString -> ErrorName -> ErrorHandler -> IO (Maybe ErrorCode)
    checkBehaviorOnFile fp contents _ errhandler = do
      if matchTest (errorHandlerFileRegex errhandler) (LBSC.pack fp) then do
        let
          matches = matchAll (errorHandlerRegex errhandler) contents
        if (length matches) == 0 then
          pure Nothing
        else
          pure $ errorHandlerMessage errhandler contents matches
      else do
        pure Nothing
  Map.traverseMaybeWithKey checkFile files

readZip :: HasCallStack => LBS.ByteString -> Either String (Map FilePath LBS.ByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left error -> Left error
    Right archive -> Right $ finishProcessing archive
  where
    finishProcessing :: Zip.Archive -> Map FilePath LBS.ByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LBS.ByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)

classifyLogs :: HasCallStack => Behavior -> Map FilePath LBS.ByteString -> IO (Map FilePath [(ErrorName, ErrorCode)])
classifyLogs behavior zipmap = do
  runBehavior behavior zipmap

countRefused :: LBS.ByteString -> [ MatchWithCaptures ] -> Maybe ErrorCode
countRefused _ matches = Just $ ConnectionRefused $ length matches

getMatches :: LBS.ByteString -> [MatchWithCaptures] -> [LBS.ByteString]
getMatches fullBS matches = map (getOneMatch fullBS) matches

getOneMatch :: LBS.ByteString -> MatchWithCaptures -> LBS.ByteString
getOneMatch bs ((start, len), _) = LBS.take (fromIntegral len) $ LBS.drop (fromIntegral start) bs

classifyZip :: HasCallStack => LBS.ByteString -> IO (Either String (Map FilePath [(ErrorName, ErrorCode)]))
classifyZip rawzip = do
  --zip <- readZip <$> LBS.readFile "logs.zip"
  let
    zipmap' = readZip rawzip
  let
    behaviorList :: [ (ErrorName, (RegexString, RegexString, LBS.ByteString -> [MatchWithCaptures] -> Maybe ErrorCode) ) ]
    behaviorList
        = [ ( "conn-refused"
            , ( "Daedalus.log"
              , "\"message\": \"connect ECONNREFUSED [^\"]*\""
              , countRefused ) )
          , ( "stale-lock-file"
            , ( "node.pub$"
              , "[^\n]*Wallet[^\n]*/open.lock: Locked by [0-9]+: resource busy"
              , (\contents matches -> Just $ StateLockFile $ LT.decodeUtf8 $ last $ getMatches contents matches) ) )
          , ( "file-not-found"
            , ( "node.pub$"
              , ": [^ ]*: openBinaryFile: does not exist \\(No such file or directory\\)"
              , (\contents matches -> Just $ FileNotFound $ getMatches contents matches) ) )
          , ( "TransportError"
            , ( "node"
              , "TransportError[^\n]*"
              , (\contents matches -> Just $ NetworkError $ getMatches contents matches) ) )
          ]
  behavior <- either (fail . Text.unpack) pure
              $ compileBehavior behaviorList
  case zipmap' of
    Left error -> pure $ Left error
    Right zipmap -> do
      result <- classifyLogs behavior zipmap
      pure $ Right result

isStaleLockFile :: Map FilePath [(ErrorName, ErrorCode)] -> Maybe LT.Text
isStaleLockFile map = Map.foldl' checkFile Nothing map
  where
    checkFile :: Maybe LT.Text -> [(ErrorName, ErrorCode)] -> Maybe LT.Text
    checkFile state list = foldl' isStale state list
      where
        matches = foldl' isStale Nothing list
        isStale :: Maybe LT.Text -> (ErrorName, ErrorCode) -> Maybe LT.Text
        isStale (Just x) _ = Just x
        isStale Nothing (_, StateLockFile str) = Just str
        isStale x _ = x

postProcessError :: Map FilePath [(ErrorName, ErrorCode)] -> Either (Map FilePath [(ErrorName, ErrorCode)]) ConfirmedError
postProcessError input = finalAnswer
  where
    finalAnswer = case isStaleLockFile input of
      Just msg -> Right $ ConfirmedStaleLockFile msg
      Nothing -> Left input
