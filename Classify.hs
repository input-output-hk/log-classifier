{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Classify (classifyZip, ErrorCode(..)) where

import qualified Codec.Archive.Zip    as Zip

import           Control.Monad        (forM)

import qualified Data.ByteString      as BS

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC

import           Regex

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           GHC.Stack

type Tag = Text

type ErrorName = Text

data ErrorHandler
  = ErrorHandler
    { errorHandlerFileRegex :: !Regex
    , errorHandlerRegex     :: !Regex
    , errorHandlerMessage   :: LBS.ByteString -> [MatchWithCaptures] -> (Maybe ErrorCode)
    }

data ErrorCode =
    ConnectionRefused Int
  | StateLockFile [MatchWithCaptures]
  | FileNotFound [ LBS.ByteString ]
  deriving Show

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
            -> (Maybe Text -> [Tag] -> IO ())
            -> IO (Map FilePath [(ErrorName, ErrorCode)])
runBehavior (Behavior behavior) files handler = do
  let
    checkFile :: FilePath -> LBS.ByteString -> IO (Maybe [(ErrorName, ErrorCode)])
    checkFile fp contents = do
      hits <- Map.traverseMaybeWithKey (checkBehaviorOnFile fp contents) behavior
      if length hits > 0 then
        pure $ Just $ Map.toList hits
      else
        pure Nothing
    checkBehaviorOnFile :: FilePath -> LBS.ByteString -> ErrorName -> ErrorHandler -> IO (Maybe ErrorCode)
    checkBehaviorOnFile fp contents errname errhandler = do
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

readZip :: LBS.ByteString -> Map FilePath LBS.ByteString
readZip = Map.fromList . map handleEntry . Zip.zEntries . Zip.toArchive
  where
    handleEntry :: Zip.Entry -> (FilePath, LBS.ByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)

classifyLogs :: HasCallStack => Behavior -> Map FilePath LBS.ByteString -> IO (Map FilePath [(ErrorName, ErrorCode)])
classifyLogs behavior zip = do
  runBehavior behavior zip printResults

printResults :: Maybe Text -> [ Tag ] -> IO ()
printResults = undefined

countRefused :: LBS.ByteString -> [ MatchWithCaptures ] -> Maybe ErrorCode
countRefused _ matches = Just $ ConnectionRefused $ length matches

getMatches :: LBS.ByteString -> [MatchWithCaptures] -> [LBS.ByteString]
getMatches fullBS matches = map (getOneMatch fullBS) matches

getOneMatch :: LBS.ByteString -> MatchWithCaptures -> LBS.ByteString
getOneMatch bs ((start, len), _) = LBS.take (fromIntegral len) $ LBS.drop (fromIntegral start) bs

classifyZip :: HasCallStack => LBS.ByteString -> IO (Map FilePath [(ErrorName, ErrorCode)])
classifyZip rawzip = do
  --zip <- readZip <$> LBS.readFile "logs.zip"
  let
    zip = readZip rawzip
  let behaviorList
        = [ ( "conn-refused"
            , ( "Daedalus.log"
              , "\"message\": \"connect ECONNREFUSED [^\"]*\""
              , countRefused ) )
          , ( "stale-lock-file"
            , ( "node.pub$"
              , "Wallet.*/open.lock: Locked by [0-9]+: resource busy"
              , (\contents matches -> Just $ StateLockFile matches) ) )
          , ( "file-not-found"
            , ( "node.pub$"
              , ": [^ ]*: openBinaryFile: does not exist \\(No such file or directory\\)"
              , (\contents matches -> Just $ FileNotFound $ getMatches contents matches) ) )
          ]
  behavior <- either (fail . Text.unpack) pure
              $ compileBehavior behaviorList
  classifyLogs behavior zip
