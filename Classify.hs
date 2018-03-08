{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Codec.Archive.Zip    as Zip

import           Control.Monad        (forM)

import qualified Data.ByteString      as BS

import qualified Data.ByteString.Lazy as LBS

import           Regex

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

type Tag = Text

type ErrorName = Text

data ErrorHandler
  = ErrorHandler
    { errorHandlerFileRegex :: !Regex
    , errorHandlerRegex     :: !Regex
    , errorHandlerMessage   :: !(Maybe Text)
    , errorHandlerTags      :: ![Tag]
    }

newtype Behavior
  = Behavior (Map ErrorName ErrorHandler)

compileBehavior :: [(ErrorName, (RegexString, RegexString, Maybe Text, [Tag]))]
                -> Either RegexCompileError Behavior
compileBehavior input = do
  pairs <- forM input $ \(errName, (frx, rx, msg, tags)) -> do
    frxCompiled <- compileRegex frx
    rxCompiled  <- compileRegex rx
    let errHandler = ErrorHandler
                     { errorHandlerFileRegex = frxCompiled
                     , errorHandlerRegex     = rxCompiled
                     , errorHandlerMessage   = msg
                     , errorHandlerTags      = tags
                     }
    pure (errName, errHandler)
  pure (Behavior (Map.fromList pairs))

runBehavior :: Behavior
            -> Map FilePath LBS.ByteString
            -> (Maybe Text -> [Tag] -> IO ())
            -> IO ()
runBehavior = _

readZip :: LBS.ByteString -> Map FilePath LBS.ByteString
readZip = Map.fromList . map handleEntry . Zip.zEntries . Zip.toArchive
  where
    handleEntry :: Zip.Entry -> (FilePath, LBS.ByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)

classifyLogs :: Behavior -> Map FilePath LBS.ByteString -> IO ()
classifyLogs behavior zip = do
  undefined

main :: IO ()
main = do
  zip <- readZip <$> LBS.readFile "logs.zip"
  let behaviorList
        = [ ( "conn-refused"
            , ( ".*"
              , "\"message\": \"connect ECONNREFUSED [^\"]*\""
              , Just "FIXME: message"
              , ["FIXME: tags"] ) )
          , ( "stale-lock-file"
            , ( ".*"
              , "Wallet-1.0/open.lock: Locked by [0-9]+: resource busy"
              , Just "FIXME: message"
              , ["FIXME: tags"] ) )
          ]
  behavior <- either (fail . Text.unpack) pure
              $ compileBehavior behaviorList
  classifyLogs behavior zip
