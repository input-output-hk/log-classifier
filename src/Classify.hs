{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Currenty not used..
module Classify
       ( ConfirmedError(..)
       , ErrorCode(..)
       , ErrorName
       , classifyZip
       , postProcessError
       ) where

import           Universum

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)
import           Regex (MatchWithCaptures, Regex, RegexCompileError, RegexString, compileRegex,
                        matchAll, matchTest)


type ErrorName = Text

data ErrorHandler = ErrorHandler
    { errorHandlerFileRegex :: !Regex
    , errorHandlerRegex     :: !Regex
    , errorHandlerMessage   :: LByteString -> [MatchWithCaptures] -> Maybe ErrorCode
    }

data ErrorCode =
    ConnectionRefused Int
  | StateLockFile LText
  | FileNotFound [ LByteString ]
  | NetworkError [ LByteString ]
  deriving Show

newtype  ConfirmedError = ConfirmedStaleLockFile LText

newtype Behavior = Behavior (Map ErrorName ErrorHandler)

compileBehavior :: [(ErrorName, (RegexString, RegexString,
                   LByteString -> [MatchWithCaptures] -> Maybe ErrorCode))]
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
            -> Map FilePath LByteString
            -> IO (Map FilePath [(ErrorName, ErrorCode)])
runBehavior (Behavior behavior) files = do
    let
        checkFile :: FilePath -> LByteString -> IO (Maybe [(ErrorName, ErrorCode)])
        checkFile fp contents = do
            hits <- Map.traverseMaybeWithKey (checkBehaviorOnFile fp contents) behavior
            if length hits > 0
            then pure $ Just $ Map.toList hits
            else pure Nothing
        checkBehaviorOnFile :: FilePath
                            -> LByteString
                            -> ErrorName
                            -> ErrorHandler
                            -> IO (Maybe ErrorCode)
        checkBehaviorOnFile fp contents _ errhandler =
            if matchTest (errorHandlerFileRegex errhandler) (LBSC.pack fp) then do
                let matches = matchAll (errorHandlerRegex errhandler) contents
                if (length matches) == 0 then
                  pure Nothing
                else
                  pure $ errorHandlerMessage errhandler contents matches
          else pure Nothing
    Map.traverseMaybeWithKey checkFile files

readZip :: HasCallStack => LByteString -> Either String (Map FilePath LByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left err      -> Left err
    Right archive -> Right $ finishProcessing archive
  where
    finishProcessing :: Zip.Archive -> Map FilePath LByteString
    finishProcessing = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)

classifyLogs :: HasCallStack
             => Behavior
             -> Map FilePath LByteString
             -> IO (Map FilePath [(ErrorName, ErrorCode)])
classifyLogs = runBehavior

countRefused :: LByteString -> [ MatchWithCaptures ] -> Maybe ErrorCode
countRefused _ matches = Just $ ConnectionRefused $ length matches

getMatches :: LByteString -> [MatchWithCaptures] -> [LByteString]
getMatches fullBS = map (getOneMatch fullBS)

getOneMatch :: LByteString -> MatchWithCaptures -> LByteString
getOneMatch bs ((start, len), _) =
    LBSC.take (fromIntegral len) $ LBSC.drop (fromIntegral start) bs

classifyZip :: HasCallStack
            => LByteString
            -> IO (Either String (Map FilePath [(ErrorName, ErrorCode)]))
classifyZip rawzip = do
  --zip <- readZip <$> LreadFile "logs.zip"
  let zipmap' = readZip rawzip
      behaviorList :: [(ErrorName, (RegexString, RegexString,
                        LByteString -> [MatchWithCaptures] -> Maybe ErrorCode))]
      behaviorList
          = [ ( "conn-refused"
              , ( "Daedalus.log"
                , "\"message\": \"connect ECONNREFUSED [^\"]*\""
                , countRefused ) )
            , ( "stale-lock-file"
              , ( "node.pub$"
                , "[^\n]*Wallet[^\n]*/open.lock: Locked by [0-9]+: resource busy"
                , \contents matches -> Just $ StateLockFile $ decodeUtf8
                      $ last $ N.fromList $ getMatches contents matches ) )
            , ( "file-not-found"
              , ( "node.pub$"
                , ": [^ ]*: openBinaryFile: does not exist \\(No such file or directory\\)"
                , \contents matches -> Just $ FileNotFound $ getMatches contents matches ) )
            , ( "TransportError"
              , ( "node"
                , "TransportError[^\n]*"
                , \contents matches -> Just $ NetworkError $ getMatches contents matches ) )
            ]
  behavior <- either (fail . toString) pure
              $ compileBehavior behaviorList
  case zipmap' of
      Left err     -> pure $ Left err
      Right zipmap -> Right <$> classifyLogs behavior zipmap

isStaleLockFile :: Map FilePath [(ErrorName, ErrorCode)] -> Maybe LText
isStaleLockFile = Map.foldl' checkFile Nothing
  where
    checkFile :: Maybe LText -> [(ErrorName, ErrorCode)] -> Maybe LText
    checkFile = foldl' isStale
      where
        isStale :: Maybe LText -> (ErrorName, ErrorCode) -> Maybe LText
        isStale (Just x) _                     = Just x
        isStale Nothing (_, StateLockFile str) = Just str
        isStale x _                            = x

postProcessError :: Map FilePath [(ErrorName, ErrorCode)]
                 -> Either (Map FilePath [(ErrorName, ErrorCode)]) ConfirmedError
postProcessError input = finalAnswer
  where
    finalAnswer = case isStaleLockFile input of
        Just msg -> Right $ ConfirmedStaleLockFile msg
        Nothing  -> Left input
