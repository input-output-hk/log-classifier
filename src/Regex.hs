module Regex
  ( Regex
  , RegexString
  , RegexCompileError
  , Match
  , MatchWithCaptures
  , compileRegex
  , matchOnce
  , matchCount
  , matchTest
  , matchAll
  ) where

-- Not used, but want to use it in the future.
import           Universum

import qualified Data.Array as Array

import qualified Data.ByteString.Lazy as LBS

import qualified Text.Regex.TDFA as TDFA
import           Text.Regex.TDFA.ByteString.Lazy (Regex)
import qualified Text.Regex.TDFA.ByteString.Lazy as TDFA

import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just x) = Right x

type RegexString = LBS.ByteString

type RegexCompileError = Text

compileRegex :: RegexString -> Either RegexCompileError Regex
compileRegex = either (Left . Text.pack) Right
               . TDFA.compile TDFA.blankCompOpt TDFA.blankExecOpt

type CaptureIndex = Int

type Match = (TDFA.MatchOffset, TDFA.MatchLength)

type MatchWithCaptures = (Match, Map CaptureIndex Match)

fromMatchArray :: TDFA.MatchArray -> MatchWithCaptures
fromMatchArray ma = either error id $ do
  let m = Map.fromList (Array.assocs ma)
  fullMatch <- maybeToEither "this shouldn't happen" (Map.lookup 0 m)
  captures <- pure (Map.delete 0 m)
  pure (fullMatch, captures)

matchOnce :: Regex -> LBS.ByteString -> Maybe MatchWithCaptures
matchOnce regex string = fromMatchArray <$> TDFA.matchOnce regex string

matchCount :: Regex -> LBS.ByteString -> Int
matchCount = TDFA.matchCount

matchTest :: Regex -> LBS.ByteString -> Bool
matchTest = TDFA.matchTest

matchAll :: Regex -> LBS.ByteString -> [MatchWithCaptures]
matchAll regex string = map fromMatchArray (TDFA.matchAll regex string)

