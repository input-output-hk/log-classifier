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
import qualified Data.Map.Strict as Map
import qualified Text.Regex.TDFA as TDFA
import           Text.Regex.TDFA.ByteString.Lazy (Regex)
import qualified Text.Regex.TDFA.ByteString.Lazy as TDFA

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just x) = Right x

type RegexString = LByteString

type RegexCompileError = Text

compileRegex :: RegexString -> Either RegexCompileError Regex
compileRegex = either (Left . toText) Right
               . TDFA.compile TDFA.blankCompOpt TDFA.blankExecOpt

type CaptureIndex = Int

type Match = (TDFA.MatchOffset, TDFA.MatchLength)

type MatchWithCaptures = (Match, Map CaptureIndex Match)

fromMatchArray :: TDFA.MatchArray -> MatchWithCaptures
fromMatchArray ma = either error id $ do
    let m = Map.fromList (Array.assocs ma)
    fullMatch <- maybeToEither "this shouldn't happen" (Map.lookup 0 m)
    let captures = Map.delete 0 m
    pure (fullMatch, captures)

matchOnce :: Regex -> LByteString -> Maybe MatchWithCaptures
matchOnce regex string = fromMatchArray <$> TDFA.matchOnce regex string

matchCount :: Regex -> LByteString -> Int
matchCount = TDFA.matchCount

matchTest :: Regex -> LByteString -> Bool
matchTest = TDFA.matchTest

matchAll :: Regex -> LByteString -> [MatchWithCaptures]
matchAll regex string = map fromMatchArray (TDFA.matchAll regex string)

