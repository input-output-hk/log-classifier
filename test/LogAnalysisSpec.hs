{-# OPTIONS_GHC -fno-warn-orphans #-}

module LogAnalysisSpec
    ( classifierSpec
    ) where

import           Universum

import           Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString.Char8 as C8
import           Data.Time (UTCTime (..), defaultTimeLocale, formatTime)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, arbitrary, choose, elements,
                                  forAll, vectorOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           LogAnalysis.Classifier (extractMessages)
import           LogAnalysis.Types (CardanoLog)

-- | Classifier tests
classifierSpec :: Spec
classifierSpec = do
    describe "CardanoLog FromJSON" $ modifyMaxSuccess (const 1000) $
       it "should be able to decode cardano log" $
           forAll randomLogText $ \(logText :: ByteString) -> do
               let decodedText = eitherDecodeStrict' logText :: Either String CardanoLog
               isRight decodedText `shouldBe` True
    -- Decoding is actually really slow so we're reducing the number of tests
    describe "extractMessages" $ modifyMaxSuccess (const 50) $ do
        it "should be able to extract log messages from JSON file" $
            forAll ((,) <$> randomLogJSONFilePath <*> randomJSONLogFile)
                $ \logWithFilePath -> testExtractMessage logWithFilePath
        it "should be able to extract log messages from plain log file" $
            forAll ((,) <$> randomLogFilePath <*> randomLogFile)
                $ \logWithFilePath -> testExtractMessage logWithFilePath

testExtractMessage :: (FilePath, ByteString) -> Property
testExtractMessage logWithFilePath =
    monadicIO $ do
        eLogLines <- run $ tryAny $ extractMessages logWithFilePath
        
        -- Check if the decoding was successfull
        assert $ isRight eLogLines
        whenRight eLogLines $ \logLines ->
            (assert . not . null) logLines

-- | Formant given UTCTime into ISO8601
showIso8601 :: UTCTime -> String
showIso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

-- | Given an list of text, pick and element and encode it
encodedElements :: [Text] -> Gen ByteString
encodedElements xs = encodeUtf8 <$> elements xs

-- | Generate random cardano log
randomLogText :: Gen ByteString
randomLogText = do
    randomTime     <- arbitrary :: Gen UTCTime
    randomEnv      <- encodedElements ["mainnet_wallet_macos64:1.3.0"]
    randomNs       <- encodedElements ["cardano-sl", "NtpClient"]
    randomApp      <- encodedElements ["cardano-sl"]
    -- Ensure it can decode message with non-latin characters
    randomMsg      <- encodedElements
        [ "Error Message","Passive Wallet kernel initialized.", "塩井ひろと"
        , "Evaluated clock offset NtpOffset {getNtpOffset = 24688mcs}mcs"]
    randomPid      <- arbitrary :: Gen Int
    randomHost     <- encodedElements ["hostname"]
    randomSev      <- encodedElements ["Info", "Warning", "Error", "Notice"]
    randomThreadId <- arbitrary :: Gen Int

    pure $
        "{                                                           \
        \\"at\": \"" <> (encodeUtf8 . showIso8601) randomTime <>"\", \
        \\"env\": \"" <> randomEnv <> "\",                           \
        \\"ns\": [                                                   \
        \    \"cardano-sl\",                                         \
        \    \""<> randomNs <>"\"                                    \
        \],                                                          \
        \\"data\": {},                                               \
        \\"app\": [\""<> randomApp <> "\"],                          \
        \\"msg\": \""<> randomMsg <> "\",                            \
        \\"pid\": \""<> show randomPid <> "\",                       \
        \\"loc\": null,                                              \
        \\"host\": \""<> randomHost <> "\",                          \
        \\"sev\": \""<> randomSev <> "\",                            \
        \\"thread\": \"ThreadId " <> show randomThreadId <> "\"      \
        \}"

-- | Generate random cardano-log file path
-- sample: node.json-20180911134009
randomLogJSONFilePath :: Gen FilePath
randomLogJSONFilePath = do
    randomNum <- arbitrary :: Gen Int
    pure $ "node.json-" <> show randomNum

-- | Generate random cardano-log json file
randomJSONLogFile :: Gen ByteString
randomJSONLogFile = do
    numOfLines <- choose (1,10000)
    logLines   <- vectorOf numOfLines randomLogText
    pure $ C8.unlines logLines

-- | Generate random plain cardano-log file path
-- Sample: node-20180911134009
randomLogFilePath :: Gen FilePath
randomLogFilePath = do
    randomNum <- arbitrary :: Gen Int
    pure $ "node-" <> show randomNum

-- | Generate random plain cardano-log file
randomLogFile :: Gen ByteString
randomLogFile = do
    numOfLines <- choose (1,10000)
    logLines <- vectorOf numOfLines (arbitrary :: Gen ByteString)
    pure $ C8.unlines logLines
