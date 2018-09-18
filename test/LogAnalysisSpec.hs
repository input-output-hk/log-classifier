{-# OPTIONS_GHC -fno-warn-orphans #-}

module LogAnalysisSpec
    ( classifierSpec
    ) where

import           Universum

import           Data.Aeson (eitherDecodeStrict')
import           Data.ByteString.Char8 (pack)
import           Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian,
                            secondsToDiffTime)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), Gen, arbitrary, choose, elements, forAll)

import           LogAnalysis.Types (CardanoLog)

-- | Classifier test
-- Test fails when log contains unicode characters!!
classifierSpec :: Spec
classifierSpec =
    describe "runClassifierJSON" $ modifyMaxSuccess (const 1000) $
       it "should be able to decode cardano log" $
        forAll randomLogText $ \(logText :: ByteString) -> do
            let decodedText = eitherDecodeStrict' logText :: Either String CardanoLog
            isRight decodedText `shouldBe` True

-- https://gist.github.com/agrafix/2b48ec069693e3ab851e
instance Arbitrary UTCTime where
    arbitrary =
        do randomDay <- choose (1, 29) :: Gen Int
           randomMonth <- choose (1, 12) :: Gen Int
           randomYear <- choose (2001, 2018) :: Gen Integer
           randomTime <- choose (0, 86401) :: Gen Int64
           return $ UTCTime 
               (fromGregorian randomYear randomMonth randomDay)
               (secondsToDiffTime $ fromIntegral randomTime)

instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary

showIso8601 :: UTCTime -> String
showIso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

encodedElements :: [Text] -> Gen ByteString
encodedElements xs = encodeUtf8 <$> elements xs

-- | Generate random cardano log
randomLogText :: Gen ByteString
randomLogText = do
    randomTime <- arbitrary :: Gen UTCTime
    randomEnv  <- encodedElements ["mainnet_wallet_macos64:1.3.0"]
    randomNs   <- encodedElements ["cardano-sl", "NtpClient"]
    randomApp  <- encodedElements ["cardano-sl"]
    -- Ensure it can decode message with unicode characters
    randomMsg  <- encodedElements
        ["Error Message","Passive Wallet kernel initialized.", "塩井ひろと"
        , "Evaluated clock offset NtpOffset {getNtpOffset = 24688mcs}mcs"]
    randomPid  <- arbitrary :: Gen Int
    randomHost <- encodedElements (["hostname"] :: [Text])
    randomSev  <- encodedElements ["Info", "Warning", "Error", "Notice"]
    randomThreadId <- arbitrary :: Gen Int

    pure $
        "{\
        \\"at\": \"" <> (pack . showIso8601) randomTime <>"\",\
        \\"env\": \"" <> randomEnv <> "\",\
        \\"ns\": [\
            \\"cardano-sl\",\
            \\""<> randomNs <>"\"\
        \],\
        \\"data\": {},\
        \\"app\": [\""<> randomApp <> "\"],\
        \\"msg\": \""<> randomMsg <> "\",\
        \\"pid\": \""<> show randomPid <> "\",\
        \\"loc\": null,\
        \\"host\": \""<> randomHost <> "\",\
        \\"sev\": \""<> randomSev <> "\",\
        \\"thread\": \"ThreadId " <> show randomThreadId <> "\"\
        \}"