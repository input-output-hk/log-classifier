{-# OPTIONS_GHC -fno-warn-orphans #-}

module LogAnalysisSpec
    ( classifierSpec
    ) where

import           Universum

import           Data.Aeson (decode, eitherDecodeStrict', encode)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime (..), defaultTimeLocale, formatTime)

import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Gen, arbitrary, choose, elements, forAll,
                                  vectorOf)
import           Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run)

import           LogAnalysis.Classifier (extractIssuesFromLogs, extractMessages)
import           LogAnalysis.Exceptions (LogAnalysisException (..))
import           LogAnalysis.Types (Analysis, CardanoLog, Knowledge (..), LogFile, setupAnalysis,
                                    toLogFile)

-- | Classifier tests
classifierSpec :: Spec
classifierSpec = do
    describe "CardanoLog JSON" $ modifyMaxSuccess (const 1000) $ do
       it "should be able to decode cardano log" $
           forAll (genJSONLog Nothing) $ \(logText :: ByteString) -> do
               let decodedText = eitherDecodeStrict' logText :: Either String CardanoLog
               isRight decodedText `shouldBe` True

       prop "should be able to perform JSON serialization roundtrip" $
           \(cardanoLog :: CardanoLog) ->
                (decode . encode) cardanoLog `shouldBe` Just cardanoLog

    describe "extractMessages" $ modifyMaxSuccess (const 100) $ do
        it "should be able to extract log messages from JSON file" $
            forAll genLogJSONFilePath $ \path ->
                forAll (genJSONLogFile Nothing) $ \file ->
                    monadicIO $ do
                        eLogLines <- run $ try $ extractMessages (toLogFile path file)
                        -- Check if the decoding was successful
                        assert $ isRight (eLogLines :: Either LogAnalysisException [Text])
                        whenRight eLogLines $ \logLines ->
                            (assert . not . null) logLines

        it "should be able to extract log messages from text log file" $
            forAll genLogFilePath $ \path ->
                forAll (genLogFile Nothing) $ \file ->
                    monadicIO $ do
                        eLogLines <- run $ try $ extractMessages (toLogFile path file)

                        assert $ isRight (eLogLines :: Either LogAnalysisException [Text])
                        whenRight eLogLines $ \logLines ->
                            (assert . not . null) logLines

        it "should throw exception when it failed to decode json file" $
            forAll genLogJSONFilePath $ \path ->
                forAll (genLogFile Nothing) $ \file ->
                    monadicIO $ do
                        eLogLines <- run $ try $ extractMessages (toLogFile path file)

                        assert $ isLeft (eLogLines :: Either LogAnalysisException [Text])

    describe "extractIssuesFromLogs" $ modifyMaxSuccess (const 100) $ do
        it "should be able to catch an error text from json file" $
            forAll genErrorText $ \errorText ->
                forAll (genKnowledgeWithErrorText errorText) $ \knowledge ->
                    forAll genLogJSONFilePath $ \path ->
                        forAll (genJSONLogFile $ Just errorText) $ \file ->
                            monadicIO $ do
                                eAnalysisResult <- testExtractIssuesFromLogs
                                                       knowledge
                                                       (toLogFile path file)

                                assert $ isRight eAnalysisResult
                                whenRight eAnalysisResult $ \analysisResult ->
                                    (assert . not . null . Map.toList) analysisResult

        it "should be able to catch an error text from text log file" $
            forAll genErrorText $ \errorText ->
                forAll (genKnowledgeWithErrorText errorText) $ \knowledge ->
                    forAll genLogFilePath $ \path ->
                        forAll (genLogFile $ Just errorText) $ \file ->
                            monadicIO $ do
                                eAnalysisResult <- testExtractIssuesFromLogs
                                                       knowledge
                                                       (toLogFile path file)

                                assert $ isRight eAnalysisResult
                                whenRight eAnalysisResult $ \analysisResult ->
                                    (assert . not . null . Map.toList) analysisResult

        it "should throw an exception when no issue is found" $
            forAll (genKnowledgeWithErrorText "This should not be caught") $ \knowledge ->
                forAll genLogJSONFilePath $ \path ->
                    forAll (genJSONLogFile Nothing) $ \file ->
                        monadicIO $ do
                            eAnalysisResult <- testExtractIssuesFromLogs
                                                   knowledge
                                                   (toLogFile path file)

                            assert $ isLeft (eAnalysisResult :: Either LogAnalysisException Analysis)

-- | Generalized testing of extractIssuesFromLogs
testExtractIssuesFromLogs :: Knowledge
                          -> LogFile
                          -> PropertyM IO (Either LogAnalysisException Analysis)
testExtractIssuesFromLogs knowledge logFile = do
        let analysis = setupAnalysis [knowledge]
        run $ try $ extractIssuesFromLogs [logFile] analysis

-- | Generate random cardano json log line
genJSONLog :: Maybe Text -> Gen ByteString
genJSONLog mErrorText = do
    randomTime     <- arbitrary :: Gen UTCTime
    randomEnv      <- encodedElements ["mainnet_wallet_macos64:1.3.0"]
    randomNs       <- encodedElements ["cardano-sl", "NtpClient"]
    randomApp      <- encodedElements ["cardano-sl"]
    -- Check aeson can decode message with non-latin characters
    randomMsg      <- encodedElements
        [ "Error Message","Passive Wallet kernel initialized."
        , "Evaluated clock offset NtpOffset {getNtpOffset = 24688mcs}mcs"
        , "Blocks have been adopted: [aabbcddeeff112233, "
        , "Trying to apply blocks w/o rollback. First 3: "
        , "Verifying and applying blocks..."
        , "Verifying and applying blocks done"
        , "デンバー　コロラド", "暗号通貨", "カルダノ"]
    let message = maybe randomMsg encodeUtf8 mErrorText
    randomPid      <- arbitrary :: Gen Int
    randomHost     <- encodedElements ["hostname"]
    randomSev      <- encodedElements ["Info", "Warning", "Error", "Notice"]
    randomThreadId <- arbitrary :: Gen Int

    -- (TODO): Seems like some of the fields are going to be modified/deleted
    -- Update when needed
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
        \\"msg\": \""<> message <> "\",                              \
        \\"pid\": \""<> show randomPid <> "\",                       \
        \\"loc\": null,                                              \
        \\"host\": \""<> randomHost <> "\",                          \
        \\"sev\": \""<> randomSev <> "\",                            \
        \\"thread\": \"ThreadId " <> show randomThreadId <> "\"      \
        \}"
 where
    encodedElements :: [Text] -> Gen ByteString
    encodedElements xs = encodeUtf8 <$> elements xs
    -- | Formant given UTCTime into ISO8601
    showIso8601 :: UTCTime -> String
    showIso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

-- | Generate random cardano-log file path
-- sample: node.json-20180911134009
genLogJSONFilePath :: Gen FilePath
genLogJSONFilePath = do
    randomNum <- arbitrary :: Gen Int
    pure $ "node.json-" <> show randomNum

-- | Generate random text cardano-log file path
-- Sample: node-20180911134009, daedalus.log, launcher
genLogFilePath :: Gen FilePath
genLogFilePath = do
    randomNum <- arbitrary :: Gen Int
    elements ["node-" <> show randomNum, "daedalus.log", "launcher"]

-- | Generate error messages that classifier needs to catch
genErrorText :: Gen Text
genErrorText = elements
    [ "useless for the following reason"
    , "DBMalformed"
    , "signalProcess: permission denied (Operation not permitted"
    , "No such file or directory"
    , "resource exhausted (No space left on device)"
    , "returned empty list"
    , "irrelevant to given wallet"
    , "Network.Socket.recvBuf: resource vanished"
    , "IO error: Failed to create dir"
    , "open.lock: Locked by"
    , "Network.Socket.recvBuf: failed (No error)"
    ]

-- | Generate random knowledge with given text as `kErrorText`
genKnowledgeWithErrorText :: Text -> Gen Knowledge
genKnowledgeWithErrorText eText = do
    randomKnowledge <- arbitrary
    pure $ randomKnowledge {kErrorText = eText}

-- | Generate tuple of (FilePath, JSONFile)
-- with given errorText included as msg
genJSONLogFile :: Maybe Text -> Gen ByteString
genJSONLogFile mErrorText = do
    file <- genJSON

    case mErrorText of
        Just errorText -> do
            jsonWithError <- genJSONLog (Just errorText)
            let fileWithError = jsonWithError <> "\n" <> file
            pure fileWithError
        Nothing -> pure file
 where
    genJSON :: Gen ByteString
    genJSON = do
        numOfLines <- choose (1,1000)
        logLines   <- vectorOf numOfLines (genJSONLog Nothing)
        pure $ C8.unlines logLines

-- | Generate tuple of (FilePath, LogFile)
-- with given errorText included in the ByteString
genLogFile :: Maybe Text -> Gen ByteString
genLogFile mErrorText = do
    file <- genLog
    let errorText = maybe mempty encodeUtf8 mErrorText
    let fileWithError = errorText <> "\n" <> file
    pure fileWithError
  where
    genLog :: Gen ByteString
    genLog = do
        numOfLines <- choose (1,1000)
        logLines   <- vectorOf numOfLines (arbitrary :: Gen ByteString)
        pure $ C8.unlines logLines
