{-# OPTIONS_GHC -fno-warn-orphans #-}

module LogAnalysisSpec
    ( classifierSpec
    ) where

import           Universum

import           Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime (..), defaultTimeLocale, formatTime)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, arbitrary, choose, elements,
                                  forAll, vectorOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, run, PropertyM)

import           LogAnalysis.Classifier (extractIssuesFromLogs, extractMessages)
import           LogAnalysis.Types (CardanoLog, Knowledge (..), setupAnalysis, Analysis)
import           LogAnalysis.Exceptions (LogAnalysisException(..))

-- | Classifier tests
classifierSpec :: Spec
classifierSpec = do
    describe "CardanoLog FromJSON" $ modifyMaxSuccess (const 1000) $
       it "should be able to decode cardano log" $
           forAll (genLogText Nothing) $ \(logText :: ByteString) -> do
               let decodedText = eitherDecodeStrict' logText :: Either String CardanoLog
               isRight decodedText `shouldBe` True

    describe "extractMessages" $ modifyMaxSuccess (const 100) $ do
        it "should be able to extract log messages from JSON file" $
            forAll ((,) <$> genLogJSONFilePath <*> genJSONLogFile)
                $ \logWithFilePath -> testExtractMessage logWithFilePath

        it "should be able to extract log messages from plain log file" $
            forAll ((,) <$> genLogFilePath <*> genLogFile)
                $ \logWithFilePath -> testExtractMessage logWithFilePath

    describe "extractIssuesFromLogs" $ modifyMaxSuccess (const 100) $ do
        it "should be able to catch an error text from json file" $
            forAll genErrorText $ \errorText ->
                forAll (genKnowledgeWithErrorText errorText) $ \knowledge ->
                    forAll (genJSONWithError errorText) $ \jsonWithFilePath ->
                        monadicIO $ do
                            eAnalysisResult <- testExtractIssuesFromLogs knowledge jsonWithFilePath
   
                            assert $ isRight eAnalysisResult
                            whenRight eAnalysisResult $ \analysisResult ->
                                (assert . not . null . Map.toList) analysisResult

        it "should be able to catch an error text from plain log file" $
            forAll genErrorText $ \errorText ->
                forAll (genKnowledgeWithErrorText errorText) $ \knowledge ->
                    forAll (genLogWithError errorText) $ \logWithFilePath ->
                        monadicIO $ do
                         eAnalysisResult <- testExtractIssuesFromLogs knowledge logWithFilePath

                         assert $ isRight eAnalysisResult
                         whenRight eAnalysisResult $ \analysisResult ->
                            (assert . not . null . Map.toList) analysisResult

        it "should throw an exception when no issue is found" $
            forAll (genKnowledgeWithErrorText "This should not be caught") $ \knowledge ->
                forAll ((,) <$> genLogJSONFilePath <*> genJSONLogFile) $ \logWithFilePath ->
                    monadicIO $ do
                        eAnalysisResult <- testExtractIssuesFromLogs knowledge logWithFilePath

                        assert $ isLeft (eAnalysisResult :: Either LogAnalysisException Analysis)


-- | Generalized testing of extractMessage
testExtractMessage :: (FilePath, ByteString) -> Property
testExtractMessage logWithFilePath =
    monadicIO $ do
        eLogLines <- run $ tryAny $ extractMessages logWithFilePath
        -- Check if the decoding was successfull
        assert $ isRight eLogLines
        whenRight eLogLines $ \logLines ->
            (assert . not . null) logLines

-- | Generalized testing of extractIssuesFromLogs
testExtractIssuesFromLogs :: Knowledge 
                          -> (FilePath, ByteString)
                          -> PropertyM IO (Either LogAnalysisException Analysis)
testExtractIssuesFromLogs knowledge logWithFilePath = do 
        let analysis = setupAnalysis [knowledge]
        run $ try $ extractIssuesFromLogs [logWithFilePath] analysis

-- | Formant given UTCTime into ISO8601
showIso8601 :: UTCTime -> String
showIso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

-- | Given an list of text, pick and element and encode it
encodedElements :: [Text] -> Gen ByteString
encodedElements xs = encodeUtf8 <$> elements xs

-- | Generate random cardano log
genLogText :: Maybe Text -> Gen ByteString
genLogText mErrorText = do
    randomTime     <- arbitrary :: Gen UTCTime
    randomEnv      <- encodedElements ["mainnet_wallet_macos64:1.3.0"]
    randomNs       <- encodedElements ["cardano-sl", "NtpClient"]
    randomApp      <- encodedElements ["cardano-sl"]
    -- Ensure it can decode message with non-latin characters
    randomMsg      <- encodedElements
        [ "Error Message","Passive Wallet kernel initialized.", "暗号通貨", "カルダノ"
        , "Evaluated clock offset NtpOffset {getNtpOffset = 24688mcs}mcs"]
    let message = maybe randomMsg encodeUtf8 mErrorText
    randomPid      <- arbitrary :: Gen Int
    randomHost     <- encodedElements ["hostname"]
    randomSev      <- encodedElements ["Info", "Warning", "Error", "Notice"]
    randomThreadId <- arbitrary :: Gen Int

    -- (TODO): Seems like some of the fields are goingo to be deleted
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

-- | Generate random cardano-log file path
-- sample: node.json-20180911134009
genLogJSONFilePath :: Gen FilePath
genLogJSONFilePath = do
    randomNum <- arbitrary :: Gen Int
    pure $ "node.json-" <> show randomNum

-- | Generate random cardano-log json file
genJSONLogFile :: Gen ByteString
genJSONLogFile = do
    numOfLines <- choose (1,1000)
    logLines   <- vectorOf numOfLines (genLogText Nothing)
    pure $ C8.unlines logLines

-- | Generate random plain cardano-log file path
-- Sample: node-20180911134009, daedalus.log, launcher
genLogFilePath :: Gen FilePath
genLogFilePath = do
    randomNum <- arbitrary :: Gen Int
    elements ["node-" <> show randomNum, "daedalus.log", "launcher"]

-- | Generate random plain cardano-log file
genLogFile :: Gen ByteString
genLogFile = do
    numOfLines <- choose (1,1000)
    logLines   <- vectorOf numOfLines (arbitrary :: Gen ByteString)
    pure $ C8.unlines logLines

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
genJSONWithError :: Text -> Gen (FilePath, ByteString)
genJSONWithError errorText = do
    filepath      <- genLogJSONFilePath
    file          <- genJSONLogFile
    jsonWithError <- genLogText (Just errorText)

    let fileWithError = jsonWithError <> "\n" <> file
    pure (filepath, fileWithError)

-- | Generate tuple of (FilePath, LogFile) 
-- with given errorText included in the ByteString
genLogWithError :: Text -> Gen (FilePath, ByteString)
genLogWithError errorText = do
    filepath <- genLogFilePath
    file     <- genLogFile
    let fileWithError = encodeUtf8 errorText <> "\n" <> file
    pure (filepath, fileWithError)