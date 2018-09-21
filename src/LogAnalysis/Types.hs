{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.Types
       ( Analysis
       , CardanoLog (..)
       , ErrorCode (..)
       , Knowledge (..)
       , FileFormat(..)
       , LogFile(..)
       , toLogFile
       , setupAnalysis
       , renderErrorCode
       , toComment
       ) where

import           Prelude (Show (..))
import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import           Test.QuickCheck (Arbitrary (..), Gen, choose, elements, sublistOf)

-- | Identifier for each error
data ErrorCode
    = ShortStorage         -- ^ Not enough space on hard drive to store block data
    | UserNameError        -- ^ User is using non-latin characters for username
    | TimeSync             -- ^ User's PC's time is out of sync
    | FileNotFound         -- ^ Some of the files were not installed properly
    | StaleLockFile        -- ^ Open.lock file is corrupted
    | SentLogCorrupted     -- ^ Log file sent to the Zendesk is corrupted
    | DBError              -- ^ Local block data is corrupted
    | DBPath               -- ^ Daedalus cannot find certain files
    | CannotGetDBSize      -- ^ Error message of Couidn't pack log files shows up
    | BalanceError         -- ^ Daedalus shows wrong Ada amount
    | NetworkError         -- ^ Firewall is blocking the connection
    | ConnectionRefused    -- ^ Firewall is blocking the connection
    | ResourceVanished     -- ^ Network error
    | DecompressionFailure -- ^ The classifier failed to decompress the log file
    | Unknown              -- ^ Unknown error (currently not used)
    | Error                -- ^ Error (currently not used)
    deriving (Eq, Ord, Show, Enum)

-- | Record identifying the issue
data Knowledge = Knowledge
    {  kErrorText :: !Text
    -- ^ Text used for matching error lines
    ,  kErrorCode :: !ErrorCode
    -- ^ Identity for error code
    ,  kProblem   :: !Text
    -- ^ Text describing what is the problem
    ,  kSolution  :: !Text
    -- ^ Text describing how to solve the issue
    ,  kFAQNumber :: !Text
    -- ^ The FAQ number that will be displayed on the official Cardano FAQ page
    }

-- | File format
data FileFormat
    = Txt
    | JSON
    deriving (Eq, Show)

-- | Logfile data type
data LogFile = LogFile
    { lfFileFormat :: !FileFormat
    -- ^ File format of a log file
    , lfContent    :: !ByteString
    -- ^ Log file
    } deriving (Eq, Show)

instance Show Knowledge where
    show Knowledge{..} =
        "{  errorText  = " <> Prelude.show kErrorText   <>
        ",  errorCode  = " <> Prelude.show kErrorCode   <>
        ",  problem    = " <> Prelude.show kProblem     <>
        ",  solution   = " <> Prelude.show kSolution    <>
        ",  FAQNumber  = " <> Prelude.show kFAQNumber   <>
        "}"

-- | Sorted accoring to knowledgebase.
-- Tag needs to be in lowercase since Zendesk automatically convert any uppercase
-- lowercase
renderErrorCode :: ErrorCode -> Text
renderErrorCode DBError              = "db-corrupted"
renderErrorCode StaleLockFile        = "stale-lock-file"
renderErrorCode FileNotFound         = "directory-not-found"
renderErrorCode ShortStorage         = "short-storage"
renderErrorCode NetworkError         = "network-error"
renderErrorCode BalanceError         = "incorrect-balance"
renderErrorCode ResourceVanished     = "resource-vanished"
renderErrorCode UserNameError        = "user-name-error"
renderErrorCode ConnectionRefused    = "connection-refused"
renderErrorCode TimeSync             = "time-out-of-sync"
renderErrorCode SentLogCorrupted     = "sent-log-corrupted"
renderErrorCode DBPath               = "db-path-error"
renderErrorCode CannotGetDBSize      = "cannot-get-db-size"
renderErrorCode DecompressionFailure = "decompression-failure"
renderErrorCode Unknown              = "unknown"
renderErrorCode Error                = "error"

toComment :: ErrorCode -> Text
toComment SentLogCorrupted = "Log file is corrupted"
toComment _                = "Error"

-- | Convert tuple of (FilePath, ByteString) into LogFile
toLogFile :: FilePath -> ByteString -> LogFile
toLogFile path content =
  let format = if ".json" `T.isInfixOf` toText path
               then JSON
               else Txt
  in LogFile format content

-- | Map used to collect error lines
type Analysis = Map Knowledge [Text]

-- | Create initial analysis environment
setupAnalysis :: [Knowledge] -> Analysis
setupAnalysis kbase = Map.fromList $ map (\kn -> (kn, [])) kbase

instance Eq Knowledge where
    e1 == e2 = kErrorCode e1 == kErrorCode e2

instance Ord Knowledge where
    e1 <= e2 = kErrorCode e1 <= kErrorCode e2

-- | Cardano log data type
data CardanoLog = CardanoLog {
      clLoggedAt    :: !UTCTime
    -- ^ UTCTime of when the message was logged
    , clEnv         :: !Text
    -- ^ Environment
    , clNs          :: ![Text]
    -- ^ NS
    , clApplication :: ![Text]
    -- ^ Application name
    , clMessage     :: !Text
    -- ^ Log message
    , clPid         :: !Text
    -- ^ Process Id
    , clLoc         :: !(Maybe Text)
    -- ^ Loc
    , clHost        :: !Text
    -- ^ Hostname
    , clSeverity    :: !Text
    -- ^ Severity of an given log (e.g Info, Notice, Warning, Error)
    , clThreadId    :: !Text
    -- ^ Thread id
    } deriving (Eq, Show)

instance FromJSON CardanoLog where
    parseJSON = withObject "CardanoLog" $ \o -> do
        loggedAt    <- o .: "at"
        env         <- o .: "env"
        ns          <- o .: "ns"
        application <- o .: "app"
        message     <- o .: "msg"
        pid         <- o .: "pid"
        loc         <- o .: "loc"
        host        <- o .: "host"
        severity    <- o .: "sev"
        threadId    <- o .: "thread"

        pure CardanoLog {
              clLoggedAt    = loggedAt
            , clEnv         = env
            , clNs          = ns
            , clApplication = application
            , clMessage     = message
            , clPid         = pid
            , clLoc         = loc
            , clHost        = host
            , clSeverity    = severity
            , clThreadId    = threadId
            }

instance ToJSON CardanoLog where
    toJSON CardanoLog{..} =
        object [ "at"     .= clLoggedAt
               , "env"    .= clEnv
               , "ns"     .= clNs
               , "app"    .= clApplication
               , "msg"    .= clMessage
               , "pid"    .= clPid
               , "loc"    .= clLoc
               , "host"   .= clHost
               , "sev"    .= clSeverity
               , "thread" .= clThreadId
               ]
--------------------------------------------------------------------------------
-- Arbitrary instance
--------------------------------------------------------------------------------

instance Arbitrary ErrorCode where
    arbitrary = elements [ShortStorage .. Error]

instance Arbitrary Knowledge where
    arbitrary = do
        errorText <- fromString <$> arbitrary
        errorCode <- arbitrary
        problem   <- fromString <$> arbitrary
        solution  <- fromString <$> arbitrary
        faqNumber <- fromString <$> arbitrary

        pure $ Knowledge {
              kErrorText = errorText
            , kErrorCode = errorCode
            , kProblem   = problem
            , kSolution  = solution
            , kFAQNumber = faqNumber
            }

instance Arbitrary Text where
    arbitrary = fromString <$> arbitrary

-- https://gist.github.com/agrafix/2b48ec069693e3ab851e
instance Arbitrary UTCTime where
    arbitrary = do
        randomDay   <- choose (1, 28) :: Gen Int
        randomMonth <- choose (1, 12) :: Gen Int
        randomYear  <- choose (2001, 2018) :: Gen Integer
        randomTime  <- choose (0, 86301) :: Gen Int64
        pure $ UTCTime
            (fromGregorian randomYear randomMonth randomDay)
            (secondsToDiffTime $ fromIntegral randomTime)

instance Arbitrary CardanoLog where
    arbitrary = do
        loggedAt <- arbitrary
        env      <- elements [ "mainnet_wallet_macos64:1.3.0"
                             , "mainnet_wallet_windows64:1.3.0"]
        ns       <- sublistOf ["cardano-sl", "NtpClient"]
        app      <- return    ["cardano-sl"]
        msg      <- arbitrary
        pid      <- Universum.show <$> (arbitrary :: Gen Integer)
        loc      <- arbitrary
        host     <- arbitrary
        sev      <- elements ["Info", "Warning", "Error", "Notice"]
        threadId <- arbitrary :: Gen Int

        pure $ CardanoLog {
              clLoggedAt    = loggedAt
            , clEnv         = env
            , clNs          = ns
            , clApplication = app
            , clMessage     = msg
            , clPid         = pid
            , clLoc         = loc
            , clHost        = host
            , clSeverity    = sev
            , clThreadId    = "ThreadId-" <> Universum.show threadId
            }