{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.Types
       ( Analysis
       , CardanoLog (..)
       , ErrorCode (..)
       , Knowledge (..)
       , setupAnalysis
       , renderErrorCode
       , toComment
       ) where

import           Prelude (Show (..))
import           Universum

import           Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime)
import           Test.QuickCheck

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
    , clHost        :: !Text
    -- ^ Hostname
    , clSeverity    :: !Text
    -- ^ Severity of an given log (e.g Info, Notice, Warning, Error)
    , clThreadId    :: !Text
    -- ^ Thread id
    } deriving (Show)

instance FromJSON CardanoLog where
    parseJSON = withObject "Cardano node log" $ \o -> do
        loggedAt    <- o .: "at"
        env         <- o .: "env"
        ns          <- o .: "ns"
        application <- o .: "app"
        message     <- o .: "msg"
        pid         <- o .: "pid"
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
            , clHost        = host
            , clSeverity    = severity
            , clThreadId    = threadId
            }

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