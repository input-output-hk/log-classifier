{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LogAnalysis.Types
       ( Analysis
       , CardanoLog (..)
       , ErrorCode (..)
       , Knowledge (..)
       , LogFile (..) -- we don't need to export this
       , CoordinatedUniversalTime (..)
       , getLogFileContent
       , toLogFile
       , setupAnalysis
       , renderErrorCode
       , isJSONFormat
       , isTxtFormat
       ) where

import           Prelude (Show (..))
import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import           Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof, sublistOf)

-- | Identifier for each error
data ErrorCode
    = IOHKS_29 --BlockDataCorrupt
    | IOHKS_44 --BalanceError         -- ^ Daedalus shows wrong Ada amount
    | IOHKS_79 --CannotConnect
    | IOHKS_78 --CannotConnectAfter
    | IOHKS_4 --CannotGetDBSize      -- ^ Error message of Couidn't pack log files shows up
    | IOHKS_43 --ConnectionRefused
    | IOHKS_10 --ConnectLoadHeaders
    | IOHKS_47 --DBCorruptIO
    | IOHKS_41 --DBError
    | IOHKS_32 --DBPath               -- ^ Daedalus cannot find certain files
    | IOHKS_35 --FileNotFound
    | IOHKS_36 --NetworkError
    | IOHKS_39 --OpenLock
    | IOHKS_30 --PermCreateFile
    | IOHKS_7 --PermDenied
    | IOHKS_12 --ResourceVanished
    | IOHKS_45 --ShortStorage
    | IOHKS_48 --StaleLockFile
    | IOHKS_8 --TimeSync
    | IOHKS_37 --TLSCert
    | IOHKS_31 --WalletNotSync
    | IOHKS_65 --WinReg
    | SentLogCorrupted     -- ^ Log file sent to the Zendesk is corrupted
    | NoKnownIssue
    | Unknown              -- ^ Unknown error (currently not used)
    | Error                -- ^ Error (currently not used)
    deriving (Eq, Ord, Show, Enum)

-- | Record identifying the issue
data Knowledge = Knowledge
    {  kErrorText :: !Text
    -- ^ Text used for matching error lines
    ,  kIssue     :: !Text
    -- ^ Text that refers to the IOHKS issue
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
data LogFile
    = TxtFormat     !ByteString
    -- ^ Text file
    | JSONFormat    !ByteString
    -- ^ JSON file
    deriving (Eq, Show)

-- | Get content from either.
getLogFileContent :: LogFile -> ByteString
getLogFileContent = \case
    TxtFormat  content      -> content
    JSONFormat content      -> content

-- | Determine whether given 'LogFile' is an 'TxtFormat'
isJSONFormat :: LogFile -> Bool
isJSONFormat = \case
   JSONFormat _ -> True
   _            -> False

-- | Determine whether given 'LogFile' is an 'TxtFormat'
-- Right now, this is equivalent to  @not . isJSONFormat@ but we might be supporting
-- other formats as well so I've implemented like below.
isTxtFormat :: LogFile -> Bool
isTxtFormat = \case
   TxtFormat _ -> True
   _           -> False

instance Show Knowledge where
    show Knowledge{..} =
        "{  errorText  = " <> Prelude.show kErrorText   <>
        ",  issue      = " <> Prelude.show kIssue       <>
        ",  errorCode  = " <> Prelude.show kErrorCode   <>
        ",  problem    = " <> Prelude.show kProblem     <>
        ",  solution   = " <> Prelude.show kSolution    <>
        ",  FAQNumber  = " <> Prelude.show kFAQNumber   <>
        "}"

-- | Sorted accoring to knowledgebase.
-- Tag needs to be in lowercase since Zendesk automatically convert any uppercase
-- lowercase

renderErrorCode :: ErrorCode -> Text
renderErrorCode IOHKS_37         = "tls-cert-error"
renderErrorCode IOHKS_65         = "win-reg-error"
renderErrorCode IOHKS_39         = "open-lock"
renderErrorCode IOHKS_31         = "wallet-not-sync"
renderErrorCode IOHKS_30         = "permission-create-file"
renderErrorCode IOHKS_10         = "connect-load-headers"
renderErrorCode IOHKS_7          = "permission-denied"
renderErrorCode IOHKS_47         = "db-corrupt-io"
renderErrorCode IOHKS_29         = "db-corrupt"
renderErrorCode IOHKS_78         = "cannot-connect-after"
renderErrorCode IOHKS_79         = "cannot-connect"
renderErrorCode IOHKS_4          = "cannot-get-db-size"
renderErrorCode IOHKS_35         = "directory-not-found"
renderErrorCode IOHKS_41         = "db-corrupted"
renderErrorCode IOHKS_32         = "db-path-error"
renderErrorCode IOHKS_44         = "incorrect-balance"
renderErrorCode IOHKS_43         = "connection-refused"
renderErrorCode IOHKS_45         = "short-storage"
renderErrorCode IOHKS_48         = "stale-lock-file"
renderErrorCode IOHKS_12         = "resource-vanished"
renderErrorCode IOHKS_8          = "time-out-of-sync"
renderErrorCode IOHKS_36         = "network-error"
renderErrorCode SentLogCorrupted = "sent-log-corrupted"
renderErrorCode NoKnownIssue     = "no-known-issue"
renderErrorCode Unknown          = "unknown"
renderErrorCode Error            = "error"

-- | Make LogFile with given Filepath and ByteString
toLogFile :: FilePath -> ByteString -> LogFile
toLogFile path content =
    if ".json" `T.isInfixOf` toText path
        then JSONFormat content
        else TxtFormat content

-- | Map used to collect error lines
type Analysis = Map Knowledge [Text]

-- | Create initial analysis environment
setupAnalysis :: [Knowledge] -> Analysis
setupAnalysis kbase = Map.fromList $ map (\kn -> (kn, [])) kbase

instance Eq Knowledge where
    e1 == e2 = kIssue e1 == kIssue e2

instance Ord Knowledge where
    e1 <= e2 = kIssue e1 <= kIssue e2

-- | Cardano log data type
data CardanoLog = CardanoLog {
      clLoggedAt    :: !CoordinatedUniversalTime
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
    , clHost        :: !(Maybe Text)
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
    arbitrary = elements [IOHKS_45 .. Error]

instance Arbitrary Knowledge where
    arbitrary = do
        errorText <- fromString <$> arbitrary
        issue <- fromString <$> arbitrary
        errorCode <- arbitrary
        problem   <- fromString <$> arbitrary
        solution  <- fromString <$> arbitrary
        faqNumber <- fromString <$> arbitrary

        pure $ Knowledge {
              kErrorText = errorText
            , kIssue = issue
            , kErrorCode = errorCode
            , kProblem   = problem
            , kSolution  = solution
            , kFAQNumber = faqNumber
            }

{-

λ> import LogAnalysis.Types
λ> decode (encode ["2018-09-11T17:08:31.002287Z"]) :: Maybe [CoordinatedUniversalTime]

Just [CoordinatedUniversalTime {getCoordinatedUniversalTime = 2018-09-11 17:08:31.002287 UTC}]

-}

-- | We wrap this into a newtype, since we can change the format more easily in the future.
newtype CoordinatedUniversalTime
    = CoordinatedUniversalTime { getCoordinatedUniversalTime :: UTCTime }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- https://gist.github.com/agrafix/2b48ec069693e3ab851e
instance Arbitrary CoordinatedUniversalTime where
    arbitrary = do
        randomDay   <- choose (1, 28)       :: Gen Int
        randomMonth <- choose (1, 12)       :: Gen Int
        randomYear  <- choose (2001, 2018)  :: Gen Integer
        randomTime  <- choose (0, 86301)    :: Gen Int64
        pure . CoordinatedUniversalTime $ UTCTime
            (fromGregorian randomYear randomMonth randomDay)
            (secondsToDiffTime $ fromIntegral randomTime)

instance Arbitrary CardanoLog where
    arbitrary = do
        loggedAt <- arbitrary
        env      <- elements [ "mainnet_wallet_macos64:1.3.0"
                             , "mainnet_wallet_windows64:1.3.0"]
        ns       <- sublistOf ["cardano-sl", "NtpClient"]
        app      <- return    ["cardano-sl"]
        msg      <- arbitraryText
        pid      <- Universum.show <$> arbitrary @Integer
        loc      <- arbitraryMaybeText
        host     <- arbitraryMaybeText
        sev      <- elements ["Info", "Warning", "Error", "Notice"]
        threadId <- arbitrary @Int

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
      where
        arbitraryText :: Gen Text
        arbitraryText = fromString <$> arbitrary @String

        arbitraryMaybeText :: Gen (Maybe Text)
        arbitraryMaybeText = oneof
            [ pure Nothing
            , Just <$> arbitraryText
            ]

