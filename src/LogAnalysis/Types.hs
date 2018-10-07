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
    = BlockDataCorrupt
    | BalanceError         -- ^ Daedalus shows wrong Ada amount
    | CannotConnect
    | CannotConnectAfter
    | CannotGetDBSize      -- ^ Error message of Couidn't pack log files shows up
    | ConnectionRefused
    | ConnectLoadHeaders
    | DBCorruptIO
    | DBError
    | DBPath               -- ^ Daedalus cannot find certain files
    | FileNotFound
    | NetworkError
    | OpenLock
    | PermCreateFile
    | PermDenied
    | ResourceVanished
    | ShortStorage
    | StaleLockFile
    | TimeSync
    | TLSCert
    | UserNameError        -- ^ User is using non-latin characters for username
    | WalletNotSync
    | WinReg
    | SentLogCorrupted     -- ^ Log file sent to the Zendesk is corrupted
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
        ",  errorCode  = " <> Prelude.show kErrorCode   <>
        ",  problem    = " <> Prelude.show kProblem     <>
        ",  solution   = " <> Prelude.show kSolution    <>
        ",  FAQNumber  = " <> Prelude.show kFAQNumber   <>
        "}"

-- | Sorted accoring to knowledgebase.
-- Tag needs to be in lowercase since Zendesk automatically convert any uppercase
-- lowercase

renderErrorCode :: ErrorCode -> Text
renderErrorCode TLSCert              = "tls-cert-error"
renderErrorCode WinReg               = "win-reg-error"
renderErrorCode OpenLock             = "open-lock"
renderErrorCode WalletNotSync        = "wallet-not-sync"
renderErrorCode PermCreateFile       = "permission-create-file"
renderErrorCode ConnectLoadHeaders   = "connect-load-headers"
renderErrorCode PermDenied           = "permission-denied"
renderErrorCode DBCorruptIO          = "db-corrupt-io"
renderErrorCode BlockDataCorrupt     = "db-corrupt"
renderErrorCode CannotConnectAfter   = "cannot-connect-after"
renderErrorCode CannotConnect        = "cannot-connect"
renderErrorCode CannotGetDBSize      = "cannot-get-db-size"
renderErrorCode FileNotFound         = "directory-not-found"
renderErrorCode DBError              = "db-corrupted"
renderErrorCode DBPath               = "db-path-error"
renderErrorCode BalanceError         = "incorrect-balance"
renderErrorCode ConnectionRefused    = "connection-refused"
renderErrorCode ShortStorage         = "short-storage"
renderErrorCode StaleLockFile        = "stale-lock-file"
renderErrorCode ResourceVanished     = "resource-vanished"
renderErrorCode TimeSync             = "time-out-of-sync"
renderErrorCode NetworkError         = "network-error"
renderErrorCode UserNameError        = "user-name-error"
renderErrorCode SentLogCorrupted     = "sent-log-corrupted"
renderErrorCode DecompressionFailure = "decompression-failure"
renderErrorCode Unknown              = "unknown"
renderErrorCode Error                = "error"

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
    e1 == e2 = kErrorCode e1 == kErrorCode e2

instance Ord Knowledge where
    e1 <= e2 = kErrorCode e1 <= kErrorCode e2

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

