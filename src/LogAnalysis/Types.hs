{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.Types
    ( Analysis
    , ErrorCode (..)
    , Knowledge (..)
    , setupAnalysis
    , renderErrorCode
    , toComment
    ) where

import           Universum

import qualified Data.Map.Strict as Map

-- | Identifier for each error
data ErrorCode
    = ShortStorage      -- ^ Not enough space on hard drive to store block data
    | UserNameError     -- ^ User is using non-latin characters for username
    | TimeSync          -- ^ User's PC's time is out of sync
    | FileNotFound      -- ^ Some of the files were not installed properly
    | StaleLockFile     -- ^ Open.lock file is corrupted
    | SentLogCorrupted  -- ^ Log file sent to the Zendesk is corrupted
    | DBError           -- ^ Local block data is corrupted
    | DBPath            -- ^ Daedalus cannot find certain files
    | CannotGetDBSize   -- ^ Error message of Couidn't pack log files shows up
    | BalanceError      -- ^ Daedalus shows wrong Ada amount
    | NetworkError      -- ^ Firewall is blocking the connection
    | ConnectionRefused -- ^ Firewall is blocking the connection
    | ResourceVanished  -- ^ Network error
    | Unknown           -- ^ Unknown error (currently not used)
    | Error             -- ^ Error (currently not used)
    deriving (Eq, Ord, Show)

-- | Record identifying the issue
data Knowledge = Knowledge
  {  kErrorText :: !LText   　-- ^ Text used for matching error lines
  ,  kErrorCode :: !ErrorCode -- ^ Identity for error code
  ,  kProblem   :: !LText   　-- ^ Text describing what is the problem
  ,  kSolution  :: !LText   　-- ^ Text describing how to solve the issue
  } deriving (Show)

renderErrorCode :: ErrorCode -> Text
renderErrorCode ShortStorage      = "short-storage"
renderErrorCode UserNameError     = "user-name-error"
renderErrorCode TimeSync          = "time-out-of-sync"
renderErrorCode FileNotFound      = "directory-not-found"
renderErrorCode StaleLockFile     = "stale-lock-file"
renderErrorCode SentLogCorrupted  = "sent-log-corrupted"
renderErrorCode DBError           = "DB-corrupted"
renderErrorCode DBPath            = "DB-path-error"
renderErrorCode CannotGetDBSize   = "cannot-get-db-size"
renderErrorCode BalanceError      = "incorrect-balance"
renderErrorCode NetworkError      = "network-error"
renderErrorCode ConnectionRefused = "connection-refused"
renderErrorCode ResourceVanished  = "resource-vanished"
renderErrorCode Unknown           = "unknown"
renderErrorCode Error             = "error"

toComment :: ErrorCode -> Text
toComment SentLogCorrupted = "Log file is corrupted"
toComment _                = "Error"

-- | Map used to collect error lines
type Analysis = Map Knowledge [LText]

-- | Create initial analysis environment
setupAnalysis :: [Knowledge] -> Analysis
setupAnalysis kbase = Map.fromList $ map (\kn -> (kn, [])) kbase

instance Eq Knowledge where
    e1 == e2 = kErrorCode e1 == kErrorCode e2

instance Ord Knowledge where
    e1 <= e2 = kErrorCode e1 <= kErrorCode e2
