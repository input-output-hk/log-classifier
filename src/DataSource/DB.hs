module DataSource.DB
    (
    ) where

import           Universum hiding (All)

import           Control.Applicative
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import           Data.Text (split)

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Types
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField

import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.ToField


import           DataSource.Types


-- https://ocharles.org.uk/blog/posts/2014-08-07-postgresql-simple-generic-sop.html

-- | The cached Zendesk layer. We are using a simple SQLite DB behind the scenes
-- that we need to sync occasionaly.
cachedZendeskLayer :: (MonadIO m, MonadReader Config m) => ZendeskLayer m
cachedZendeskLayer = ZendeskLayer
    { zlGetTicketInfo           = liftIO . getTicketInfoByTicketId
    , zlListAssignedTickets     = liftIO . getAllAssignedTicketsByUser
    , zlListRequestedTickets    = liftIO . getAllRequestedTicketsByUser
    , zlPostTicketComment       = error "We won't use this for now!"
    , zlGetAttachment           = error "!"
    , zlGetTicketComments       = error "!"
    }

-- Database instances
-- FROM

instance FromField TicketId where
    fromField (Field (SQLInteger tId) _)    = Ok . TicketId . fromIntegral $ tId
    fromField f                             = returnError ConversionFailed f "need a integer, ticket id"

instance FromField UserId where
    fromField (Field (SQLInteger uId) _)    = Ok . UserId . fromIntegral $ uId
    fromField f                             = returnError ConversionFailed f "need a integer, user id"

instance FromField TicketURL where
    fromField (Field (SQLText tURL) _)      = Ok . TicketURL $ tURL
    fromField f                             = returnError ConversionFailed f "need a text, ticket url"

-- | TODO(ks): Yes, yes, normal form...
instance FromField TicketTags where
    fromField (Field (SQLText tTags) _)     = Ok . TicketTags . split (==',') $ tTags
    fromField f                             = returnError ConversionFailed f "need a text, ticket tags"

instance FromField TicketStatus where
    fromField (Field (SQLText tStat) _)     = Ok . TicketStatus $ tStat
    fromField f                             = returnError ConversionFailed f "need a text, ticket status"

instance FromRow TicketInfo where
    fromRow = TicketInfo <$> field <*> field <*> field <*> field <*> field <*> field

-- TO

instance ToField TicketId where
    toField (TicketId tId)                  = SQLInteger . fromIntegral $ tId

instance ToField UserId where
    toField (UserId userId)                 = SQLInteger . fromIntegral $ userId

-- | A general resource closing function.
withDatabase :: forall a. String -> (Connection -> IO a) -> IO a
withDatabase dbName dbOperation = do
    connection  <- open dbName
    result      <- dbOperation connection
    close connection
    pure result

-- | A production resource closing function.
withProdDatabase :: forall a. (Connection -> IO a) -> IO a
withProdDatabase = withDatabase "prod.db"

getTicketsInfo :: IO [TicketInfo]
getTicketsInfo = withProdDatabase $ \conn ->
    query_ conn "SELECT * FROM ticket_info"

getTicketInfoByTicketId :: TicketId -> IO (Maybe TicketInfo)
getTicketInfoByTicketId ticketId = withProdDatabase $ \conn ->
    safeHead <$> queryNamed conn "SELECT * FROM ticket_info WHERE ticket_id = :id" [":id" := ticketId]

getAllAssignedTicketsByUser :: UserId -> IO [TicketInfo]
getAllAssignedTicketsByUser userId = withProdDatabase $ \conn ->
    queryNamed conn "SELECT * FROM ticket_info WHERE assignee_id = :id" [":id" := userId]

getAllRequestedTicketsByUser :: UserId -> IO [TicketInfo]
getAllRequestedTicketsByUser userId = withProdDatabase $ \conn ->
    queryNamed conn "SELECT * FROM ticket_info WHERE requester_id = :id" [":id" := userId]


