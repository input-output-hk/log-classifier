{-# OPTIONS_GHC -fno-warn-orphans #-}
-- The idea behind this module and doing orphan instances is that
-- if we ever want to switch to another implementation, we should
-- just remove this module, not rewrite all the types and change instances.
-- This way, we have real separation.

module DataSource.DB
    ( DBConnPool
    , withDatabase
    , withProdDatabase
    -- * Empty layer
    , emptyDBLayer
    -- * Single connection
    , connDataLayer
    , connPoolDataLayer
    -- * Connection pool
    , connDBLayer
    , connPoolDBLayer
    -- * Create connection pool
    , createProdConnectionPool
    ) where

import           Universum

import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Pool (Pool, createPool, withResource)
import           Data.Text (split)

import           Database.SQLite.Simple (FromRow (..), NamedParam (..), SQLData (..), close,
                                         executeNamed, execute_, field, open, queryNamed, query_)
import           Database.SQLite.Simple.FromField (FromField (..), ResultError (..), returnError)
import           Database.SQLite.Simple.Internal (Connection, Field (..))
import           Database.SQLite.Simple.Ok (Ok (..))
import           Database.SQLite.Simple.ToField (ToField (..))

import           DataSource.Http (basicDataLayer)
import           DataSource.Types (Attachment (..), AttachmentContent (..), AttachmentId (..),
                                   Comment (..), CommentBody (..), CommentId (..), Config,
                                   DBLayer (..), TicketField (..), TicketFieldId (..),
                                   TicketFieldValue (..), TicketId (..), TicketInfo (..),
                                   TicketStatus (..), TicketTags (..), TicketURL (..), UserId (..),
                                   DataLayer (..))

------------------------------------------------------------
-- Single connection, simple
------------------------------------------------------------

-- | A general resource closing function.
-- The issue with this is that we currently can't use any concurrency
-- primitives, but that will be fixed in the future.
withDatabase :: forall a. String -> (Connection -> IO a) -> IO a
withDatabase dbName dbOperation =
    bracket
        (open dbName)
        (close)
        dbOperation

-- | A production resource closing function.
withProdDatabase :: forall m a. (MonadIO m) => (Connection -> IO a) -> m a
withProdDatabase = liftIO . withDatabase "./prod.db"

------------------------------------------------------------
-- Connection pool
------------------------------------------------------------

-- Export it, hide it inside the module so
-- we have control over it.
newtype DBConnPool = DBConnPool
    { getDBConnectionPool :: Pool Connection
    }

-- | The connection pool to enable concurrent work.
-- TODO(ks): Newtype wrappers if we export.
createConnectionPool :: forall m. (MonadIO m) => String -> Int -> m DBConnPool
createConnectionPool dbName simulConns =
    liftIO $ DBConnPool <$> createPool newConn delConn 1 10 simulConns
  where
    newConn :: IO Connection
    newConn = open dbName

    delConn :: Connection -> IO ()
    delConn = close

-- | Create production connection pool. By default a 100 connections.
createProdConnectionPool :: forall m. (MonadIO m) => m DBConnPool
createProdConnectionPool = createConnectionPool "prod.db" 100

-- | A utility function for unwrapping the connections.
withConnPool :: forall m a. (MonadBaseControl IO m) => DBConnPool -> (Connection -> m a) -> m a
withConnPool dbConnPool dbFunc =
    withResource (getDBConnectionPool dbConnPool) dbFunc

------------------------------------------------------------
-- Empty layer
------------------------------------------------------------

emptyDBLayer :: forall m. (Monad m) => DBLayer m
emptyDBLayer = DBLayer
    { dlInsertTicketInfo          = \_     -> pure ()
    , dlInsertTicketComments      = \_ _   -> pure ()
    , dlInsertCommentAttachments  = \_ _   -> pure ()
    , dlDeleteCommentAttachments  = pure ()
    , dlDeleteTicketComments      = pure ()
    , dlDeleteTickets             = pure ()
    , dlDeleteAllData             = pure ()
    , dlCreateSchema              = pure ()
    }

------------------------------------------------------------
-- Simple connection layer
------------------------------------------------------------

-- | The simple connection Zendesk layer. Used for database querying.
-- We need to sync occasionaly.
connDataLayer :: forall m. (MonadIO m, MonadReader Config m) => DataLayer m
connDataLayer = DataLayer
    { zlGetTicketInfo           = \tId -> withProdDatabase $ \conn -> getTicketInfoByTicketId conn tId
    , zlListDeletedTickets      = zlListDeletedTickets basicDataLayer
    , zlListAssignedTickets     = \uId -> withProdDatabase $ \conn -> getAllAssignedTicketsByUser conn uId
    , zlListRequestedTickets    = \uId -> withProdDatabase $ \conn -> getAllRequestedTicketsByUser conn uId
    , zlListUnassignedTickets   =         withProdDatabase getAllUnassignedTicketsByUser
    , zlListAdminAgents         = zlListAdminAgents basicDataLayer
    , zlGetAttachment           = \att -> withProdDatabase $ \conn -> DataSource.DB.getAttachmentContent conn att
    , zlGetTicketComments       = \tId -> withProdDatabase $ \conn -> getTicketComments conn tId
    , zlPostTicketComment       = zlPostTicketComment basicDataLayer
    , zlExportTickets           = zlExportTickets basicDataLayer
    }


-- | The simple connection database layer. Used for database modification.
connDBLayer :: forall m. (MonadIO m, MonadReader Config m) => DBLayer m
connDBLayer = DBLayer
    { dlInsertTicketInfo          = \tIn        -> withProdDatabase $ \conn -> insertTicketInfo conn tIn
    , dlInsertTicketComments      = \tId comm   -> withProdDatabase $ \conn -> insertTicketComments conn tId comm
    , dlInsertCommentAttachments  = \comm att   -> withProdDatabase $ \conn -> insertCommentAttachments conn comm att
    , dlDeleteCommentAttachments  =                withProdDatabase $ \conn -> deleteCommentAttachments conn
    , dlDeleteTicketComments      =                withProdDatabase $ \conn -> deleteTicketComments conn
    , dlDeleteTickets             =                withProdDatabase $ \conn -> deleteTickets conn
    , dlDeleteAllData             =                withProdDatabase $ \conn -> deleteAllData conn
    , dlCreateSchema              =                withProdDatabase $ \conn -> createSchema conn
    }

------------------------------------------------------------
-- Connection pool layer
------------------------------------------------------------

-- | The connection pooled Zendesk layer. Used for database querying.
-- We need to sync occasionaly.
connPoolDataLayer :: forall m. (MonadBaseControl IO m, MonadIO m, MonadReader Config m) => DBConnPool -> DataLayer m
connPoolDataLayer connPool = DataLayer
    { zlGetTicketInfo           = \tId -> withConnPool connPool $ \conn -> getTicketInfoByTicketId conn tId
    , zlListDeletedTickets      = zlListDeletedTickets basicDataLayer
    , zlListAssignedTickets     = \uId -> withConnPool connPool $ \conn -> getAllAssignedTicketsByUser conn uId
    , zlListRequestedTickets    = \uId -> withConnPool connPool $ \conn -> getAllRequestedTicketsByUser conn uId
    , zlListUnassignedTickets   =         withConnPool connPool getAllUnassignedTicketsByUser
    , zlListAdminAgents         = zlListAdminAgents basicDataLayer
    , zlGetAttachment           = \att -> withConnPool connPool $ \conn -> DataSource.DB.getAttachmentContent conn att
    , zlGetTicketComments       = \tId -> withConnPool connPool $ \conn -> getTicketComments conn tId
    , zlPostTicketComment       = zlPostTicketComment basicDataLayer
    , zlExportTickets           = zlExportTickets basicDataLayer
    }


-- | The connection pooled database layer. Used for database modification.
connPoolDBLayer :: forall m. (MonadBaseControl IO m, MonadIO m, MonadReader Config m) => DBConnPool -> DBLayer m
connPoolDBLayer connPool = DBLayer
    { dlInsertTicketInfo          = \tIn        -> withConnPool connPool $ \conn -> insertTicketInfo conn tIn
    , dlInsertTicketComments      = \tId comm   -> withConnPool connPool $ \conn -> insertTicketComments conn tId comm
    , dlInsertCommentAttachments  = \comm att   -> withConnPool connPool $ \conn -> insertCommentAttachments conn comm att
    , dlDeleteCommentAttachments  =                withConnPool connPool $ \conn -> deleteCommentAttachments conn
    , dlDeleteTicketComments      =                withConnPool connPool $ \conn -> deleteTicketComments conn
    , dlDeleteTickets             =                withConnPool connPool $ \conn -> deleteTickets conn
    , dlDeleteAllData             =                withConnPool connPool $ \conn -> deleteAllData conn
    , dlCreateSchema              =                withConnPool connPool $ \conn -> createSchema conn
    }

------------------------------------------------------------
-- Database instances
------------------------------------------------------------

-- https://ocharles.org.uk/blog/posts/2014-08-07-postgresql-simple-generic-sop.html

instance FromField TicketFieldId where
    fromField (Field (SQLInteger tfId) _)   = Ok . TicketFieldId . fromIntegral $ tfId
    fromField f                             = returnError ConversionFailed f "need a text, ticket status"

instance FromField TicketFieldValue where
    fromField (Field (SQLText tfValue) _)   = Ok . TicketFieldValue $ tfValue
    fromField f                             = returnError ConversionFailed f "need a text, ticket status"

-- TODO(ks): Separate table!
instance FromField [TicketField] where
    fromField _                             = empty

instance FromField TicketId where
    fromField (Field (SQLInteger tId) _)    = Ok . TicketId . fromIntegral $ tId
    fromField f                             = returnError ConversionFailed f "need an integer, ticket id"

instance FromField UserId where
    fromField (Field (SQLInteger uId) _)    = Ok . UserId . fromIntegral $ uId
    fromField f                             = returnError ConversionFailed f "need an integer, user id"

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
    fromRow = TicketInfo
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

instance FromField CommentId where
    fromField (Field (SQLInteger commId) _) = Ok . CommentId . fromIntegral $ commId
    fromField f                             = returnError ConversionFailed f "need an integer, comment id"

instance FromField CommentBody where
    fromField (Field (SQLText cBody) _)     = Ok . CommentBody $ cBody
    fromField f                             = returnError ConversionFailed f "need a text, comment body"

instance FromField AttachmentId where
    fromField (Field (SQLInteger attId) _)  = Ok . AttachmentId . fromIntegral $ attId
    fromField f                             = returnError ConversionFailed f "need an integer, attachment id"


-- TO

instance FromRow Attachment where
    fromRow = Attachment
        <$> field
        <*> field
        <*> field
        <*> field

instance FromRow AttachmentContent where
    fromRow = AttachmentContent <$> field

instance ToField TicketId where
    toField (TicketId tId)                  = SQLInteger . fromIntegral $ tId

instance ToField UserId where
    toField (UserId userId)                 = SQLInteger . fromIntegral $ userId

instance ToField CommentId where
    toField (CommentId commentId)           = SQLInteger . fromIntegral $ commentId

instance ToField CommentBody where
    toField (CommentBody commentBody)       = SQLText $ commentBody

instance ToField AttachmentId where
    toField (AttachmentId attachmentId)     = SQLInteger . fromIntegral $ attachmentId

instance ToField TicketURL where
    toField (TicketURL tiUrl)               = SQLText $ tiUrl

instance ToField TicketTags where
    toField (TicketTags tiTags)             = SQLText . fromString $ intercalate "," $ map toString tiTags

instance ToField TicketStatus where
    toField (TicketStatus tiStatus)         = SQLText $ tiStatus

------------------------------------------------------------
-- Query
------------------------------------------------------------

_getTicketsInfo
    :: forall m. (MonadIO m)
    => Connection
    -> m [TicketInfo]
_getTicketsInfo conn =
    liftIO $ query_ conn "SELECT * FROM ticket_info"

getTicketInfoByTicketId
    :: forall m. (MonadIO m)
    => Connection
    -> TicketId
    -> m (Maybe TicketInfo)
getTicketInfoByTicketId conn ticketId =
    liftIO $ safeHead <$> queryNamed conn
        "SELECT * FROM ticket_info WHERE tId = :id" [":id" := ticketId]

getAllAssignedTicketsByUser
    :: forall m. (MonadIO m)
    => Connection
    -> UserId
    -> m [TicketInfo]
getAllAssignedTicketsByUser conn userId =
    liftIO $ queryNamed conn "SELECT * FROM ticket_info WHERE assignee_id = :id" [":id" := userId]

getAllUnassignedTicketsByUser :: forall m. (MonadIO m) => Connection -> m [TicketInfo]
getAllUnassignedTicketsByUser conn =
    liftIO $ query_ conn "SELECT * FROM ticket_info WHERE assignee_id = NULL"

getAllRequestedTicketsByUser :: forall m. (MonadIO m) => Connection -> UserId -> m [TicketInfo]
getAllRequestedTicketsByUser conn userId =
    liftIO $ queryNamed conn "SELECT * FROM ticket_info WHERE requester_id = :id" [":id" := userId]


-- | A join would be more performant, but KISS for now.
getTicketComments :: forall m. (MonadIO m) => Connection -> TicketId -> m [Comment]
getTicketComments conn ticketId = do
    commentsInfo <- getTicketIdComments ticketId

    forM commentsInfo $ \(commentId, commentBody, commentIsPublic, commentAuthorId) -> do

        commentAttachments <- getCommentAttachments commentId

        pure Comment
            { cId          = commentId
            , cBody        = commentBody
            , cAttachments = commentAttachments
            , cPublic      = commentIsPublic
            , cAuthor      = commentAuthorId
            }
  where
    getTicketIdComments :: TicketId -> m [(CommentId, CommentBody, Bool, Integer)]
    getTicketIdComments ticketId' =
        liftIO $ queryNamed conn "SELECT tc.id, tc.body, tc.is_public, tc.author_id \
            \FROM ticket_comment tc \
            \WHERE tc.ticket_id = :id" [":id" := ticketId']

    getCommentAttachments :: CommentId -> m [Attachment]
    getCommentAttachments commentId =
        liftIO $ queryNamed conn
            "SELECT * FROM comment_attachments WHERE comment_id = :id" [":id" := commentId]

-- | We use a different database here since a simple calculation shows that
-- this database will be huge. If an average log takes around 10Mb, saving
-- 1000's or 10000's logs in a database is very space intensive.
-- Since most of the time we want to deal with the regular information not
-- relating to attachments, it makes sense to separate the DB's. Another option
-- is to use another database (KV storage/database) for this.
-- TODO(ks): For now, let's delay this decision for a bit.
getAttachmentContent :: forall m. (MonadIO m) => Connection -> Attachment -> m (Maybe AttachmentContent)
getAttachmentContent conn Attachment{..} =
    liftIO $ safeHead <$> queryNamed conn
        "SELECT * FROM attachment_content WHERE attachment_id = :id" [":id" := aId]

------------------------------------------------------------
-- DML
------------------------------------------------------------

-- TODO(ks): withTransaction

createSchema :: forall m. (MonadIO m) => Connection -> m ()
createSchema conn = liftIO $ execute_ conn
    "CREATE TABLE `ticket_info` (                                           \
\       `tiId`  INTEGER,                                                    \
\       `tiRequesterId` INTEGER NOT NULL,                                   \
\       `tiAssigneeId`  INTEGER,                                            \
\       `tiUrl` TEXT NOT NULL,                                              \
\       `tiTags`    TEXT NOT NULL,                                          \
\       `tiStatus`  TEXT NOT NULL,                                          \
\       PRIMARY KEY(tiId)                                                   \
\    ) WITHOUT ROWID;                                                       \
\                                                                           \
\    CREATE TABLE `ticket_comment` (                                        \
\       `id`    INTEGER,                                                    \
\       `ticket_id` INTEGER NOT NULL,                                       \
\       `body`  TEXT NOT NULL,                                              \
\       `is_public` INTEGER NOT NULL,                                       \
\       `author_id` INTEGER NOT NULL,                                       \
\       PRIMARY KEY(id),                                                    \
\       FOREIGN KEY(`ticket_id`) REFERENCES ticket_info(tId)                \
\    ) WITHOUT ROWID;                                                       \
\                                                                           \
\    CREATE TABLE `comment_attachment` (                                    \
\       `aId`   INTEGER,                                                    \
\       `comment_id`    INTEGER NOT NULL,                                   \
\       `aURL`  TEXT NOT NULL,                                              \
\       `aContentType`  TEXT NOT NULL,                                      \
\       `aSize` INTEGER NOT NULL,                                           \
\       PRIMARY KEY(aId),                                                   \
\         FOREIGN KEY(`comment_id`) REFERENCES ticket_comment ( ticket_id ) \
\    ) WITHOUT ROWID;                                                       \
\                                                                           \
\    CREATE TABLE `attachment_content` (                                    \
\       `attachment_id` INTEGER,                                            \
\       `content`   BLOB NOT NULL,                                          \
\       PRIMARY KEY(attachment_id),                                         \
\       FOREIGN KEY(`attachment_id`) REFERENCES comment_attachment(aId)     \
\    )"

insertTicketInfo :: forall m. (MonadIO m) => Connection -> TicketInfo -> m ()
insertTicketInfo conn TicketInfo{..} =
    liftIO $ executeNamed conn "INSERT INTO ticket_info (tiId, tiRequesterId, tiAssigneeId, tiUrl, tiTags, tiStatus) \
        \VALUES (:tiId, :tiRequesterId, :tiAssigneeId, :tiUrl, :tiTags, :tiStatus)"
        [ ":tiId"           := tiId
        , ":tiRequesterId"  := tiRequesterId
        , ":tiAssigneeId"   := tiAssigneeId
        , ":tiUrl"          := tiUrl
        , ":tiTags"         := tiTags
        , ":tiStatus"       := tiStatus
        ]

insertTicketComments :: forall m. (MonadIO m) => Connection -> TicketId -> Comment -> m ()
insertTicketComments conn ticketId Comment{..} =
    liftIO $ executeNamed conn "INSERT INTO ticket_comment (id, ticket_id, body, is_public, author_id) \
        \VALUES (:id, :ticket_id, :body, :is_public, :author_id)"
        [ ":id"             := cId
        , ":ticket_id"      := ticketId
        , ":body"           := cBody
        , ":is_public"      := cPublic
        , ":author_id"      := cAuthor
        ]

insertCommentAttachments :: forall m. (MonadIO m) => Connection -> Comment -> Attachment -> m ()
insertCommentAttachments conn Comment{..} Attachment{..} =
    liftIO $ executeNamed conn "INSERT INTO comment_attachment (aId, comment_id, aURL, aContentType, aSize) \
        \VALUES (:aId, :comment_id, :aURL, :aContentType, :aSize)"
        [ ":aId"            := aId
        , ":comment_id"     := cId
        , ":aURL"           := aURL
        , ":aContentType"   := aContentType
        , ":aSize"          := aSize
        ]

deleteCommentAttachments :: forall m. (MonadIO m) => Connection -> m ()
deleteCommentAttachments conn =
    liftIO $ execute_ conn "DELETE FROM comment_attachment"

deleteTicketComments :: forall m. (MonadIO m) => Connection -> m ()
deleteTicketComments conn =
    liftIO $ execute_ conn "DELETE FROM ticket_comment"

deleteTickets :: forall m. (MonadIO m) => Connection -> m ()
deleteTickets conn =
    liftIO $ execute_ conn "DELETE FROM ticket_info"

-- | Delete all data.
deleteAllData :: forall m. (MonadIO m) => Connection -> m ()
deleteAllData conn = do
    deleteCommentAttachments conn
    deleteTicketComments conn
    deleteTickets conn

