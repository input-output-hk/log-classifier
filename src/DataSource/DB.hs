{-# OPTIONS_GHC -fno-warn-orphans #-}
-- The idea behind this module and doing orphan instances is that
-- if we ever want to switch to another implementation, we should
-- just remove this module, not rewrite all the types and change instances.
-- This way, we have real separation.

module DataSource.DB
    ( withDatabase
    , withProdDatabase
    , cachedZendeskLayer
    -- * For updating local database
    , insertTicketInfo
    , insertTicketComments
    , insertCommentAttachments
    -- * Delete data
    , deleteAllData
    , deleteCommentAttachments
    , deleteTicketComments
    , deleteTickets
    ) where

import           Universum

import           Data.Text (split)

import           Database.SQLite.Simple (FromRow (..), NamedParam (..), SQLData (..), close,
                                         executeNamed, execute_, field, open, queryNamed, query_)
import           Database.SQLite.Simple.FromField (FromField (..), ResultError (..), returnError)
import           Database.SQLite.Simple.Internal (Connection, Field (..))
import           Database.SQLite.Simple.Ok (Ok (..))
import           Database.SQLite.Simple.ToField (ToField (..))

import           DataSource.Types (Attachment (..), AttachmentContent (..), AttachmentId (..),
                                   Comment (..), CommentBody (..), CommentId (..), Config,
                                   TicketId (..), TicketInfo (..), TicketStatus (..),
                                   TicketTags (..), TicketURL (..), UserId (..), ZendeskLayer (..))


-- https://ocharles.org.uk/blog/posts/2014-08-07-postgresql-simple-generic-sop.html

-- | The cached Zendesk layer. We are using a simple SQLite DB behind the scenes
-- that we need to sync occasionaly.
cachedZendeskLayer :: (MonadIO m, MonadReader Config m) => ZendeskLayer m
cachedZendeskLayer = ZendeskLayer
    { zlGetTicketInfo           = getTicketInfoByTicketId
    , zlListDeletedTickets      = error "We won't use this for now."
    , zlListAssignedTickets     = getAllAssignedTicketsByUser
    , zlListRequestedTickets    = getAllRequestedTicketsByUser
    , zlListUnassignedTickets   = getAllUnassignedTicketsByUser
    , zlListAdminAgents         = error "We won't use this for now!"
    , zlGetAttachment           = DataSource.DB.getAttachmentContent
    , zlGetTicketComments       = getTicketComments
    , zlPostTicketComment       = error "We won't use this for now!"
    , zlExportTickets           = error "This is redundant since we mostly require it for caching!"
    }

-- Database instances
-- FROM

instance FromField TicketId where
    fromField (Field (SQLInteger tId) _)    = Ok . TicketId . fromIntegral $ tId
    fromField f                             = returnError ConversionFailed f "need an integer, ticket id"

instance FromField UserId where
    fromField (Field (SQLInteger uId) _)    = Ok . UserId . fromIntegral $ uId
    fromField f                             = returnError ConversionFailed f "need an integer, user id"

instance FromField TicketURL where
    fromField (Field (SQLText tURL) _) = Ok . TicketURL $ tURL
    fromField f                        = returnError ConversionFailed f "need a text, ticket url"

-- | TODO(ks): Yes, yes, normal form...
instance FromField TicketTags where
    fromField (Field (SQLText tTags) _)     = Ok . TicketTags . split (==',') $ tTags
    fromField f                             = returnError ConversionFailed f "need a text, ticket tags"

instance FromField TicketStatus where
    fromField (Field (SQLText tStat) _)     = Ok . TicketStatus $ tStat
    fromField f                             = returnError ConversionFailed f "need a text, ticket status"

instance FromRow TicketInfo where
    fromRow = TicketInfo <$> field <*> field <*> field <*> field <*> field <*> field

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
    fromRow = Attachment <$> field <*> field <*> field <*> field

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
-- Query
------------------------------------------------------------

_getTicketsInfo :: forall m. (MonadIO m) => m [TicketInfo]
_getTicketsInfo = withProdDatabase $ \conn ->
    query_ conn "SELECT * FROM ticket_info"

getTicketInfoByTicketId :: forall m. (MonadIO m) => TicketId -> m (Maybe TicketInfo)
getTicketInfoByTicketId ticketId = withProdDatabase $ \conn ->
    safeHead <$> queryNamed conn "SELECT * FROM ticket_info WHERE tId = :id" [":id" := ticketId]

getAllAssignedTicketsByUser :: forall m. (MonadIO m) => UserId -> m [TicketInfo]
getAllAssignedTicketsByUser userId = withProdDatabase $ \conn ->
    queryNamed conn "SELECT * FROM ticket_info WHERE assignee_id = :id" [":id" := userId]

getAllUnassignedTicketsByUser :: forall m. (MonadIO m) => m [TicketInfo]
getAllUnassignedTicketsByUser = withProdDatabase $ \conn ->
    query_ conn "SELECT * FROM ticket_info WHERE assignee_id = NULL"

getAllRequestedTicketsByUser :: forall m. (MonadIO m) => UserId -> m [TicketInfo]
getAllRequestedTicketsByUser userId = withProdDatabase $ \conn ->
    queryNamed conn "SELECT * FROM ticket_info WHERE requester_id = :id" [":id" := userId]


-- | A join would be more performant, but KISS for now.
getTicketComments :: forall m. (MonadIO m) => TicketId -> m [Comment]
getTicketComments ticketId = do
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
    getTicketIdComments ticketId' = withProdDatabase $ \conn ->
        queryNamed conn "SELECT tc.id, tc.body, tc.is_public, tc.author_id FROM ticket_comment tc WHERE tc.ticket_id = :id" [":id" := ticketId']

    getCommentAttachments :: CommentId -> m [Attachment]
    getCommentAttachments commentId = withProdDatabase $ \conn ->
        queryNamed conn "SELECT * FROM comment_attachments WHERE comment_id = :id" [":id" := commentId]

-- | We use a different database here since a simple calculation shows that
-- this database will be huge. If an average log takes around 10Mb, saving
-- 1000's or 10000's logs in a database is very space intensive.
-- Since most of the time we want to deal with the regular information not
-- relating to attachments, it makes sense to separate the DB's. Another option
-- is to use another database (KV storage/database) for this.
-- TODO(ks): For now, let's delay this decision for a bit.
getAttachmentContent :: forall m. (MonadIO m) => Attachment -> m (Maybe AttachmentContent)
getAttachmentContent Attachment{..} = withProdDatabase $ \conn ->
    safeHead <$> queryNamed conn "SELECT * FROM attachment_content WHERE attachment_id = :id" [":id" := aId]

------------------------------------------------------------
-- DML
------------------------------------------------------------

-- TODO(ks): withTransaction

insertTicketInfo :: forall m. (MonadIO m) => TicketInfo -> m ()
insertTicketInfo TicketInfo{..} = withProdDatabase $ \conn ->
    executeNamed conn "INSERT INTO ticket_info (tiId, tiRequesterId, tiAssigneeId, tiUrl, tiTags, tiStatus) VALUES (:tiId, :tiRequesterId, :tiAssigneeId, :tiUrl, :tiTags, :tiStatus)"
        [ ":tiId"           := tiId
        , ":tiRequesterId"  := tiRequesterId
        , ":tiAssigneeId"   := tiAssigneeId
        , ":tiUrl"          := tiUrl
        , ":tiTags"         := tiTags
        , ":tiStatus"       := tiStatus
        ]

insertTicketComments :: forall m. (MonadIO m) => TicketId -> Comment -> m ()
insertTicketComments ticketId Comment{..} = withProdDatabase $ \conn ->
    executeNamed conn "INSERT INTO ticket_comment (id, ticket_id, body, is_public, author_id) VALUES (:id, :ticket_id, :body, :is_public, :author_id)"
        [ ":id"             := cId
        , ":ticket_id"      := ticketId
        , ":body"           := cBody
        , ":is_public"      := cPublic
        , ":author_id"      := cAuthor
        ]

insertCommentAttachments :: forall m. (MonadIO m) => Comment -> Attachment -> m ()
insertCommentAttachments Comment{..} Attachment{..} = withProdDatabase $ \conn ->
    executeNamed conn "INSERT INTO comment_attachment (aId, comment_id, aURL, aContentType, aSize) VALUES (:aId, :comment_id, :aURL, :aContentType, :aSize)"
        [ ":aId"            := aId
        , ":comment_id"     := cId
        , ":aURL"           := aURL
        , ":aContentType"   := aContentType
        , ":aSize"          := aSize
        ]

deleteCommentAttachments :: forall m. (MonadIO m) => m ()
deleteCommentAttachments = withProdDatabase $ \conn ->
    execute_ conn "DELETE FROM comment_attachment"

deleteTicketComments :: forall m. (MonadIO m) => m ()
deleteTicketComments = withProdDatabase $ \conn ->
    execute_ conn "DELETE FROM ticket_comment"

deleteTickets :: forall m. (MonadIO m) => m ()
deleteTickets = withProdDatabase $ \conn ->
    execute_ conn "DELETE FROM ticket_info"

-- | Delete all data.
-- Here we can show that we lack composition, and the step we
-- need to achive it.
deleteAllData :: forall m. (MonadIO m) => m ()
deleteAllData = withProdDatabase $ \conn -> do
    execute_ conn "DELETE FROM comment_attachment"
    execute_ conn "DELETE FROM ticket_comment"
    execute_ conn "DELETE FROM ticket_info"

