{-# OPTIONS_GHC -fno-warn-orphans #-}
-- The idea behind this module and doing orphan instances is that
-- if we ever want to switch to another implementation, we should
-- just remove this module, not rewrite all the types and change instances.
-- This way, we have real separation.

module DataSource.DB
    ( withDatabase
    , withProdDatabase
    , cachedZendeskLayer
    ) where

import           Universum

import           Data.Text (split)

import           Database.SQLite.Simple (FromRow (..), NamedParam (..), SQLData (..), close, field,
                                         open, queryNamed, query_)
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
    { zlGetTicketInfo           = liftIO . getTicketInfoByTicketId
    , zlListAssignedTickets     = liftIO . getAllAssignedTicketsByUser
    , zlListRequestedTickets    = liftIO . getAllRequestedTicketsByUser
    , zlListAgents              = error "We won't use this for now!"
    , zlGetAttachment           = liftIO . DataSource.DB.getAttachmentContent
    , zlGetTicketComments       = liftIO . getTicketComments
    , zlPostTicketComment       = error "We won't use this for now!"
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

instance ToField AttachmentId where
    toField (AttachmentId attachmentId)     = SQLInteger . fromIntegral $ attachmentId


-- | A general resource closing function.
withDatabase :: forall a. String -> (Connection -> IO a) -> IO a
withDatabase dbName dbOperation =
    bracket
        (open dbName)
        (close)
        dbOperation

-- | A production resource closing function.
withProdDatabase :: forall a. (Connection -> IO a) -> IO a
withProdDatabase = withDatabase "./prod.db"

_getTicketsInfo :: IO [TicketInfo]
_getTicketsInfo = withProdDatabase $ \conn ->
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


-- | A join would be more performance, but KISS for now.
getTicketComments :: TicketId -> IO [Comment]
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
    getTicketIdComments :: TicketId -> IO [(CommentId, CommentBody, Bool, Integer)]
    getTicketIdComments ticketId' = withProdDatabase $ \conn ->
        queryNamed conn "SELECT tc.id, tc.body, tc.is_public, tc.author_id FROM ticket_comment tc WHERE tc.ticket_id = :id" [":id" := ticketId']

    getCommentAttachments :: CommentId -> IO [Attachment]
    getCommentAttachments commentId = withProdDatabase $ \conn ->
        queryNamed conn "SELECT * FROM comment_attachments WHERE comment_id = :id" [":id" := commentId]

getAttachmentContent :: Attachment -> IO (Maybe AttachmentContent)
getAttachmentContent Attachment{..} = withProdDatabase $ \conn ->
    safeHead <$> queryNamed conn "SELECT * FROM attachment_content WHERE attachment_id = :id" [":id" := aId]

