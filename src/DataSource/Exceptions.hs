module DataSource.Exceptions where

import           Universum

import           DataSource.Types (AttachmentId (..), CommentId (..), TicketId (..))

import           Prelude (Show (..))

-- | Exceptions that can occur in 'DBLayer'
data DBException =
      DBDeleteException
    | InsertCommentAttachmentException CommentId AttachmentId
    | InsertTicketCommentException TicketId CommentId
    | InsertTicketInfoException TicketId
    deriving Show

-- | This is used to throw both 'DBLayerException and 'SomeException'
-- 'Error' from sqlite-simple has no instance of 'Exception'
-- therefore the only way to catching it is by 'SomeException'
-- Both of these exception are need in case the program crashes and we need to
-- find the root cause of it.
data DBLayerException = DBLayerException DBException SomeException

instance Exception DBException

instance Exception DBLayerException

instance Show DBLayerException where
    show (DBLayerException dberr someErr) =
        concat
        [ "Error occured on DBlayer: "
        , Universum.show dberr
        , " with reason: "
        , Universum.show someErr
        ]
