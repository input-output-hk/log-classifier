{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Comment(..)
    , CommentOuter(..)
    , Attachment(..)
    , Ticket(..)
    , TicketList(..)
    , TicketId
    , TicketInfo(..)
    , TicketStatus(..)
    , parseAgentId
    , parseComments
    , parseTickets
    , renderTicketStatus
    ) where

import           Universum

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)

-- | Comments
data Comment = Comment
    { commentBody        :: !Text         -- ^ Body of comment
    , commentAttachments :: ![Attachment] -- ^ Attachment
    , commentPublic      :: !Bool         -- ^ Flag of whether comment should be public
    , commentAuthor      :: !Integer      -- ^ Auther of comment
    }

-- | Outer comment ??
newtype CommentOuter = CommentOuter {
      coComment :: Comment
    }

-- | Attachment of the ticket
data Attachment = Attachment
    { attachmentURL         :: !Text -- ^ URL of the attachment
    , attachmentContentType :: !Text -- ^ ContentType of the attachment
    , attachmentSize        :: !Int  -- ^ Attachment size
    }

-- | Zendexk ticket
data Ticket = Ticket
    { ticketComment  :: !Comment   -- ^ Ticket comment
    , ticketAssignee :: !Integer   -- ^ Assignee of the ticket
    , ticketTag      :: ![Text]    -- ^ Tags attached to ticket
    }

-- | List of zendesk ticket
data TicketList = TicketList
    { ticketListTickets :: ![TicketInfo] -- ^ Information of tickets
    , nextPage          :: Maybe Text    -- ^ Next page
    }

type TicketId = Int

data TicketInfo = TicketInfo
    { ticketId   :: !Int    -- ^ Id of an ticket
    , ticketTags :: ![Text] -- ^ Tags associated with ticket
    }

-- | Ticket status
data TicketStatus =
      AnalyzedByScript -- ^ Ticket has been analyzed
    | NoKnownIssue     -- ^ Ticket had no known issue
    deriving Show

-- | Defining it's own show instance to use it as tags
renderTicketStatus :: TicketStatus -> Text
renderTicketStatus AnalyzedByScript = "analyzed-by-script"
renderTicketStatus NoKnownIssue     = "no-known-issues"

-- | JSON Parsing
instance FromJSON Comment where
  parseJSON = withObject "comment" $ \o ->
    Comment <$> o .: "body" <*> o .: "attachments" <*> o .: "public" <*> o .: "author_id"

instance ToJSON Comment where -- Add tag and status
  toJSON (Comment b as public author)
    = object [ "body" .= b, "attachments" .= as,
      "public" .= public, "author_id" .= author]

instance ToJSON CommentOuter where
  toJSON (CommentOuter c) = object [ "comment" .= c ]

instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \o ->
    Attachment <$> o .: "content_url" <*> o .: "content_type" <*> o .: "size"

instance ToJSON Ticket where
  toJSON (Ticket comment assignee tags) =
    object [ "ticket" .= object
             [ "comment" .= comment, "assignee_id" .= assignee, "tags" .= tags]
           ]

instance ToJSON Attachment where
  toJSON (Attachment url contenttype size) =
    object [ "content_url" .= url, "content_type" .= contenttype, "size" .= size]

instance FromJSON TicketInfo where
  parseJSON = withObject "ticket" $ \o -> TicketInfo <$> (o .: "id") <*> (o .: "tags")

instance FromJSON TicketList where
  parseJSON = withObject "ticketList" $ \o -> TicketList <$> o .: "tickets" <*> o .: "next_page"

-- | Parse tickets
parseTickets :: Value -> Parser TicketList
parseTickets = withObject "tickets" $ \o -> TicketList <$> o .: "tickets" <*> o .: "next_page"

-- | Parse comments
parseComments :: Value -> Parser [ Comment ]
parseComments = withObject "comments" $ \o -> o .: "comments"

-- | Parse the apiRequest of getAgentId
parseAgentId :: Value -> Parser Integer
parseAgentId = withObject "user" $ \o -> (o .: "user") >>= (.: "id")
