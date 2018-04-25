{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Text        (Text)

-- | Comments
data Comment = Comment
  { commentBody        :: Text
  , commentAttachments :: [Attachment]
  , commentPublic      :: Bool
  , commentAuthor      :: Integer
  } deriving (Show, Eq)

-- | Outer comment ??
newtype CommentOuter = CommentOuter {
    coComment :: Comment
  } deriving (Show, Eq)

-- | Attachment of the ticket
data Attachment = Attachment
  { attachmentURL         :: Text
  , attachmentContentType :: Text
  , attachmentSize        :: Int
  } deriving (Show, Eq)

-- | Zendexk ticket
data Ticket = Ticket {
    ticketComment  :: Comment
  , ticketAssignee :: Integer
  , ticketTag      :: [Text]
  } deriving (Show, Eq)

-- | List of zendesk ticket
data TicketList = TicketList {
    ticketListTickets :: [ TicketId ]
  , nextPage          :: Maybe Text
  } deriving (Show, Eq)

newtype TicketId = TicketId Int deriving (Eq)

instance Show TicketId where
  show (TicketId tid) = show tid

-- | Ticket status
data TicketStatus =
    AnalyzedByScript
  | NoKnownIssue

instance Show TicketStatus where
  show AnalyzedByScript = "analyzed-by-script"
  show NoKnownIssue     = "no-known-issues"


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

instance FromJSON TicketId where
  parseJSON = withObject "ticket" $ \o -> TicketId <$> (o .: "id")

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
