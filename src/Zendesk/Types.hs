{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Zendesk.Types
    ( ZendeskLayer (..)
    , ZendeskResponse (..)
    , Comment (..)
    , CommentOuter (..)
    , Attachment (..)
    , RequestType (..)
    , Ticket (..)
    , TicketList (..)
    , TicketId
    , TicketURL
    , TicketInfo (..)
    , TicketTag (..)
    , parseAgentId
    , parseComments
    , parseTickets
    , renderTicketStatus
    -- * General configuration
    , Config (..)
    , knowledgebasePath
    , tokenPath
    , assignToPath
    , asksZendeskLayer
    , App
    , runApp
    ) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject, (.:),
                             (.=))
import           Data.Aeson.Types (Parser)
import           LogAnalysis.Types (Knowledge)

------------------------------------------------------------
-- Configuration
------------------------------------------------------------

newtype App a = App (ReaderT Config IO a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadReader Config
             , MonadIO
             )

runApp :: App a -> Config -> IO a
runApp (App a) = runReaderT a

-- | The basic configuration.
data Config = Config
    { cfgAgentId            :: !Integer
    -- ^ Zendesk agent id
    , cfgZendesk            :: !Text
    -- ^ URL to Zendesk
    , cfgToken              :: !Text
    -- ^ Zendesk token
    , cfgEmail              :: !Text
    -- ^ Email address of the user the classifier will process on
    , cfgAssignTo           :: !Integer
    -- ^ User that will be assigned to after the classifier has done the analysis
    , cfgKnowledgebase      :: ![Knowledge]
    -- ^ Knowledgebase
    , cfgNumOfLogsToAnalyze :: !Int
    -- ^ Number of files classifier will analyze
    , cfgIsCommentPublic    :: !Bool
    -- ^ If the comment is public or not, for a test run we use an internal comment.
    , cfgZendeskLayer       :: !(ZendeskLayer App)
    -- ^ The Zendesk API layer. We will ideally move this into a
    -- separate configuration containing all the layer (yes, there a couple of them).
    } -- deriving (Eq, Show)


-- | Utility function for getting a function of the @ZendeskLayer@.
-- There are plenty of other alternatives, but this is the simplest
-- and most direct one.
asksZendeskLayer :: forall m a. (MonadReader Config m) => (ZendeskLayer App -> a) -> m a
asksZendeskLayer getter = do
    Config{..} <- ask
    pure $ getter cfgZendeskLayer


-- TODO(ks): Move these three below to CLI!
-- | Path to knowledgebase
knowledgebasePath :: FilePath
knowledgebasePath = "./knowledgebase/knowledge.csv"

-- | Filepath to token file
tokenPath :: FilePath
tokenPath = "./tmp-secrets/token"

-- | Filepath to assign_to file
assignToPath :: FilePath
assignToPath = "./tmp-secrets/assign_to"

-- | The Zendesk API interface that we want to expose.
-- We don't want anything to leak out, so we expose only the most relevant information,
-- anything relating to how it internaly works should NOT be exposed.
data ZendeskLayer m = ZendeskLayer
    { zlGetTicketInfo     :: TicketId -> m TicketInfo
    , zlListTickets       :: RequestType -> m [TicketInfo]
    , zlPostTicketComment :: ZendeskResponse -> m ()
    , zlGetAgentId        :: m Integer
    , zlGetAttachment     :: Attachment -> m LByteString
    , zlGetTicketComments :: TicketId -> m [Comment]
    }

-- | Attachment of the ticket
data Attachment = Attachment
    { aURL         :: !Text
    -- ^ URL of the attachment
    , aContentType :: !Text
    -- ^ ContentType of the attachment
    , aSize        :: !Int
    -- ^ Attachment size
    }

-- | Request type of the ticket
data RequestType
    = Requested
    | Assigned

-- | The response for ZenDesk.
data ZendeskResponse = ZendeskResponse
    { zrTicketId :: !TicketId
    , zrComment  :: !Text
    , zrTags     :: ![Text] -- TODO(ks): This should be wrapped
    , zrIsPublic :: !Bool
    }

-- | Comments
data Comment = Comment
    { cBody        :: !Text
    -- ^ Body of comment
    , cAttachments :: ![Attachment]
    -- ^ Attachment
    , cPublic      :: !Bool
    -- ^ Flag of whether comment should be public
    , cAuthor      :: !Integer
    -- ^ Auther of comment
    }

-- | Outer comment ??
newtype CommentOuter = CommentOuter {
      coComment :: Comment
    }

-- | Zendesk ticket
data Ticket = Ticket
    { tComment  :: !Comment
    -- ^ Ticket comment
    , tAssignee :: !Integer
    -- ^ Assignee of the ticket
    , tTag      :: ![Text]
    -- ^ Tags attached to ticket
    }

-- | List of zendesk ticket
data TicketList = TicketList
    { tlTickets :: ![TicketInfo]
    -- ^ Information of tickets
    , nextPage  :: Maybe Text
    -- ^ Next page
    }

type TicketId = Int
type TicketURL = Text -- TODO(ks): We should wrap all these...

data TicketInfo = TicketInfo
    { ticketId     :: !TicketId    -- ^ Id of an ticket
    , ticketUrl    :: !TicketURL   -- ^ The ticket URL
    , ticketTags   :: ![Text]      -- ^ Tags associated with ticket
    , ticketStatus :: !Text        -- ^ The status of the ticket
    } deriving (Eq, Show)

instance Ord TicketInfo where
    compare t1 t2 = compare (ticketId t1) (ticketId t2)


-- | Ticket tag
-- TODO(ks): @Generic@ type migrations. Also possible to provide the version from runtime,
-- we need to weigh these options later on.
data TicketTag
    = AnalyzedByScript      -- ^ Ticket has been analyzed
    | AnalyzedByScriptV1_0  -- ^ Ticket has been analyzed by the version 1.0
    | NoKnownIssue          -- ^ Ticket had no known issue

-- | Defining it's own show instance to use it as tags
renderTicketStatus :: TicketTag -> Text
renderTicketStatus AnalyzedByScript     = "analyzed-by-script"
renderTicketStatus AnalyzedByScriptV1_0 = "analyzed-by-script-v1.0"
renderTicketStatus NoKnownIssue         = "no-known-issues"

-- | JSON Parsing
instance FromJSON Comment where
    parseJSON = withObject "comment" $ \o ->
        Comment <$> o .: "body" <*> o .: "attachments" <*> o .: "public" <*> o .: "author_id"

instance ToJSON Comment where
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
    parseJSON = withObject "ticket" $ \o -> do
        ticketId        <- o .: "id"
        ticketUrl       <- o .: "url"
        ticketTags      <- o .: "tags"
        ticketStatus    <- o .: "status"

        pure TicketInfo{..}

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

