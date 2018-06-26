{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module DataSource.Types
    ( Attachment (..)
    , AttachmentId (..)
    , AttachmentContent (..)
    , Comment (..)
    , CommentId (..)
    , CommentBody (..)
    , CommentOuter (..)
    , PageResultList (..)
    , RequestType (..)
    , Ticket (..)
    , TicketId (..)
    , TicketURL (..)
    , TicketTags (..)
    , TicketStatus (..)
    , TicketInfo (..)
    , TicketTag (..)
    , User (..)
    , UserId (..)
    , UserURL (..)
    , UserName (..)
    , UserEmail (..)
    , ZendeskResponse (..)
    , FromPageResultList (..)
    , parseComments
    , parseTicket
    , renderTicketStatus
    -- * General configuration
    , App
    , Config (..)
    , IOLayer (..)
    , ZendeskLayer (..)
    , ZendeskAPIUrl (..)
    , knowledgebasePath
    , tokenPath
    , assignToPath
    , asksZendeskLayer
    , asksIOLayer
    , showURL
    , runApp
    ) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject, (.:),
                             (.=))
import           Data.Aeson.Types (Parser)
import           Data.Text (pack)
import           LogAnalysis.Types (Knowledge)

import           Test.QuickCheck (Arbitrary (..), elements, listOf1, vectorOf)

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
    , cfgIOLayer            :: !(IOLayer App)
    -- ^ The IO layer.
    }


-- | Utility function for getting a function of the @ZendeskLayer@.
-- There are plenty of other alternatives, but this is the simplest
-- and most direct one.
asksZendeskLayer :: forall m a. (MonadReader Config m) => (ZendeskLayer App -> a) -> m a
asksZendeskLayer getter = do
    Config{..} <- ask
    pure $ getter cfgZendeskLayer


-- | Utility function for getting a function of the @ZendeskLayer@.
-- There are plenty of other alternatives, but this is the simplest
-- and most direct one.
asksIOLayer :: forall m a. (MonadReader Config m) => (IOLayer App -> a) -> m a
asksIOLayer getter = do
    Config{..} <- ask
    pure $ getter cfgIOLayer


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
    { zlGetTicketInfo        :: TicketId         -> m (Maybe TicketInfo)
    , zlListRequestedTickets :: UserId           -> m [TicketInfo]
    , zlListAssignedTickets  :: UserId           -> m [TicketInfo]
    , zlListAdminAgents      ::                     m [User]
    , zlGetTicketComments    :: TicketId         -> m [Comment]
    , zlGetAttachment        :: Attachment       -> m (Maybe AttachmentContent)
    , zlPostTicketComment    :: ZendeskResponse  -> m ()
    }

-- | The IOLayer interface that we can expose.
-- We want to do this since we want to be able to mock out any function tied to @IO@.
data IOLayer m = IOLayer
    { iolPrintText :: Text -> m ()
    , iolReadFile  :: FilePath -> m String
    }

------------------------------------------------------------
-- Class and instances to display in URL
------------------------------------------------------------

-- Let's keep this simple for now. Not much use for it now since
-- we can't simply extend it using @ZendeskAPIUrl@, but let's worry
-- about that later.
class ToURL a where
    toURL :: a -> Text

instance ToURL UserId where
    toURL (UserId uId) = show uId

instance ToURL TicketId where
    toURL (TicketId ticketId) = show ticketId

data ZendeskAPIUrl
    = AgentGroupURL
    | UserRequestedTicketsURL UserId
    | UserAssignedTicketsURL UserId
    | TicketsURL TicketId
    | TicketAgentURL TicketId
    | UserInfoURL
    | TicketCommentsURL TicketId
    deriving (Eq, Generic)

showURL :: ZendeskAPIUrl -> Text
showURL AgentGroupURL                       = "users.json?role%5B%5D=admin&role%5B%5D=agent"
showURL (UserRequestedTicketsURL userId)    = "/users/" <> toURL userId <> "/tickets/requested.json"
showURL (UserAssignedTicketsURL userId)     = "/users/" <> toURL userId <> "/tickets/assigned.json"
showURL (TicketsURL ticketId)               = "/tickets/" <> toURL ticketId <> ".json"
showURL (TicketAgentURL ticketId)           = "https://iohk.zendesk.com/agent/tickets/" <> toURL ticketId
showURL (UserInfoURL)                       = "/users/me.json"
showURL (TicketCommentsURL ticketId)        = "/tickets/" <> toURL ticketId <> "/comments.json"

------------------------------------------------------------
-- Types
------------------------------------------------------------

newtype AttachmentId = AttachmentId
    { getAttachmentId :: Int
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

-- TODO(ks): Arbitrary log contents?
newtype AttachmentContent = AttachmentContent
    { getAttachmentContent :: LByteString
    } deriving (Eq, Show, Ord, Generic, Monoid)

-- | Attachment of the ticket
data Attachment = Attachment
    { aId          :: !AttachmentId
    -- ^ Id of the attachment
    , aURL         :: !Text
    -- ^ URL of the attachment
    , aContentType :: !Text
    -- ^ ContentType of the attachment
    , aSize        :: !Int
    -- ^ Attachment size
    } deriving (Eq, Show)

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

newtype CommentId = CommentId
    { getCommentId :: Int
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype CommentBody = CommentBody
    { getCommentBody :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

-- | Comments
data Comment = Comment
    { cId          :: !CommentId
    -- ^ The ID of the comment
    , cBody        :: !CommentBody
    -- ^ Body of comment
    , cAttachments :: ![Attachment]
    -- ^ Attachment
    , cPublic      :: !Bool
    -- ^ Flag of whether comment should be public
    , cAuthor      :: !Integer
    -- ^ Author of comment
    } deriving (Eq, Show)

-- | Outer comment ??
newtype CommentOuter = CommentOuter {
      coComment :: Comment
    }

-- | Zendesk ticket
data Ticket = Ticket
    { tComment  :: !Comment
    -- ^ Ticket comment
    , tTag      :: ![Text]
    -- ^ Tags attached to ticket
    }

-- TODO(ks): We need to verify this still works, we don't have any
-- regression tests...
newtype TicketId = TicketId
    { getTicketId :: Int
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype TicketURL = TicketURL
    { getTicketURL :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype TicketTags = TicketTags
    { getTicketTags :: [Text] -- TODO(ks): We need to fix the @TicketTag@ / @TicketTags@ story.
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype TicketStatus = TicketStatus
    { getTicketStatus :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

data TicketInfo = TicketInfo
    { tiId          :: !TicketId        -- ^ Id of an ticket
    , tiRequesterId :: !UserId          -- ^ Id of the requester
    , tiAssigneeId  :: !(Maybe UserId)  -- ^ Id of the asignee
    , tiUrl         :: !TicketURL       -- ^ The ticket URL
    , tiTags        :: !TicketTags      -- ^ Tags associated with ticket
    , tiStatus      :: !TicketStatus    -- ^ The status of the ticket
    } deriving (Eq, Show, Generic)

-- | Ticket tag
-- TODO(ks): @Generic@ type migrations. Also possible to provide the version from runtime,
-- we need to weigh these options later on.
data TicketTag
    = AnalyzedByScript      -- ^ Ticket has been analyzed
    | AnalyzedByScriptV1_0  -- ^ Ticket has been analyzed by the version 1.0
    | AnalyzedByScriptV1_1  -- ^ Ticket has been analyzed by the version 1.1
    | NoKnownIssue          -- ^ Ticket had no known issue
    | NoLogAttached         -- ^ Log file not attached

newtype UserId = UserId
    { getUserId :: Int
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype UserURL = UserURL
    { getUserURL :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype UserName = UserName
    { getUserName :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype UserEmail = UserEmail
    { getUserEmail :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

data User = User
    { uId    :: !UserId      -- ^ Id of the user
    , uURL   :: !UserURL     -- ^ URL of the user
    , uName  :: !UserName    -- ^ Name of the user
    , uEmail :: !UserEmail   -- ^ Email of the user
    } deriving (Eq, Show, Generic)

data PageResultList a = PageResultList
    { prlResults  :: ![a]
    , prlNextPage :: !(Maybe Text)
    }

------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------

instance Arbitrary AttachmentId where
    arbitrary = AttachmentId <$> arbitrary

instance Arbitrary Attachment where
    arbitrary = Attachment
        <$> arbitrary
        <*> pure "http://attach.com"
        <*> pure "application/zip"  -- TODO(ks): More random...
        <*> arbitrary

instance Arbitrary CommentBody where
    arbitrary = CommentBody . fromString <$> arbitrary

instance Arbitrary CommentId where
    arbitrary = CommentId <$> arbitrary

instance Arbitrary Comment where
    arbitrary = Comment
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary TicketId where
    arbitrary = TicketId <$> arbitrary

instance Arbitrary TicketStatus where
    arbitrary = TicketStatus <$> elements ["new", "hold", "open","solved", "pending"]

instance Arbitrary TicketTags where
    arbitrary = do
        numberOfTag <- arbitrary
        tagsList    <- map fromString <$> vectorOf numberOfTag arbitrary
        pure . TicketTags $ tagsList

instance Arbitrary TicketURL where
    arbitrary = do
        protocol    <- elements ["http://", "https://"]
        name        <- listOf1 $ elements ['a'..'z']
        domain      <- elements [".com",".com.br",".net",".io"]
        pure . TicketURL . pack $ protocol ++ name ++ domain

instance Arbitrary TicketInfo where
    arbitrary = do
        ticketId            <- arbitrary
        ticketRequesterId   <- arbitrary
        ticketAssigneeId    <- arbitrary
        ticketUrl           <- arbitrary
        ticketTags          <- arbitrary
        ticketStatus        <- arbitrary

        pure TicketInfo
            { tiId          = ticketId
            , tiRequesterId = ticketRequesterId
            , tiAssigneeId  = ticketAssigneeId
            , tiUrl         = ticketUrl
            , tiTags        = ticketTags
            , tiStatus      = ticketStatus
            }

instance Arbitrary UserId where
    arbitrary = UserId <$> arbitrary


instance Arbitrary UserEmail where
    arbitrary = do
        address     <- listOf1 $ elements ['a'..'z']
        domain      <- elements ["gmail.com", "yahoo.com", "hotmail.com"]
        pure . UserEmail . pack $ address <> "@" <> domain

instance Arbitrary UserName where
    arbitrary = UserName . fromString <$> arbitrary

instance Arbitrary UserURL where
    arbitrary = do
        protocol    <- elements ["http://", "https://"]
        name        <- listOf1 $ elements ['a'..'z']
        domain      <- elements [".com",".com.br",".net",".io"]
        pure . UserURL . pack $ protocol ++ name ++ domain

instance Arbitrary User where
    arbitrary = do
        userId    <- arbitrary
        userUrl   <- arbitrary
        userName  <- arbitrary
        userEmail <- arbitrary

        pure User
            { uId = userId
            , uURL = userUrl
            , uName = userName
            , uEmail = userEmail
            }
------------------------------------------------------------
-- FromJSON instances
------------------------------------------------------------

instance FromJSON Attachment where
    parseJSON = withObject "attachment" $ \o -> do
        attachmentId     <- o .: "id"
        attachmentUrl    <- o .: "content_url"
        attachmentType   <- o .: "content_type"
        attachmentSize   <- o .: "size"

        pure Attachment
            { aId           = attachmentId
            , aURL          = attachmentUrl
            , aContentType  = attachmentType
            , aSize         = attachmentSize
            }

-- | JSON Parsing
instance FromJSON Comment where
    parseJSON = withObject "comment" $ \o -> do
        commentId           <- o .: "id"
        commentBody         <- o .: "body"
        commentAttachments  <- o .: "attachments"
        commentIsPublic     <- o .: "public"
        commentAuthorId     <- o .: "author_id"

        pure Comment
            { cId          = commentId
            , cBody        = commentBody
            , cAttachments = commentAttachments
            , cPublic      = commentIsPublic
            , cAuthor      = commentAuthorId
            }

class FromPageResultList a where
    fromPageResult :: Value -> Parser (PageResultList a)

instance FromPageResultList TicketInfo where
    fromPageResult = withObject "ticketList" $ \o ->
            PageResultList
                <$> o .: "tickets"
                <*> o .: "next_page"

instance FromPageResultList User where
    fromPageResult = withObject "userList" $ \o ->
            PageResultList
                <$> o .: "users"
                <*> o .: "next_page"

instance (FromPageResultList a) => FromJSON (PageResultList a) where
    parseJSON = fromPageResult

instance FromJSON TicketInfo where
    parseJSON = withObject "ticket" $ \o -> do
        ticketId            <- o .: "id"
        ticketRequesterId   <- o .: "requester_id"
        ticketAssigneeId    <- o .: "assignee_id"
        ticketUrl           <- o .: "url"
        ticketTags          <- o .: "tags"
        ticketStatus        <- o .: "status"

        pure TicketInfo
            { tiId          = ticketId
            , tiRequesterId = ticketRequesterId
            , tiAssigneeId  = ticketAssigneeId
            , tiUrl         = ticketUrl
            , tiTags        = ticketTags
            , tiStatus      = ticketStatus
            }

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        userId        <- o .: "id"
        userUrl       <- o .: "url"
        userName      <- o .: "name"
        userEmail     <- o .: "email"

        pure User
            { uId       = userId
            , uURL      = userUrl
            , uName     = userName
            , uEmail    = userEmail
            }

------------------------------------------------------------
-- ToJSON instances
------------------------------------------------------------

instance ToJSON Attachment where
    toJSON (Attachment _ url contenttype size) =
        object  [ "content_url"     .= url
                , "content_type"    .= contenttype
                , "size"            .= size
                ]

instance ToJSON Comment where
    toJSON (Comment _ b as public author) =
        object  [ "body"            .= b
                , "attachments"     .= as
                , "public"          .= public
                , "author_id"       .= author
                ]

-- TODO (hs): Erase ths since it's not used
instance ToJSON CommentOuter where
    toJSON (CommentOuter c) =
        object  [ "comment"         .= c
                ]

instance ToJSON Ticket where
    toJSON (Ticket comment tags) =
        object  [ "ticket" .= object
                    [ "comment"     .= comment
                    , "tags"        .= tags
                    ]
                ]

------------------------------------------------------------
-- Ord instances
------------------------------------------------------------

instance Ord Attachment where
    compare a1 a2 = compare (aId a1) (aId a2)

instance Ord Comment where
    compare c1 c2 = compare (cId c1) (cId c2)

instance Ord TicketInfo where
    compare t1 t2 = compare (tiId t1) (tiId t2)

------------------------------------------------------------
-- JSON parsers
------------------------------------------------------------

parseTicket :: Value -> Parser TicketInfo
parseTicket = withObject "ticket" $ \o -> o .: "ticket"

-- | TODO(ks): This seems like it's not required.
-- Parse comments
parseComments :: Value -> Parser [ Comment ]
parseComments = withObject "comments" $ \o -> o .: "comments"

------------------------------------------------------------
-- Auxiliary functions
------------------------------------------------------------

-- | Defining it's own show instance to use it as tags
renderTicketStatus :: TicketTag -> Text
renderTicketStatus AnalyzedByScript     = "analyzed-by-script"
renderTicketStatus AnalyzedByScriptV1_0 = "analyzed-by-script-v1.0"
renderTicketStatus AnalyzedByScriptV1_1 = "analyzed-by-script-v1.1"
renderTicketStatus NoKnownIssue         = "no-known-issues"
renderTicketStatus NoLogAttached        = "no-log-files"
