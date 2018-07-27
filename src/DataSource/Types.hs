{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
    , DeletedTicket (..)
    , Ticket (..)
    , TicketField (..)
    , TicketFieldId (..)
    , TicketFieldValue (..)
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
    , ExportFromTime (..)
    , ZendeskAPIUrl (..)
    , parseComments
    , parseTicket
    , renderTicketStatus
    -- * General configuration
    , App
    , Config (..)
    , DataLayer (..)
    , HTTPNetworkLayer (..)
    , IOLayer (..)
    , DBLayer (..)
    , knowledgebasePath
    , tokenPath
    , assignToPath
    , asksDataLayer
    , asksHTTPNetworkLayer
    , asksIOLayer
    , asksDBLayer
    , showURL
    , runApp
    ) where

import           Universum

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Trans.Control (MonadBaseControl (..))

import           UnliftIO (MonadUnliftIO)

import           Data.Aeson (FromJSON, ToJSON, Value (Object), object, parseJSON, toJSON,
                             withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types (Parser)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (POSIXTime)
import           Network.HTTP.Simple (Request)

import           LogAnalysis.Types (Knowledge)

import           Test.QuickCheck (Arbitrary (..), elements, listOf1, vectorOf)

------------------------------------------------------------
-- Configuration
------------------------------------------------------------

newtype App a = App { runAppBase :: ReaderT Config IO a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadReader Config
             , MonadIO
             , MonadBase IO
             , MonadUnliftIO
             , MonadCatch
             , MonadThrow
             )

instance MonadBaseControl IO App where
    type StM App a = a
    liftBaseWith f = App $ liftBaseWith $ \q -> f (q . runAppBase)
    restoreM = App . restoreM

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
    , cfgDataLayer          :: !(DataLayer App)
    -- ^ The Zendesk API layer. We will ideally move this into a
    -- separate configuration containing all the layer (yes, there a couple of them).
    , cfgIOLayer            :: !(IOLayer App)
    -- ^ The _IO@ layer. This is containing all the functions we have for IO.
    , cfgDBLayer            :: !(DBLayer App)
    -- ^ The _DB_ layer. This is containing all the modification functions.
    -- TODO(ks): @Maybe@ db layer. It's not really required.
    , cfgHTTPNetworkLayer   :: !HTTPNetworkLayer
    -- ^ The HTTP network layer. Required so we can mock out the responses and separate
    -- the specific HTTP communication (library) from the code that uses it.
    }


-- | Utility function for getting a function of the @DataLayer@.
asksDataLayer
    :: forall m a. (MonadReader Config m)
    => (DataLayer App -> a)
    -> m a
asksDataLayer getter = do
    Config{..} <- ask
    pure $ getter cfgDataLayer

-- | Utility function for getting a function of the @cfgHTTPNetworkLayer@.
asksHTTPNetworkLayer
    :: forall m a. (MonadReader Config m)
    => (HTTPNetworkLayer -> a)
    -> m a
asksHTTPNetworkLayer getter = do
    Config{..} <- ask
    pure $ getter cfgHTTPNetworkLayer

-- | Utility function for getting a function of the @ZendeskLayer@.
asksIOLayer
    :: forall m a. (MonadReader Config m)
    => (IOLayer App -> a)
    -> m a
asksIOLayer getter = do
    Config{..} <- ask
    pure $ getter cfgIOLayer

-- | Utility function for getting a function of the @DBLayer@.
asksDBLayer
    :: forall m a. (MonadReader Config m)
    => (DBLayer App -> a)
    -> m a
asksDBLayer getter = do
    Config{..} <- ask
    pure $ getter cfgDBLayer

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
data DataLayer m = DataLayer
    { zlGetTicketInfo         :: TicketId         -> m (Maybe TicketInfo)
    , zlListDeletedTickets    ::                     m [DeletedTicket]
    , zlListRequestedTickets  :: UserId           -> m [TicketInfo]
    , zlListAssignedTickets   :: UserId           -> m [TicketInfo]
    , zlListUnassignedTickets ::                     m [TicketInfo]
    , zlListAdminAgents       ::                     m [User]
    , zlGetTicketComments     :: TicketId         -> m [Comment]
    , zlGetAttachment         :: Attachment       -> m (Maybe AttachmentContent)
    , zlPostTicketComment     :: TicketInfo
                              -> ZendeskResponse
                              -> m ()
    , zlExportTickets         :: ExportFromTime   -> m [TicketInfo]
    }

-- | The HTTP network layer that we want to expose.
-- We don't want anything to leak out, so we expose only the most relevant information,
-- anything relating to how it internaly works should NOT be exposed.
-- Actually, it would be better if we "inject" this into
-- the @Http@ module, say in the @ZendeskaLayer@, but it's good enough for now.
-- We need to use RankNTypes due to some complications that appeared.
data HTTPNetworkLayer = HTTPNetworkLayer
    { hnlAddJsonBody    :: forall a.    (ToJSON a) => a -> Request -> Request
    -- TODO(ks): These two below are basically the same, will fix in future PR, requires refactoring.
    , hnlApiCall        :: forall m a.  (MonadIO m, FromJSON a) => (Value -> Parser a) -> Request -> m a
    , hnlApiCallSafe    :: forall m a.  (MonadIO m, FromJSON a) => (Value -> Parser a) -> Request -> m (Either String a)
    }

-- | The IOLayer interface that we can expose.
-- We want to do this since we want to be able to mock out any function tied to @IO@.
data IOLayer m = IOLayer
    { iolAppendFile :: FilePath -> Text -> m ()
    , iolPrintText  :: Text -> m ()
    , iolReadFile   :: FilePath -> m Text
    , iolLogDebug   :: Text -> m ()
    , iolLogInfo    :: Text -> m ()
    }

-- | The @DBLayer@ for the database modifications.
-- TODO(ks): We should remove all these void functions with
-- some return values we can test (and stub out).
data DBLayer m = DBLayer
    { dlInsertTicketInfo         :: TicketInfo -> m ()
    , dlInsertTicketComments     :: TicketId -> Comment -> m ()
    , dlInsertCommentAttachments :: Comment -> Attachment -> m ()
    , dlDeleteCommentAttachments :: m ()
    , dlDeleteTicketComments     :: m ()
    , dlDeleteTickets            :: m ()
    , dlDeleteAllData            :: m ()
    , dlCreateSchema             :: m ()
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

instance ToURL ExportFromTime where
    toURL (ExportFromTime time) = show @_ @Integer . floor . toRational $ time

data ZendeskAPIUrl
    = AgentGroupURL
    | DeletedTicketsURL
    | UserRequestedTicketsURL UserId
    | UserAssignedTicketsURL UserId
    | UserUnassignedTicketsURL
    | TicketsURL TicketId
    | TicketAgentURL TicketId
    | UserInfoURL
    | TicketCommentsURL TicketId
    | ExportDataByTimestamp ExportFromTime
    deriving (Eq, Generic)

showURL :: ZendeskAPIUrl -> Text
showURL AgentGroupURL                       = "/users.json?role%5B%5D=admin&role%5B%5D=agent"
showURL DeletedTicketsURL                   = "/deleted_tickets.json"
showURL (UserRequestedTicketsURL userId)    = "/users/" <> toURL userId <> "/tickets/requested.json"
showURL (UserAssignedTicketsURL userId)     = "/users/" <> toURL userId <> "/tickets/assigned.json"
showURL (UserUnassignedTicketsURL)          = "/search.json?query=" <> urlEncode "type:ticket assignee:none" <> "&sort_by=created_at&sort_order=asc"
showURL (TicketsURL ticketId)               = "/tickets/" <> toURL ticketId <> ".json"
showURL (TicketAgentURL ticketId)           = "https://iohk.zendesk.com/agent/tickets/" <> toURL ticketId
showURL (UserInfoURL)                       = "/users/me.json"
showURL (TicketCommentsURL ticketId)        = "/tickets/" <> toURL ticketId <> "/comments.json"
showURL (ExportDataByTimestamp time)        = "https://iohk.zendesk.com/api/v2/incremental/tickets.json?start_time=" <> toURL time

-- | Plain @Text@ to @Text@ encoding.
-- https://en.wikipedia.org/wiki/Percent-encoding
urlEncode :: Text -> Text
urlEncode url = T.concatMap encodeChar url
  where
    encodeChar :: Char -> Text
    encodeChar '!'  = T.pack "%21"
    encodeChar '#'  = T.pack "%23"
    encodeChar '$'  = T.pack "%24"
    encodeChar '&'  = T.pack "%26"
    encodeChar '\'' = T.pack "%27"
    encodeChar '('  = T.pack "%28"
    encodeChar ')'  = T.pack "%29"
    encodeChar '*'  = T.pack "%2A"
    encodeChar '+'  = T.pack "%2B"
    encodeChar ','  = T.pack "%2C"
    encodeChar '/'  = T.pack "%2F"
    encodeChar ':'  = T.pack "%3A"
    encodeChar ';'  = T.pack "%3B"
    encodeChar '='  = T.pack "%3D"
    encodeChar '?'  = T.pack "%3F"
    encodeChar '@'  = T.pack "%40"
    encodeChar '['  = T.pack "%5B"
    encodeChar ']'  = T.pack "%5D"
    encodeChar '\n' = T.pack "%0A"
    encodeChar ' '  = T.pack "%20"
    encodeChar '"'  = T.pack "%22"
    encodeChar '%'  = T.pack "%25"
    encodeChar '-'  = T.pack "%2D"
    encodeChar '.'  = T.pack "%2E"
    encodeChar '<'  = T.pack "%3C"
    encodeChar '>'  = T.pack "%3E"
    encodeChar '\\' = T.pack "%5C"
    encodeChar '^'  = T.pack "%5E"
    encodeChar '_'  = T.pack "%5F"
    encodeChar '`'  = T.pack "%60"
    encodeChar '{'  = T.pack "%7B"
    encodeChar '|'  = T.pack "%7C"
    encodeChar '}'  = T.pack "%7D"
    encodeChar '~'  = T.pack "%7E"
    encodeChar char = T.singleton char

------------------------------------------------------------
-- Types
------------------------------------------------------------

newtype AttachmentId = AttachmentId
    { getAttachmentId :: Int
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

-- TODO(ks): Arbitrary log contents?
newtype AttachmentContent = AttachmentContent
    { getAttachmentContent :: LByteString
    } deriving (Eq, Show, Ord, Generic, Monoid, Semigroup)

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
    , zrTags     :: !TicketTags
    , zrIsPublic :: !Bool
    } deriving (Eq, Show)

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
    { tComment     :: !Comment
    -- ^ Ticket comment
    , tTag         :: !TicketTags
    , tField       :: ![TicketField]
    , tCustomField :: ![TicketField]
    -- ^ Tags attached to ticket
    }

newtype TicketFieldId = TicketFieldId
    { getTicketFieldId :: Integer
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

newtype TicketFieldValue = TicketFieldValue
    { getTicketFieldValue :: Text
    } deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

data TicketField = TicketField
    { tfId    :: TicketFieldId
    , tfValue :: Maybe TicketFieldValue
    } deriving (Eq, Show, Ord)

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
    , tiField       :: ![TicketField]   -- ^ Custom field (e.g. Priority, Category)
    , tiCustomField :: ![TicketField]   -- ^ No idea why but there's two fields..
    } deriving (Eq, Show, Generic)

-- | Ticket tag
-- TODO(ks): @Generic@ type migrations. Also possible to provide the version from runtime,
-- we need to weigh these options later on.
data TicketTag
    = AnalyzedByScript      -- ^ Ticket has been analyzed
    | AnalyzedByScriptV1_0  -- ^ Ticket has been analyzed by the version 1.0
    | AnalyzedByScriptV1_1  -- ^ Ticket has been analyzed by the version 1.1
    | AnalyzedByScriptV1_2  -- ^ Ticket has been analyzed by the version 1.2
    | ToBeAnalyzed          -- ^ Ticket needs to be analyzed
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

-- Ideally, we might add more fields here, for now it's good enough.
data DeletedTicket = DeletedTicket
    { dtId      :: !TicketId
    } deriving (Eq, Show, Generic)

data PageResultList a = PageResultList
    { prlResults  :: ![a]
    , prlNextPage :: !(Maybe Text)
    , prlCount    :: !(Maybe Int)
    }

newtype ExportFromTime = ExportFromTime
    { getExportFromTime :: POSIXTime
    } deriving (Eq, Show, Ord, Generic)

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

instance Arbitrary TicketFieldId where
    arbitrary = TicketFieldId <$> arbitrary

instance Arbitrary TicketFieldValue where
    arbitrary = TicketFieldValue . fromString <$> arbitrary

instance Arbitrary TicketField where
    arbitrary = TicketField
        <$> arbitrary
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
        pure . TicketURL . T.pack $ protocol ++ name ++ domain

instance Arbitrary TicketInfo where
    arbitrary = do
        ticketId            <- arbitrary
        ticketRequesterId   <- arbitrary
        ticketAssigneeId    <- arbitrary
        ticketUrl           <- arbitrary
        ticketTags          <- arbitrary
        ticketStatus        <- arbitrary
        ticketField         <- arbitrary
        ticketCustomField   <- arbitrary


        pure TicketInfo
            { tiId          = ticketId
            , tiRequesterId = ticketRequesterId
            , tiAssigneeId  = ticketAssigneeId
            , tiUrl         = ticketUrl
            , tiTags        = ticketTags
            , tiStatus      = ticketStatus
            , tiField       = ticketField
            , tiCustomField = ticketCustomField
            }

instance Arbitrary UserId where
    arbitrary = UserId <$> arbitrary


instance Arbitrary UserEmail where
    arbitrary = do
        address     <- listOf1 $ elements ['a'..'z']
        domain      <- elements ["gmail.com", "yahoo.com", "hotmail.com"]
        pure . UserEmail . T.pack $ address <> "@" <> domain

instance Arbitrary UserName where
    arbitrary = UserName . fromString <$> arbitrary

instance Arbitrary UserURL where
    arbitrary = do
        protocol    <- elements ["http://", "https://"]
        name        <- listOf1 $ elements ['a'..'z']
        domain      <- elements [".com",".com.br",".net",".io"]
        pure . UserURL . T.pack $ protocol ++ name ++ domain

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

instance Arbitrary POSIXTime where
    arbitrary = do
        randomInt <- arbitrary
        pure . abs . fromInteger $ randomInt

instance Arbitrary ExportFromTime where
    arbitrary = ExportFromTime <$> arbitrary

instance Arbitrary DeletedTicket where
    arbitrary = DeletedTicket <$> arbitrary

instance Arbitrary ZendeskResponse where
    arbitrary = do
        zendeskResponseTicketId <- arbitrary
        zendeskResponseComment  <- fromString <$> arbitrary
        zendeskResponseTags     <- arbitrary
        zendeskResponseIsPublic <- arbitrary

        pure ZendeskResponse
            { zrTicketId = zendeskResponseTicketId
            , zrComment  = zendeskResponseComment
            , zrTags     = zendeskResponseTags
            , zrIsPublic = zendeskResponseIsPublic
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

instance FromJSON TicketField where
    parseJSON = withObject "ticket field" $ \o -> do
        ticketFieldId    <- o .: "id"
        ticketFieldValue <- o .: "value"

        pure TicketField
            { tfId    = ticketFieldId
            , tfValue = ticketFieldValue
            }

class FromPageResultList a where
    fromPageResult :: Value -> Parser (PageResultList a)

-- TODO(ks): Okey, we should probably move this into separate types...
-- The confusing thing is - they _ARE_ the same type, I would say ZenDesk API
-- is not properly constructed.
instance FromPageResultList TicketInfo where
    fromPageResult obj = asum
        [ ticketListParser obj
        , resultsTicketsParser obj
        , exportTicketsParser obj
        ]
      where
        -- | The case when we have the simple parser from tickets.
        ticketListParser :: Value -> Parser (PageResultList TicketInfo)
        ticketListParser = withObject "ticketList" $ \o ->
            PageResultList
                <$> o .:    "tickets"
                <*> o .:    "next_page"
                <*> o .:?   "count"

        -- | The case when we get results back from a query using the
        -- search API.
        resultsTicketsParser :: Value -> Parser (PageResultList TicketInfo)
        resultsTicketsParser (Object o) =
            PageResultList
                <$> o .:    "results"
                <*> o .:    "next_page"
                <*> o .:?   "count"
        resultsTicketsParser _ = fail "Cannot parse PageResultList from search API."

        -- | The case when we get results back from a query using the
        -- export API.
        exportTicketsParser :: Value -> Parser (PageResultList TicketInfo)
        exportTicketsParser (Object o) =
            PageResultList
                <$> o .:    "tickets"
                <*> o .:    "next_page"
                <*> o .:?   "count"
        exportTicketsParser _ = fail "Cannot parse PageResultList from search API."


instance FromPageResultList User where
    fromPageResult = withObject "userList" $ \o ->
            PageResultList
                <$> o .:    "users"
                <*> o .:    "next_page"
                <*> o .:?   "count"

instance FromPageResultList DeletedTicket where
    fromPageResult = withObject "deleted_tickets" $ \o ->
            PageResultList
                <$> o .:    "deleted_tickets"
                <*> o .:    "next_page"
                <*> o .:?   "count"

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
        ticketField         <- o .: "fields"
        ticketCustomField   <- o .: "custom_fields"

        pure TicketInfo
            { tiId          = ticketId
            , tiRequesterId = ticketRequesterId
            , tiAssigneeId  = ticketAssigneeId
            , tiUrl         = ticketUrl
            , tiTags        = ticketTags
            , tiStatus      = ticketStatus
            , tiField       = ticketField
            , tiCustomField = ticketCustomField
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

instance FromJSON DeletedTicket where
    parseJSON (Object o) = DeletedTicket <$> o .: "id"
    parseJSON _          = fail "Cannot parse deleted ticket API."

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

instance ToJSON TicketField where
    toJSON (TicketField fid fvalue) =
        object [ "id"               .= fid
               , "value"            .= fvalue
               ]

instance ToJSON Ticket where
    toJSON (Ticket comment tags fields customField) =
        object  [ "ticket" .= object
                    [ "comment"       .= comment
                    , "tags"          .= tags
                    , "fields"        .= fields
                    , "custom_fields" .= customField
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
renderTicketStatus AnalyzedByScriptV1_2 = "analyzed-by-script-v1.2"
renderTicketStatus ToBeAnalyzed         = "to_be_analysed" -- https://iohk.zendesk.com/agent/admin/tags
renderTicketStatus NoKnownIssue         = "no-known-issues"
renderTicketStatus NoLogAttached        = "no-log-files"
