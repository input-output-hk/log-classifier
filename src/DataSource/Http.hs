{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataSource.Http
    ( DataLayer (..)
    , basicDataLayer
    , emptyDataLayer
    , createResponseTicket
    ) where

import           Universum

import           Control.Concurrent.Classy (MonadConc, threadDelay)

import           Control.Monad.Reader (ask)

import           Data.Aeson (parseJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.List (nub)
-- TODO(ks): We should be able to remove this once we untangle the layers and configuration.
import           Network.HTTP.Simple (Request, parseRequest_)

import           Http.Exceptions (HttpNetworkLayerException (..))
import           Http.Layer (HTTPNetworkLayer (..), apiRequest, apiRequestAbsolute)

import           DataSource.Types (Attachment (..), AttachmentContent (..), Comment (..),
                                   CommentBody (..), CommentId (..), Config (..),
                                   DeletedTicket (..), ExportFromTime (..), FromPageResultList (..),
                                   PageResultList (..), Ticket (..), TicketId (..), TicketInfo (..),
                                   TicketTag (..), TicketTags (..), User, UserId (..),
                                   ZendeskAPIUrl (..), ZendeskResponse (..), parseComments,
                                   parseTicket, renderTicketStatus, showURL)

-- ./mitmproxy --mode reverse:https://iohk.zendesk.com -p 4001

------------------------------------------------------------
-- Zendesk layer
------------------------------------------------------------

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

-- | The basic Zendesk layer.
-- The convention:
--   - get returns a single result (wrapped in @Maybe@)
--   - list returns multiple results
--   - post submits a result (maybe PUT?!)
basicDataLayer
    :: (MonadIO m, MonadConc m, MonadCatch m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> DataLayer m
basicDataLayer httpNetworkLayer = DataLayer
    { zlGetTicketInfo           = getTicketInfo httpNetworkLayer
    , zlListDeletedTickets      = listDeletedTickets httpNetworkLayer
    , zlListRequestedTickets    = listRequestedTickets httpNetworkLayer
    , zlListAssignedTickets     = listAssignedTickets httpNetworkLayer
    , zlListUnassignedTickets   = listUnassignedTickets httpNetworkLayer
    , zlListAdminAgents         = listAdminAgents httpNetworkLayer
    , zlPostTicketComment       = postTicketComment httpNetworkLayer
    , zlGetAttachment           = getAttachment httpNetworkLayer
    , zlGetTicketComments       = getTicketComments httpNetworkLayer
    , zlExportTickets           = getExportedTickets httpNetworkLayer
    }

-- | The non-implemented Zendesk layer.
emptyDataLayer :: forall m. (Monad m) => DataLayer m
emptyDataLayer = DataLayer
    { zlGetTicketInfo           = \_     -> error "Not implemented zlGetTicketInfo!"
    , zlListDeletedTickets      =           pure []
    , zlListRequestedTickets    = \_     -> error "Not implemented zlListRequestedTickets!"
    , zlListAssignedTickets     = \_     -> error "Not implemented zlListAssignedTickets!"
    , zlListUnassignedTickets   =           pure []
    , zlListAdminAgents         =           pure []
    , zlPostTicketComment       = \_     -> error "Not implemented zlPostTicketComment!"
    , zlGetAttachment           = \_     -> error "Not implemented zlGetAttachment!"
    , zlGetTicketComments       = \_     -> error "Not implemented zlGetTicketComments!"
    , zlExportTickets           = \_     -> error "Not implemented zlExportTickets!"
    }

------------------------------------------------------------
-- Zendesk functions
------------------------------------------------------------

-- | Get single ticket info.
getTicketInfo
    :: (HasCallStack, MonadIO m, MonadConc m, MonadCatch m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> TicketId
    -> m (Maybe TicketInfo)
getTicketInfo httpNetworkLayer ticketId = do
    cfg <- ask

    let url = showURL $ TicketsURL ticketId
    let req = apiRequest cfg url

    let apiCall = hnlApiCall httpNetworkLayer

    result  <- try $ apiCall parseTicket req

    notFoundExceptionToMaybe result
  where
    -- | We want only @HttpNotFound@ to return @Nothing@, otherwise
    -- propagate the exception.
    notFoundExceptionToMaybe
        :: forall m a. (MonadCatch m)
        => Either HttpNetworkLayerException a
        -> m (Maybe a)
    notFoundExceptionToMaybe result = case result of
        Left exception      -> case exception of
            HttpNotFound _  -> pure Nothing
            otherExceptions -> throwM otherExceptions
        Right ticketInfo    -> pure $ Just ticketInfo

-- | Get ticket's comments
getTicketComments
    :: (HasCallStack, MonadIO m, MonadConc m, MonadCatch m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> TicketId
    -> m [Comment]
getTicketComments httpNetworkLayer tId = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL $ TicketCommentsURL tId
    let req = apiRequest cfg url

    result  <- try $ apiCall parseComments req

    notFoundExceptionToList result
  where
    -- | We want only @HttpNotFound@ to return an empty list, otherwise
    -- propagate the exception.
    -- When a @Ticket@ is not found, we don't have any of the @[Comment]@,
    -- effectivly returning an empty list.
    -- We might include a stricter policy to cause exception when no ticket is
    -- found, but this should be good as well.
    notFoundExceptionToList
        :: forall m a. (MonadCatch m)
        => Either HttpNetworkLayerException [a]
        -> m [a]
    notFoundExceptionToList result = case result of
        Left exception      -> case exception of
            HttpNotFound _  -> pure []
            otherExceptions -> throwM otherExceptions
        Right comments      -> pure comments

-- | Return list of deleted tickets.
listDeletedTickets
    :: forall m. (MonadIO m, MonadConc m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> m [DeletedTicket]
listDeletedTickets httpNetworkLayer = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL $ DeletedTicketsURL
    let req = apiRequest cfg url

    iteratePages (apiCall parseJSON) req

-- | Return list of ticketIds that has been requested by config user.
listRequestedTickets
    :: forall m. (MonadIO m, MonadConc m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> UserId
    -> m [TicketInfo]
listRequestedTickets httpNetworkLayer userId = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL $ UserRequestedTicketsURL userId
    let req = apiRequest cfg url

    iteratePages (apiCall parseJSON) req

-- | Return list of ticketIds that has been assigned by config user.
listAssignedTickets
    :: forall m. (MonadIO m, MonadConc m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> UserId
    -> m [TicketInfo]
listAssignedTickets httpNetworkLayer userId = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL $ UserAssignedTicketsURL userId
    let req = apiRequest cfg url

    iteratePages (apiCall parseJSON) req

-- | Return list of ticketIds that has been unassigned.
listUnassignedTickets
    :: forall m. (MonadIO m, MonadConc m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> m [TicketInfo]
listUnassignedTickets httpNetworkLayer = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL $ UserUnassignedTicketsURL
    let req = apiRequest cfg url

    iteratePages (apiCall parseJSON) req

listAdminAgents
    :: forall m. (MonadIO m, MonadConc m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> m [User]
listAdminAgents httpNetworkLayer = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL AgentGroupURL
    let req = apiRequest cfg url

    iteratePages (apiCall parseJSON) req

-- | Export tickets from Zendesk - https://developer.zendesk.com/rest_api/docs/core/incremental_export
-- NOTE: If count is less than 1000, then stop paginating.
-- Otherwise, use the next_page URL to get the next page of results.
getExportedTickets
    :: forall m. (MonadIO m, MonadConc m, MonadCatch m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> ExportFromTime
    -> m [TicketInfo]
getExportedTickets httpNetworkLayer time = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL $ ExportDataByTimestamp time
    let req = apiRequestAbsolute cfg url

    iterateExportedTicketsWithDelay (apiCall parseJSON) req

-- | Send API request to post comment
postTicketComment
    :: forall m. (MonadIO m, MonadConc m, MonadCatch m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> TicketInfo
    -> ZendeskResponse
    -> m ()
postTicketComment httpNetworkLayer ticketInfo zendeskResponse = do
    cfg <- ask

    let addJsonBody = hnlAddJsonBody httpNetworkLayer
    let apiCall = hnlApiCall httpNetworkLayer

    let responseTicket = createResponseTicket (cfgAgentId cfg) ticketInfo zendeskResponse
    let url  = showURL $ TicketsURL (zrTicketId zendeskResponse)
    let req = addJsonBody responseTicket (apiRequest cfg url)
    void $ apiCall (pure . encodeToLazyText) req

-- | Create response ticket
createResponseTicket :: Integer -> TicketInfo -> ZendeskResponse -> Ticket
createResponseTicket agentId TicketInfo{..} ZendeskResponse{..} =
    let analyzedTag = renderTicketStatus AnalyzedByScriptV1_5_0
        -- Nub so it won't post duplicate tags
        allTags    = nub $ [analyzedTag] <> getTicketTags tiTags <> getTicketTags zrTags
        -- We remove the @ToBeAnalyzed@ tag.
        mergedTags = TicketTags . filter (/= renderTicketStatus ToBeAnalyzed) $ allTags

    in (Ticket
            (Comment (CommentId 0)
                (CommentBody zrComment)
                []
                zrIsPublic
                agentId
            )
            mergedTags
            tiField
            tiCustomField
        )

-- | Get user information.
_getUser
    :: forall m. (MonadIO m, MonadConc m, MonadCatch m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> m User
_getUser httpNetworkLayer = do
    cfg <- ask

    let apiCall = hnlApiCall httpNetworkLayer

    let url = showURL UserInfoURL
    let req = apiRequest cfg url

    apiCall parseJSON req

-- | Given attachmentUrl, return attachment in bytestring
getAttachment
    :: forall m. (MonadIO m, MonadConc m, MonadMask m, MonadReader Config m)
    => HTTPNetworkLayer m
    -> Attachment
    -> m (Maybe AttachmentContent)
getAttachment httpNetworkLayer Attachment{..} = do
    let apiCallBare = hnlApiCallBare httpNetworkLayer
    Just . AttachmentContent <$> apiCallBare (parseRequest_ (toString aURL))

------------------------------------------------------------
-- Utility
------------------------------------------------------------

-- | Iterate all the ticket pages and combine into a result.
iteratePages
    :: forall m a. (MonadIO m, MonadConc m, MonadReader Config m, FromPageResultList a)
    => (Request -> m (PageResultList a))
    -> Request
    -> m [a]
iteratePages apiCall req = iteratePagesWithDelay apiCall 0 req

-- | Iterate all the ticket pages and combine into a result. Wait for
-- some time in-between the requests.
iteratePagesWithDelay
    :: forall m a. (MonadIO m, MonadConc m, MonadReader Config m, FromPageResultList a)
    => (Request -> m (PageResultList a))
    -> Int
    -> Request
    -> m [a]
iteratePagesWithDelay apiCall seconds req = do
    cfg <- ask

    let go :: [a] -> Text -> m [a]
        go list' nextPage' = do
            -- Wait for @Int@ seconds.
            threadDelay $ seconds * 1000000

            let req'      = apiRequestAbsolute cfg nextPage'
            (PageResultList pagen nextPagen _) <- apiCall req'
            case nextPagen of
                Just nextUrl -> go (list' <> pagen) nextUrl
                Nothing      -> pure (list' <> pagen)

    (PageResultList page0 nextPage _) <- apiCall req
    case nextPage of
        Just nextUrl -> go page0 nextUrl
        Nothing      -> pure page0

-- | A specific call which has a condition for finishing up.
-- Seems the most general call is this with the delay enabled.
iterateExportedTicketsWithDelay
    :: forall m a. (MonadIO m, MonadReader Config m, FromPageResultList a)
    => (Request -> m (PageResultList a))
    -> Request
    -> m [a]
iterateExportedTicketsWithDelay apiCall req = do
    cfg <- ask

    let go :: [a] -> Text -> m [a]
        go list' nextPage' = do
            liftIO $ threadDelay $ 10 * 1000000 -- Wait, Zendesk allows for 10 per minute.

            let req'      = apiRequestAbsolute cfg nextPage'
            (PageResultList pagen nextPagen count) <- apiCall req'
            case nextPagen of
                Just nextUrl -> if maybe False (>= 1000) count
                                    then go (list' <> pagen) nextUrl
                                    else pure (list' <> pagen)

                Nothing      -> pure (list' <> pagen)


    (PageResultList page0 nextPage _) <- apiCall req
    case nextPage of
        Just nextUrl -> go page0 nextUrl
        Nothing      -> pure page0

