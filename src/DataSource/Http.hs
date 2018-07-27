{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataSource.Http
    ( basicDataLayer
    , emptyDataLayer
    , createResponseTicket
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Monad.Reader (ask)

import           Data.Aeson (parseJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.List (nub)
import           Network.HTTP.Simple (Request, getResponseBody, httpLBS, parseRequest_)

import           HttpLayer
import           HttpLayer (JSONParsingException (..))
import           DataSource.Types (Attachment (..), AttachmentContent (..), Comment (..),
                                   CommentBody (..), CommentId (..), Config (..),
                                   DeletedTicket (..), ExportFromTime (..), FromPageResultList (..),
                                   PageResultList (..), Ticket (..), TicketId (..), TicketInfo (..),
                                   TicketTag (..), TicketTags (..), User, UserId (..),
                                   ZendeskAPIUrl (..), DataLayer (..), ZendeskResponse (..),
                                   asksHTTPNetworkLayer, parseComments, renderTicketStatus, showURL)

-- ./mitmproxy --mode reverse:https://iohk.zendesk.com -p 4001

------------------------------------------------------------
-- Zendesk layer
------------------------------------------------------------

-- | The basic Zendesk layer.
-- The convention:
--   - get returns a single result (wrapped in @Maybe@)
--   - list returns multiple results
--   - post submits a result (maybe PUT?!)
basicDataLayer :: (MonadIO m, MonadReader Config m, MonadCatch m) => DataLayer m
basicDataLayer = DataLayer
    { zlGetTicketInfo           = getTicketInfo
    , zlListDeletedTickets      = listDeletedTickets
    , zlListRequestedTickets    = listRequestedTickets
    , zlListAssignedTickets     = listAssignedTickets
    , zlListUnassignedTickets   = listUnassignedTickets
    , zlListAdminAgents         = listAdminAgents
    , zlPostTicketComment       = postTicketComment
    , zlGetAttachment           = getAttachment
    , zlGetTicketComments       = getTicketComments
    , zlExportTickets           = getExportedTickets
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
    :: forall m. (MonadIO m, MonadReader Config m, MonadCatch m)
    => TicketId
    -> m (Maybe TicketInfo)
getTicketInfo ticketId =
    catch getInfo $ \(e :: JSONParsingException) -> throwM e
  where
    getInfo :: m (Maybe TicketInfo)
    getInfo = do
        cfg <- ask

        let url = showURL $ TicketsURL ticketId
        let req = apiRequest cfg url

        apiCall <- asksHTTPNetworkLayer hnlApiCall

        Just <$> apiCall parseJSON req

-- | Return list of deleted tickets.
listDeletedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => m [DeletedTicket]
listDeletedTickets = do
    cfg <- ask

    let url = showURL $ DeletedTicketsURL
    let req = apiRequest cfg url

    iteratePages req

-- | Return list of ticketIds that has been requested by config user.
listRequestedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => UserId
    -> m [TicketInfo]
listRequestedTickets userId = do
    cfg <- ask

    let url = showURL $ UserRequestedTicketsURL userId
    let req = apiRequest cfg url

    iteratePages req

-- | Return list of ticketIds that has been assigned by config user.
listAssignedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => UserId
    -> m [TicketInfo]
listAssignedTickets userId = do
    cfg <- ask

    let url = showURL $ UserAssignedTicketsURL userId
    let req = apiRequest cfg url

    iteratePages req

-- | Return list of ticketIds that has been unassigned.
listUnassignedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => m [TicketInfo]
listUnassignedTickets = do
    cfg <- ask

    let url = showURL $ UserUnassignedTicketsURL
    let req = apiRequest cfg url

    iteratePages req

listAdminAgents
    :: forall m. (MonadIO m, MonadReader Config m)
    => m [User]
listAdminAgents = do
    cfg <- ask
    let url = showURL AgentGroupURL
    let req = apiRequest cfg url

    iteratePages req

-- | Export tickets from Zendesk - https://developer.zendesk.com/rest_api/docs/core/incremental_export
-- NOTE: If count is less than 1000, then stop paginating.
-- Otherwise, use the next_page URL to get the next page of results.
getExportedTickets
    :: forall m. (MonadIO m, MonadReader Config m, MonadThrow m)
    => ExportFromTime
    -> m [TicketInfo]
getExportedTickets time = do
    cfg <- ask

    apiCall <- asksHTTPNetworkLayer hnlApiCall

    let url = showURL $ ExportDataByTimestamp time
    let req = apiRequestAbsolute cfg url

    iterateExportedTicketsWithDelay req (apiCall parseJSON)
  where

    iterateExportedTicketsWithDelay
        :: Request
        -> (Request -> m (PageResultList TicketInfo))
        -> m [TicketInfo]
    iterateExportedTicketsWithDelay req apiCall = do
        cfg <- ask

        let go :: [TicketInfo] -> Text -> m [TicketInfo]
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

-- | Send API request to post comment
postTicketComment
    :: (MonadIO m, MonadReader Config m, MonadThrow m)
    => TicketInfo
    -> ZendeskResponse
    -> m ()
postTicketComment ticketInfo zendeskResponse = do
    cfg <- ask

    addJsonBody <- asksHTTPNetworkLayer hnlAddJsonBody
    apiCall     <- asksHTTPNetworkLayer hnlApiCall

    let responseTicket = createResponseTicket (cfgAgentId cfg) ticketInfo zendeskResponse
    let url  = showURL $ TicketsURL (zrTicketId zendeskResponse)
    let req = addJsonBody responseTicket (apiRequest cfg url)
    void $ apiCall (pure . encodeToLazyText) req

-- | Create response ticket
createResponseTicket :: Integer -> TicketInfo -> ZendeskResponse -> Ticket
createResponseTicket agentId TicketInfo{..} ZendeskResponse{..} =
    let analyzedTag = renderTicketStatus AnalyzedByScriptV1_2
    -- Nub so it won't post duplicate tags
        mergedTags = TicketTags . nub $ [analyzedTag] <> getTicketTags tiTags <> getTicketTags zrTags
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
    :: (MonadIO m, MonadReader Config m, MonadThrow m)
    => m User
_getUser = do
    cfg <- ask

    apiCall     <- asksHTTPNetworkLayer hnlApiCall

    let url = showURL UserInfoURL
    let req = apiRequest cfg url

    apiCall parseJSON req

-- | Given attachmentUrl, return attachment in bytestring
getAttachment
    :: (MonadIO m)
    => Attachment
    -> m (Maybe AttachmentContent)
getAttachment Attachment{..} = Just . AttachmentContent . getResponseBody <$> httpLBS req
    where
      req :: Request
      req = parseRequest_ (toString aURL)

-- | Get ticket's comments
getTicketComments
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> m [Comment]
getTicketComments tId = do
    cfg <- ask

    apiCallSafe     <- asksHTTPNetworkLayer hnlApiCallSafe

    let url = showURL $ TicketCommentsURL tId
    let req = apiRequest cfg url

    result          <- apiCallSafe parseComments req

    -- TODO(ks): For now return empty if there is an exception.
    -- After we have exception handling, we propagate this up.
    case result of
        Left _  -> pure []
        Right r -> pure r

------------------------------------------------------------
-- Utility
------------------------------------------------------------

-- | Iterate all the ticket pages and combine into a result.
iteratePages
    :: forall m a. (MonadIO m, MonadReader Config m, FromPageResultList a)
    => Request
    -> m [a]
iteratePages req = iteratePagesWithDelay 0 req

-- | Iterate all the ticket pages and combine into a result. Wait for
-- some time in-between the requests.
iteratePagesWithDelay
    :: forall m a. (MonadIO m, MonadReader Config m, FromPageResultList a)
    => Int
    -> Request
    -> m [a]
iteratePagesWithDelay seconds req = do
    cfg <- ask

    apiCall     <- asksHTTPNetworkLayer hnlApiCall

    let go :: [a] -> Text -> IO [a]
        go list' nextPage' = do
            -- Wait for @Int@ seconds.
            threadDelay $ seconds * 1000000

            let req'      = apiRequestAbsolute cfg nextPage'
            (PageResultList pagen nextPagen _) <- apiCall parseJSON req'
            case nextPagen of
                Just nextUrl -> go (list' <> pagen) nextUrl
                Nothing      -> pure (list' <> pagen)

    (PageResultList page0 nextPage _) <- liftIO $ apiCall parseJSON req
    case nextPage of
        Just nextUrl -> liftIO $ go page0 nextUrl
        Nothing      -> pure page0
