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
import           Control.Monad.Trans.Either

import           Data.Aeson (parseJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.List (nub)
import           Universum.String.Conversion (toText)
import           Network.HTTP.Simple (Request, getResponseBody, httpLBS, parseRequest_)

import           HttpLayer (HTTPNetworkLayer (..), apiRequest, apiRequestAbsolute)

import           DataSource.Types (Attachment (..), AttachmentContent (..), Comment (..),
                                   CommentBody (..), CommentId (..), Config (..), DataLayer (..),
                                   DeletedTicket (..), ExportFromTime (..), FromPageResultList (..),
                                   PageResultList (..), Ticket (..), TicketId (..), TicketInfo (..),
                                   TicketTag (..), TicketTags (..), User, UserId (..),
                                   ZendeskAPIUrl (..), ZendeskResponse (..), asksHTTPNetworkLayer,
                                   parseComments, parseTicket, renderTicketStatus, showURL)

-- ./mitmproxy --mode reverse:https://iohk.zendesk.com -p 4001

------------------------------------------------------------
-- Zendesk layer
------------------------------------------------------------

-- | The basic Zendesk layer.
-- The convention:
--   - get returns a single result (wrapped in @Maybe@)
--   - list returns multiple results
--   - post submits a result (maybe PUT?!)
basicDataLayer :: (MonadIO m, MonadReader Config m) => DataLayer m
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
    :: (HasCallStack, MonadIO m, MonadReader Config m)
    => TicketId
    -> m (Maybe TicketInfo)
getTicketInfo ticketId = do
    cfg <- ask

    let url = showURL $ TicketsURL ticketId
    let req = apiRequest cfg url
    case req of
        Left e -> error $ toText e
        Right r -> do
            apiCall <- asksHTTPNetworkLayer hnlApiCall
            rightToMaybe <$> apiCall parseTicket r

-- | Return list of deleted tickets.
listDeletedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => m [DeletedTicket]
listDeletedTickets = do
    cfg <- ask

    let url = showURL $ DeletedTicketsURL
    let req = apiRequest cfg url

    wrapIteratePages req

-- | Return list of ticketIds that has been requested by config user.
listRequestedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => UserId
    -> m [TicketInfo]
listRequestedTickets userId = do
    cfg <- ask

    let url = showURL $ UserRequestedTicketsURL userId
    let req = apiRequest cfg url

    wrapIteratePages req

-- | Return list of ticketIds that has been assigned by config user.
listAssignedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => UserId
    -> m [TicketInfo]
listAssignedTickets userId = do
    cfg <- ask

    let url = showURL $ UserAssignedTicketsURL userId
    let req = apiRequest cfg url

    wrapIteratePages req

-- | Return list of ticketIds that has been unassigned.
listUnassignedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => m [TicketInfo]
listUnassignedTickets = do
    cfg <- ask

    let url = showURL $ UserUnassignedTicketsURL
    let req = apiRequest cfg url

    wrapIteratePages req

listAdminAgents
    :: forall m. (MonadIO m, MonadReader Config m)
    => m [User]
listAdminAgents = do
    cfg <- ask
    let url = showURL AgentGroupURL
    let req = apiRequest cfg url

    wrapIteratePages req

-- | Export tickets from Zendesk - https://developer.zendesk.com/rest_api/docs/core/incremental_export
-- NOTE: If count is less than 1000, then stop paginating.
-- Otherwise, use the next_page URL to get the next page of results.
getExportedTickets
    :: forall m. (MonadIO m, MonadReader Config m)
    => ExportFromTime
    -> m [TicketInfo]
getExportedTickets time = do
    cfg <- ask

    apiCall <- asksHTTPNetworkLayer hnlApiCall

    let url = showURL $ ExportDataByTimestamp time
    let req = apiRequestAbsolute cfg url

    wrappedIterate req (apiCall parseJSON)
  where

    wrappedIterate
        :: Either String Request
        -> (Request -> m (Either String (PageResultList TicketInfo)))
        -> m [TicketInfo]
    wrappedIterate (Left e) _  = error $ toText e
    wrappedIterate (Right r) f = iterateExportedTicketsWithDelay r f

    iterateExportedTicketsWithDelay
        :: Request
        -> (Request -> m (Either String (PageResultList TicketInfo)))
        -> m [TicketInfo]
    iterateExportedTicketsWithDelay req apiCall = do
        cfg <- ask

        let go :: [TicketInfo] -> Text -> m [TicketInfo]
            go list' nextPage' = do
                liftIO $ threadDelay $ 10 * 1000000 -- Wait, Zendesk allows for 10 per minute.

                let req'      = apiRequestAbsolute cfg nextPage'
                case req' of
                    Left e      -> error $ toText e
                    Right req'' -> do
                        req''' <- apiCall req''
                        case req''' of
                            Right (PageResultList pagen nextPagen count) ->
                                case nextPagen of
                                    Just nextUrl -> if maybe False (>= 1000) count
                                                        then go (list' <> pagen) nextUrl
                                                        else pure (list' <> pagen)

                                    Nothing      -> pure (list' <> pagen)
                            Left e -> error $ toText e


        req' <- apiCall req
        case req' of
            Right (PageResultList page0 nextPage _) -> do
                case nextPage of
                    Just nextUrl -> go page0 nextUrl
                    Nothing      -> pure page0
            Left e -> error $ toText e

-- | Send API request to post comment
postTicketComment
    :: (MonadIO m, MonadReader Config m)
    => TicketInfo
    -> ZendeskResponse
    -> m ()
postTicketComment ticketInfo zendeskResponse = do
    cfg <- ask

    addJsonBody <- asksHTTPNetworkLayer hnlAddJsonBody
    apiCall     <- asksHTTPNetworkLayer hnlApiCall

    let responseTicket = createResponseTicket (cfgAgentId cfg) ticketInfo zendeskResponse
    let url  = showURL $ TicketsURL (zrTicketId zendeskResponse)
    res <- runEitherT . hoistEither $ do
        req <- apiRequest cfg url
        addJsonBody responseTicket req
    case res of
        Left e  -> error $ toText e
        Right r -> void $ apiCall (pure . encodeToLazyText) r

-- | Create response ticket
createResponseTicket :: Integer -> TicketInfo -> ZendeskResponse -> Ticket
createResponseTicket agentId TicketInfo{..} ZendeskResponse{..} =
    let analyzedTag = renderTicketStatus AnalyzedByScriptV1_4
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
    :: (MonadIO m, MonadReader Config m)
    => m User
_getUser = do
    cfg <- ask

    apiCall     <- asksHTTPNetworkLayer hnlApiCall

    let url = showURL UserInfoURL
    let req = apiRequest cfg url
    case req of
      Left e  -> error $ toText e
      Right r -> do
          r' <- apiCall parseJSON r
          case r' of
              Left e  -> error $ toText e
              Right u -> pure u

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
    res <- runEitherT $ do
        req <- hoistEither $ apiRequest cfg url
        newEitherT $ apiCallSafe parseComments req
    case res of
        -- TODO(ks): For now return empty if there is an exception.
        -- After we have exception handling, we propagate this up.
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

-- | Wraps a call to iteratePages with error handling for a failed request
wrapIteratePages
    :: (MonadIO m, MonadReader Config m, FromPageResultList a)
    => Either String Request
    -> m [a]
wrapIteratePages = either (error . toText) iteratePages

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
            case req' of
                Left e  -> error $ toText e
                Right req'' -> do
                    req''' <- apiCall parseJSON req''
                    case req''' of
                        Left e  -> error $ toText e
                        Right (PageResultList pagen nextPagen _) ->
                            case nextPagen of
                                Just nextUrl -> go (list' <> pagen) nextUrl
                                Nothing      -> pure (list' <> pagen)

    -- <- liftIO $ apiCall parseJSON req
    req' <- liftIO $ apiCall parseJSON req
    case req' of
        Left e  -> error $ toText e
        Right (PageResultList page0 nextPage _) ->
            case nextPage of
                Just nextUrl -> liftIO $ go page0 nextUrl
                Nothing      -> pure page0
