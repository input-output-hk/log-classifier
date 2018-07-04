{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataSource.Http
    ( basicZendeskLayer
    , emptyZendeskLayer
    , basicIOLayer
    , defaultConfig
    , createResponseTicket
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Monad.Reader (ask)

import           Data.Aeson (FromJSON, ToJSON, Value, encode, parseJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.List (nub)
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpJSON, httpLBS,
                                      parseRequest_, setRequestBasicAuth, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath)

import           DataSource.Types (Attachment (..), AttachmentContent (..), Comment (..),
                                   CommentBody (..), CommentId (..), Config (..),
                                   DeletedTicket (..), ExportFromTime (..), FromPageResultList (..),
                                   IOLayer (..), PageResultList (..), Ticket (..), TicketId (..),
                                   TicketInfo (..), TicketTag (..), TicketTags (..), User, UserId (..),
                                   ZendeskAPIUrl (..), ZendeskLayer (..), ZendeskResponse (..),
                                   parseComments, parseTicket, renderTicketStatus, showURL)

-- ./mitmproxy --mode reverse:https://iohk.zendesk.com -p 4001

-- | The default configuration.
defaultConfig :: Config
defaultConfig =
    Config
        { cfgAgentId            = 0
        , cfgZendesk            = "https://iohk.zendesk.com"
        , cfgToken              = ""
        , cfgEmail              = "kristijan.saric@iohk.io"
        , cfgAssignTo           = 0
        , cfgKnowledgebase      = []
        , cfgNumOfLogsToAnalyze = 5
        , cfgIsCommentPublic    = True -- TODO(ks): For now, we need this in CLI.
        , cfgZendeskLayer       = basicZendeskLayer
        , cfgIOLayer            = basicIOLayer
        }

-- | The basic Zendesk layer.
-- The convention:
--   - get returns a single result (wrapped in @Maybe@)
--   - list returns multiple results
--   - post submits a result (maybe PUT?!)
basicZendeskLayer :: (MonadIO m, MonadReader Config m) => ZendeskLayer m
basicZendeskLayer = ZendeskLayer
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

basicIOLayer :: (MonadIO m, MonadReader Config m) => IOLayer m
basicIOLayer = IOLayer
    { iolAppendFile             = appendFile
    , iolPrintText              = putTextLn
    , iolReadFile               = \_ -> error "Not implemented readFile!"
    -- ^ TODO(ks): We need to implement this!
    }

-- | The non-implemented Zendesk layer.
emptyZendeskLayer :: forall m. (Monad m) => ZendeskLayer m
emptyZendeskLayer = ZendeskLayer
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

-- | Get single ticket info.
getTicketInfo
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> m (Maybe TicketInfo)
getTicketInfo ticketId = do
    cfg <- ask

    let url = showURL $ TicketsURL ticketId
    let req = apiRequest cfg url
    liftIO $ Just <$> apiCall parseTicket req

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
    :: forall m. (MonadIO m, MonadReader Config m)
    => ExportFromTime
    -> m [TicketInfo]
getExportedTickets time = do
    cfg <- ask
    let url = showURL $ ExportDataByTimestamp time
    putTextLn $ "/api/v2" <> url
    let req = apiRequestAbsolute cfg url
    putTextLn "OUT!"
    iterateExportedTicketsWithDelay req
  where

    iterateExportedTicketsWithDelay
        :: Request
        -> m [TicketInfo]
    iterateExportedTicketsWithDelay req = do
        cfg <- ask

        let go :: [TicketInfo] -> Text -> IO [TicketInfo]
            go list' nextPage' = do
                threadDelay $ 10 * 1000000 -- Wait, Zendesk allows for 10 per minute.

                let req'      = apiRequestAbsolute cfg nextPage'
                (PageResultList pagen nextPagen count) <- apiCall parseJSON req'
                case nextPagen of
                    Just nextUrl -> if maybe False (>= 1000) count
                                        then go (list' <> pagen) nextUrl
                                        else pure (list' <> pagen)

                    Nothing      -> pure (list' <> pagen)


        (PageResultList page0 nextPage _) <- liftIO $ apiCall parseJSON req
        putTextLn $ show page0
        putTextLn $ show nextPage
        case nextPage of
            Just nextUrl -> liftIO $ go page0 nextUrl
            Nothing      -> pure page0

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


-- | Send API request to post comment
postTicketComment
    :: (MonadIO m, MonadReader Config m)
    => TicketInfo
    -> ZendeskResponse
    -> m ()
postTicketComment ticketInfo zendeskResponse = do
    cfg <- ask
    let responseTicket = createResponseTicket (cfgAgentId cfg) ticketInfo zendeskResponse
    let url  = showURL $ TicketsURL (zrTicketId zendeskResponse)
    let req = addJsonBody responseTicket (apiRequest cfg url)
    void $ liftIO $ apiCall (pure . encodeToLazyText) req

-- | Create response ticket
createResponseTicket :: Integer -> TicketInfo -> ZendeskResponse -> Ticket
createResponseTicket agentId TicketInfo{..} ZendeskResponse{..} =
    let analyzedTag = renderTicketStatus AnalyzedByScriptV1_1
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
    :: (MonadIO m, MonadReader Config m)
    => m User
_getUser = do
    cfg <- ask

    let url = showURL UserInfoURL
    let req = apiRequest cfg url

    liftIO $ apiCall parseJSON req

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

    let url = showURL $ TicketCommentsURL tId
    let req = apiRequest cfg url

    result  <- apiCallSafe parseComments req

    -- TODO(ks): For now return empty if there is an exception.
    -- After we have exception handling, we propagate this up.
    case result of
        Left _  -> pure []
        Right r -> pure r

------------------------------------------------------------
-- HTTP utility
------------------------------------------------------------

-- | Request PUT
addJsonBody :: ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | Make an api call
-- TODO(ks): Switch to @Either@.
apiCall :: FromJSON a => (Value -> Parser a) -> Request -> IO a
apiCall parser req = do
    putTextLn $ show req
    v <- getResponseBody <$> httpJSON req
    case parseEither parser v of
        Right o -> pure o
        Left e -> error $ "couldn't parse response "
            <> toText e <> "\n" <> decodeUtf8 (encode v)

-- | Make a safe api call.
apiCallSafe
    :: forall m a. (MonadIO m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m (Either String a)
apiCallSafe parser req = do
    putTextLn $ show req
    v <- getResponseBody <$> httpJSON req
    pure $ parseEither parser v

-- | General api request function
apiRequest :: Config -> Text -> Request
apiRequest Config{..} u = setRequestPath (encodeUtf8 path) $
                          addRequestHeader "Content-Type" "application/json" $
                          setRequestBasicAuth
                              (encodeUtf8 cfgEmail <> "/token")
                              (encodeUtf8 cfgToken) $
                          parseRequest_ (toString (cfgZendesk <> path))
                        where
                          path :: Text
                          path = "/api/v2" <> u

-- | Api request but use absolute path
apiRequestAbsolute :: Config -> Text -> Request
apiRequestAbsolute Config{..} u = addRequestHeader "Content-Type" "application/json" $
                                  setRequestBasicAuth
                                      (encodeUtf8 cfgEmail <> "/token")
                                      (encodeUtf8 cfgToken) $
                                  parseRequest_ (toString u)
