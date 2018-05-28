{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Zendesk.Functions
    ( basicZendeskLayer
    , emptyZendeskLayer
    , defaultConfig
    ) where

import           Universum

import           Control.Monad.Reader (ask)
import           Data.Aeson (FromJSON, ToJSON, Value, encode, parseJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Parser, parseEither)
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpJSON, httpLBS,
                                      parseRequest_, setRequestBasicAuth, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath)

import           Zendesk.Types (Attachment (..), Comment (..), Config (..), RequestType (..),
                                Ticket (..), TicketId, TicketInfo (..), TicketList (..),
                                TicketTag (..), ZendeskLayer (..), ZendeskResponse (..),
                                parseAgentId, parseComments, parseTickets, renderTicketStatus)


-- | The default configuration.
defaultConfig :: Config
defaultConfig =
    Config
        { cfgAgentId            = 0
        , cfgZendesk            = "https://iohk.zendesk.com"
        , cfgToken              = ""
        , cfgEmail              = "daedalus-bug-reports@iohk.io"
        , cfgAssignTo           = 0
        , cfgKnowledgebase      = []
        , cfgNumOfLogsToAnalyze = 5
        , cfgIsCommentPublic    = True -- TODO(ks): For now, we need this in CLI.
        , cfgZendeskLayer       = basicZendeskLayer
        }

-- | The basic Zendesk layer.
basicZendeskLayer :: (MonadIO m, MonadReader Config m) => ZendeskLayer m
basicZendeskLayer = ZendeskLayer
    { zlGetTicketInfo           = getTicketInfo
    , zlListTickets             = listTickets
    , zlPostTicketComment       = postTicketComment
    , zlGetAgentId              = getAgentId
    , zlGetAttachment           = getAttachment
    , zlGetTicketComments       = getTicketComments
    }

-- | The non-implemented Zendesk layer.
emptyZendeskLayer :: (MonadIO m, MonadReader Config m) => ZendeskLayer m
emptyZendeskLayer = ZendeskLayer
    { zlGetTicketInfo           = error "Not implemented zlGetTicketInfo        !"
    , zlListTickets             = error "Not implemented zlListTickets          !"
    , zlPostTicketComment       = error "Not implemented zlPostTicketComment    !"
    , zlGetAgentId              = error "Not implemented zlGetAgentId           !"
    , zlGetAttachment           = error "Not implemented zlGetAttachment        !"
    , zlGetTicketComments       = error "Not implemented zlGetTicketComments    !"
    }


-- | Get single ticket info.
getTicketInfo
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> m TicketInfo
getTicketInfo ticketId = do
    cfg <- ask

    let req = apiRequest cfg ("tickets/" <> show ticketId <> ".json")
    liftIO $ apiCall parseJSON req

-- | Return list of ticketIds that has been requested by config user (not used)
listTickets
    :: (MonadIO m, MonadReader Config m)
    => RequestType
    -> m [TicketInfo]
listTickets request = do
    cfg <- ask

    let agentId = cfgAgentId cfg
    let url = case request of
                  Requested -> "/users/" <> show agentId <> "/tickets/requested.json"
                  Assigned  -> "/users/" <> show agentId <> "/tickets/assigned.json"
    let req = apiRequest cfg url

    let go :: [TicketInfo] -> Text -> IO [TicketInfo]
        go list' nextPage' = do
          let req' = apiRequestAbsolute cfg nextPage'
          (TicketList pagen nextPagen) <- apiCall parseTickets req'
          case nextPagen of
              Just nextUrl -> go (list' <> pagen) nextUrl
              Nothing      -> pure (list' <> pagen)

    (TicketList page0 nextPage) <- liftIO $ apiCall parseTickets req
    case nextPage of
        Just nextUrl -> liftIO $ go page0 nextUrl
        Nothing      -> pure page0

-- | Send API request to post comment
postTicketComment
    :: (MonadIO m, MonadReader Config m)
    => ZendeskResponse
    -> m ()
postTicketComment ZendeskResponse{..} = do
    cfg <- ask

    let req1 = apiRequest cfg ("tickets/" <> show zrTicketId <> ".json")
    let req2 = addJsonBody
                   (Ticket
                       (Comment ("**Log classifier**\n\n" <> zrComment) [] zrIsPublic (cfgAgentId cfg))
                       (cfgAssignTo cfg)
                       (renderTicketStatus AnalyzedByScriptV1_0:zrTags)
                   )
                   req1
    void $ liftIO $ apiCall (pure . encodeToLazyText) req2

-- | Get agent id that has been set on Config
getAgentId
    :: (MonadIO m, MonadReader Config m)
    => m Integer
getAgentId = do
    cfg <- ask
    let req = apiRequest cfg "users/me.json"
    liftIO $ apiCall parseAgentId req

-- | Given attachmentUrl, return attachment in bytestring
getAttachment
    :: (MonadIO m, MonadReader Config m) -- TODO(ks): We have to fix this
    => Attachment
    -> m LByteString
getAttachment Attachment{..} = getResponseBody <$> httpLBS req
    where
      req :: Request
      req = parseRequest_ (toString aURL)

-- | Get ticket's comments
getTicketComments
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> m [Comment]
getTicketComments tid = do
    cfg <- ask
    let req = apiRequest cfg ("tickets/" <> show tid <> "/comments.json")
    liftIO $ apiCall parseComments req

------------------------------------------------------------
-- HTTP utility
------------------------------------------------------------

-- | Request PUT
addJsonBody :: ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | Make an api call
apiCall :: FromJSON a => (Value -> Parser a) -> Request -> IO a
apiCall parser req = do
    v <- getResponseBody <$> httpJSON req
    case parseEither parser v of
        Right o -> pure o
        Left e -> error $ "couldn't parse response "
            <> toText e <> "\n" <> decodeUtf8 (encode v)

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
                          path = "/api/v2/" <> u

-- | Api request but use absolute path
apiRequestAbsolute :: Config -> Text -> Request
apiRequestAbsolute Config{..} u = addRequestHeader "Content-Type" "application/json" $
                                  setRequestBasicAuth
                                      (encodeUtf8 cfgEmail <> "/token")
                                      (encodeUtf8 cfgToken) $
                                  parseRequest_ (toString u)
