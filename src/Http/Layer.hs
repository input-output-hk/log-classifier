{-# LANGUAGE RankNTypes #-}

module Http.Layer
    ( HTTPNetworkLayer (..)
    , basicHTTPNetworkLayer
    , emptyHTTPNetworkLayer
    , apiRequest
    , apiRequestAbsolute
    ) where

import           Universum

import           Control.Concurrent.Classy (MonadConc)

import qualified Data.ByteString.Lazy as BL

import           Data.Aeson (FromJSON, ToJSON, Value)
import           Data.Aeson.Types (Parser, parseEither)
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody,
                                      getResponseStatusCode, httpJSONEither, httpLBS, parseRequest_,
                                      setRequestBasicAuth, setRequestBodyJSON, setRequestMethod,
                                      setRequestPath)

import           DataSource.Types (Config (..))

import           Http.Exceptions (HttpNetworkLayerException (..), statusCodeToException)
import           Http.Queue (SchedulerConfig, runDispatch)

------------------------------------------------------------
-- Layer
------------------------------------------------------------

-- | The HTTP network layer that we want to expose.
-- We don't want anything to leak out, so we expose only the most relevant information,
-- anything relating to how it internaly works should NOT be exposed.
-- Actually, it would be better if we "inject" this into
-- the @Http@ module, say in the @ZendeskaLayer@, but it's good enough for now.
-- We need to use RankNTypes due to some complications that appeared.
data HTTPNetworkLayer m = HTTPNetworkLayer
    { hnlAddJsonBody    :: forall a.    (ToJSON a)      => a -> Request -> Request
    , hnlApiCall        :: forall a.    (FromJSON a)    => (Value -> Parser a) -> Request -> m a
    , hnlApiCallBare    :: Request -> m BL.ByteString
    }

-- | Basic Http network layer
basicHTTPNetworkLayer
    :: forall m. (MonadIO m, MonadCatch m, MonadMask m, MonadConc m)
    => SchedulerConfig m
    -> HTTPNetworkLayer m
basicHTTPNetworkLayer shedulerConfig = HTTPNetworkLayer
    { hnlAddJsonBody    = addJsonBody
    , hnlApiCall        = apiCall shedulerConfig
    , hnlApiCallBare    = apiCallBare shedulerConfig
    }
  where

    -- | Request PUT
    addJsonBody :: forall a. ToJSON a => a -> Request -> Request
    addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

    -- | Make an api call
    apiCall
        :: forall a. (FromJSON a)
        => SchedulerConfig m
        -> (Value -> Parser a)
        -> Request
        -> m a
    apiCall schedulerConfig parser request = do
        -- putTextLn $ show req -- logging !?!
        httpResult      <- runDispatch schedulerConfig (httpJSONEither request)

        let httpResponseBody = getResponseBody httpResult

        httpResponse    <-  either
                                (throwM . HttpDecodingJSON request . show)
                                pure
                                httpResponseBody

        let httpStatusCode  = getResponseStatusCode httpResult

        _               <- statusCodeToException request httpStatusCode

        case parseEither parser httpResponse of
            Left reason -> throwM $ HttpDecodingJSON request reason
            Right value -> pure value

    -- | Make a bare api call which returns a lazy @ByteString@.
    apiCallBare
        :: SchedulerConfig m
        -> Request
        -> m BL.ByteString
    apiCallBare schedulerConfig request = do
        response    <- runDispatch schedulerConfig (httpLBS request)
        pure $ getResponseBody response



emptyHTTPNetworkLayer :: HTTPNetworkLayer m
emptyHTTPNetworkLayer = HTTPNetworkLayer
    { hnlAddJsonBody    = \_ _  -> error "hnlAddJsonBody not implemented!"
    , hnlApiCall        = \_    -> error "hnlApiCall not implemented!"
    , hnlApiCallBare    = \_    -> error "hnlApiCallBare not implemented!"
    }

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | General api request function
apiRequest :: Config -> Text -> Request
apiRequest Config{..} u =
    setRequestPath (encodeUtf8 path) $
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
apiRequestAbsolute Config{..} u =
    addRequestHeader "Content-Type" "application/json" $
    setRequestBasicAuth (encodeUtf8 cfgEmail <> "/token") (encodeUtf8 cfgToken) $
    parseRequest_(toString u)

