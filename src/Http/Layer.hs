module Http.Layer
    ( HTTPNetworkLayer (..)
    , basicHTTPNetworkLayer
    , emptyHTTPNetworkLayer
    , apiRequest
    , apiRequestAbsolute
    ) where

import           Universum

import qualified Data.ByteString.Lazy as BL

import           Data.Aeson (FromJSON, ToJSON, Value)
import           Data.Aeson.Types (Parser, parseEither)
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody,
                                      getResponseStatusCode, httpJSONEither, httpLBS, parseRequest_,
                                      setRequestBasicAuth, setRequestBodyJSON, setRequestMethod,
                                      setRequestPath)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))

import           Http.Exceptions (HttpNetworkLayerException (..), statusCodeToException)
import           Http.Queue (ShedulerConfig, runDispatch)

------------------------------------------------------------
-- Layer
------------------------------------------------------------

basicHTTPNetworkLayer :: ShedulerConfig -> HTTPNetworkLayer
basicHTTPNetworkLayer shedulerConfig = HTTPNetworkLayer
    { hnlAddJsonBody    = addJsonBody
    , hnlApiCall        = apiCall shedulerConfig
    , hnlApiCallBare    = apiCallBare shedulerConfig
    }

emptyHTTPNetworkLayer :: HTTPNetworkLayer
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

------------------------------------------------------------
-- Layer functions
------------------------------------------------------------

-- | Request PUT
addJsonBody :: forall a. ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | Make an api call
apiCall
    :: forall m a. (MonadIO m, MonadCatch m, MonadMask m, FromJSON a)
    => ShedulerConfig
    -> (Value -> Parser a)
    -> Request
    -> m a
apiCall shedulerConfig parser request = do
    -- putTextLn $ show req -- logging !?!
    httpResult      <- runDispatch shedulerConfig (httpJSONEither request)

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
    :: forall m. (MonadIO m, MonadMask m)
    => ShedulerConfig
    -> Request
    -> m BL.ByteString
apiCallBare shedulerConfig request = do
    response    <- runDispatch shedulerConfig (httpLBS request)
    pure $ getResponseBody response


