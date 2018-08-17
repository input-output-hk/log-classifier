module HttpLayer
    ( HTTPNetworkLayer (..)
    , JSONException (..)
    , basicHTTPNetworkLayer
    , emptyHTTPNetworkLayer
    , apiRequest
    , apiRequestAbsolute
    ) where

import           Universum

import           Control.Exception.Safe (Handler (..), catches)
import           Data.Aeson (FromJSON, ToJSON, Value, encode)
import           Data.Aeson.Types (Parser, parseEither)
import           Network.HTTP.Client.Conduit (HttpException (..), parseUrlThrow)
import           Network.HTTP.Simple (JSONException (..), Request, addRequestHeader,
                                      getResponseBody, httpJSON, parseRequest_, setRequestBasicAuth,
                                      setRequestBodyJSON, setRequestMethod, setRequestPath)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))

------------------------------------------------------------
-- Layer
------------------------------------------------------------

basicHTTPNetworkLayer :: HTTPNetworkLayer
basicHTTPNetworkLayer = HTTPNetworkLayer
    { hnlAddJsonBody    = addJsonBody
    , hnlApiCall        = apiCall
    , hnlApiCallSafe    = apiCallSafe
    }

emptyHTTPNetworkLayer :: HTTPNetworkLayer
emptyHTTPNetworkLayer = HTTPNetworkLayer
    { hnlAddJsonBody    = \_ _  -> error "hnlAddJsonBody not implemented!"
    , hnlApiCall        = \_    -> error "hnlApiCall not implemented!"
    , hnlApiCallSafe    = \_ _  -> error "hnlApiCallSafe not implemented!"
    }

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | General api request function
apiRequest
    :: Config
    -> Text
    -> Either String Request
apiRequest Config{..} u = showErr $ catches buildRequest handlerList
  where
    buildRequest :: forall m. MonadCatch m => m Request
    buildRequest = do
        req <- parseUrlThrow (toString (cfgZendesk <> path)) -- TODO(md): Get a list of exceptions that parseUrlThrow can throw
        return $ setRequestPath (encodeUtf8 path) $
                 addRequestHeader "Content-Type" "application/json" $
                 setRequestBasicAuth
                     (encodeUtf8 cfgEmail <> "/token")
                     (encodeUtf8 cfgToken) req
    handlerList :: MonadCatch m => [Handler m Request]
    handlerList = [
                    handlerJSON
                  , handlerHTTP
                  ]
    handlerJSON :: MonadCatch m => Handler m Request
    handlerJSON = Handler $ \(ex :: JSONException) -> error $ show ex
    handlerHTTP :: MonadCatch m => Handler m Request
    handlerHTTP = Handler $ \(ex :: HttpException) -> error $ show ex
    path :: Text
    path = "/api/v2" <> u
    showErr :: Either SomeException Request -> Either String Request
    showErr (Left x)  = Left (show x)
    showErr (Right x) = Right x


-- | Api request but use absolute path
apiRequestAbsolute
    :: Config
    -> Text
    -> Either String Request
apiRequestAbsolute Config{..} u =
    Right <$> addRequestHeader "Content-Type" "application/json" $
    setRequestBasicAuth (encodeUtf8 cfgEmail <> "/token") (encodeUtf8 cfgToken) $
    parseRequest_(toString u)

-- | Request PUT
addJsonBody :: forall a. ToJSON a => a -> Request -> Either String Request
addJsonBody body req = showErr $ catch updateReq Left
  where
    updateReq :: Exception e => Either e Request
    updateReq = Right <$> setRequestBodyJSON body $ setRequestMethod "PUT" req
    showErr :: Either SomeException Request -> Either String Request
    showErr (Left x)  = Left (show x)
    showErr (Right x) = Right x


-- | Make an api call
-- TODO(ks): Switch to @Either@.
apiCall
    :: forall m a. (MonadIO m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m (Either String a)
apiCall parser req = do
    putTextLn $ show req
    v <- getResponseBody <$> httpJSON req
    case parseEither parser v of
        Right o -> pure $ Right o
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
