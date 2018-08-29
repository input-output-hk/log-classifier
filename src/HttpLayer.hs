module HttpLayer
    ( HTTPNetworkLayer (..)
    , HttpNetworkLayerException (..)
    , basicHTTPNetworkLayer
    , emptyHTTPNetworkLayer
    , apiRequest
    , apiRequestAbsolute
    ) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON, Value)
import           Data.Aeson.Types (Parser, parseEither)
import           Network.HTTP.Simple (Request, addRequestHeader, defaultRequest, getResponseBody,
                                      getResponseStatusCode, httpJSONEither, parseRequest_,
                                      setRequestBasicAuth, setRequestBodyJSON, setRequestMethod,
                                      setRequestPath)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))

import           Test.QuickCheck (Arbitrary (..), oneof)

------------------------------------------------------------
-- Layer
------------------------------------------------------------

basicHTTPNetworkLayer :: HTTPNetworkLayer
basicHTTPNetworkLayer = HTTPNetworkLayer
    { hnlAddJsonBody    = addJsonBody
    , hnlApiCall        = apiCall
    }

emptyHTTPNetworkLayer :: HTTPNetworkLayer
emptyHTTPNetworkLayer = HTTPNetworkLayer
    { hnlAddJsonBody    = \_ _  -> error "hnlAddJsonBody not implemented!"
    , hnlApiCall        = \_    -> error "hnlApiCall not implemented!"
    }

------------------------------------------------------------
-- Exceptions
------------------------------------------------------------

data HttpNetworkLayerException
    = HttpDecodingJSON Request String
    -- 4XX
    | HttpBadRequest Request
    | HttpUnauthorized Request
    | HttpForbidden Request
    | HttpNotFound Request
    | HttpMethodNotAllowed Request
    | HttpUnsupportedMediaType Request
    | HttpTooManyRequests Request
    -- 5XX
    | HttpInternalServerError Request
    | HttpNotImplemented Request
    | HttpServiceUnavailable Request
    deriving (Show)


-- | The way to convert the status codes to exceptions.
statusCodeToException :: forall m. (MonadThrow m) => Request -> Int -> m ()
statusCodeToException request = \case
    400 -> throwM $ HttpBadRequest request
    401 -> throwM $ HttpUnauthorized request
    403 -> throwM $ HttpForbidden request
    404 -> throwM $ HttpNotFound request
    405 -> throwM $ HttpMethodNotAllowed request
    415 -> throwM $ HttpUnsupportedMediaType request
    429 -> throwM $ HttpTooManyRequests request

    500 -> throwM $ HttpInternalServerError request
    501 -> throwM $ HttpNotImplemented request
    503 -> throwM $ HttpServiceUnavailable request

    _   -> pure ()


instance Exception HttpNetworkLayerException

-- | TODO(ks): Good enough for our purposes
instance Arbitrary Request where
    arbitrary = pure defaultRequest

instance Arbitrary HttpNetworkLayerException where
    arbitrary = oneof
        [ HttpDecodingJSON <$> arbitrary <*> arbitrary

        , HttpBadRequest <$> arbitrary
        , HttpUnauthorized  <$> arbitrary
        , HttpForbidden  <$> arbitrary
        , HttpNotFound  <$> arbitrary
        , HttpMethodNotAllowed  <$> arbitrary
        , HttpUnsupportedMediaType  <$> arbitrary
        , HttpTooManyRequests  <$> arbitrary

        , HttpInternalServerError  <$> arbitrary
        , HttpNotImplemented  <$> arbitrary
        , HttpServiceUnavailable  <$> arbitrary
        ]

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

-- | Request PUT
addJsonBody :: forall a. ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | Make an api call
apiCall
    :: forall m a. (MonadIO m, MonadCatch m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m a
apiCall parser request = do
    -- putTextLn $ show req -- logging !?!
    httpResult      <- httpJSONEither request

    httpResponse    <-  either
                            (throwM . HttpDecodingJSON request . show)
                            pure
                            (getResponseBody httpResult)

    let httpStatusCode = getResponseStatusCode httpResult

    _               <- statusCodeToException request httpStatusCode

    case parseEither parser httpResponse of
        Left reason -> throwM $ HttpDecodingJSON request reason
        Right value -> pure value

