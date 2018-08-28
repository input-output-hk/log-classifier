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
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpJSON,
                                      parseRequest_, setRequestBasicAuth, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))


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
    = CannotParseResult String -- Request
    deriving (Show)

instance Exception HttpNetworkLayerException

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
    httpResponse <- getResponseBody <$> httpJSON request

    case parseEither parser httpResponse of
        Left reason -> throwM $ CannotParseResult reason
        Right value -> pure value

