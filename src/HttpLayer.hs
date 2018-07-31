module HttpLayer
    ( HTTPNetworkLayer (..)
    , JSONParsingException (..)
    , basicHTTPNetworkLayer
    , emptyHTTPNetworkLayer
    , apiRequest
    , apiRequestAbsolute
    ) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON, Value, encode)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Either.Combinators (mapLeft)
import           Data.Text (unpack)
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpJSON,
                                      parseRequest_, setRequestBasicAuth, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))
import qualified Prelude (Show(..))

------------------------------------------------------------
-- JSON parsing exceptions
------------------------------------------------------------
-- | Exceptions that occur during JSON parsing
newtype JSONParsingException = MkJSONParsingException Text

instance Exception JSONParsingException
instance Prelude.Show JSONParsingException where
  show (MkJSONParsingException s) = "JSON parsing exception: " <> (unpack s)

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
addJsonBody :: forall a. ToJSON a => a -> Request -> Either String Request
addJsonBody body req = mapLeft show $ catch updateReq Left
  where
    updateReq :: Exception e => Either e Request
    updateReq = Right <$> setRequestBodyJSON body $ setRequestMethod "PUT" req


-- | Make an api call
-- TODO(ks): Switch to @Either@.
apiCall
    :: forall m a. (MonadIO m, MonadThrow m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m a
apiCall parser req = do
    putTextLn $ show req
    v <- getResponseBody <$> httpJSON req
    case parseEither parser v of
        Right o -> pure o
        Left e -> throwM $ MkJSONParsingException $
            "couldn't parse response " <> toText e <> "\n" <> decodeUtf8 (encode v)

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
