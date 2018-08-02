module HttpLayer
    ( HTTPNetworkLayer (..)
    , JSONException (..)
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
                                      setRequestMethod, setRequestPath, parseRequest)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))
import qualified Prelude (Show(..))

------------------------------------------------------------
-- JSON parsing exceptions
------------------------------------------------------------
-- | Exceptions that occur during JSON parsing
data JSONException
    = JSONParsingException Text
    | JSONEncodingException Text

instance Exception JSONException
instance Prelude.Show JSONException where
  show (JSONParsingException s)  = "JSON parsing exception: " <> (unpack s)
  show (JSONEncodingException s) = "JSON encoding exception: " <> (unpack s)

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
apiRequest :: forall m. MonadThrow m => Config -> Text -> Either String Request
apiRequest Config{..} u = mapLeft show $ catch buildRequest Left
  where
    buildRequest :: m Request
    buildRequest = do
        req <- parseRequest (toString (cfgZendesk <> path)) -- TODO(md): Get a list of exceptions that parseRequest can throw
        return $ setRequestPath (encodeUtf8 path) $
                 addRequestHeader "Content-Type" "application/json" $
                 setRequestBasicAuth
                     (encodeUtf8 cfgEmail <> "/token")
                     (encodeUtf8 cfgToken) $ req
    catchException :: Exception e => e -> Either String Request
    catchException (JSONParseException s) = Left s
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
    :: forall m a. (MonadIO m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m (Either String a)
apiCall parser req = do
    putTextLn $ show req
    v <- getResponseBody <$> httpJSON req
    pure $ parseEither parser v
    -- case parseEither parser v of
    --     Right o -> pure o
    --     Left e -> Left $ MkJSONParsingException $
    --         "couldn't parse response " <> toText e <> "\n" <> decodeUtf8 (encode v)

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
