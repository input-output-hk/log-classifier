module HttpLayer
    ( HTTPNetworkLayer (..)
    , JSONException (..)
    , basicHTTPNetworkLayer
    , emptyHTTPNetworkLayer
    , apiRequest
    , apiRequestAbsolute
    ) where

import           Universum

import           Control.Exception.Safe (throwM)
import           Data.Aeson (FromJSON, ToJSON, Value)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Text (pack)
import           Network.HTTP.Client.Conduit (parseUrlThrow)
import           Network.HTTP.Simple (JSONException (..), Request, addRequestHeader,
                                      getResponseBody, httpJSON, parseRequest_, setRequestBasicAuth,
                                      setRequestBodyJSON, setRequestMethod, setRequestPath)

import           DataSource.Types (Config (..), HTTPNetworkLayer (..))


------------------------------------------------------------
-- Internal Exceptions
------------------------------------------------------------
newtype APICallException
    = MkAPICallException Text deriving Show

instance Exception APICallException

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
-- This function can throw a 'JSONException' and an 'HttpException'.
apiRequest
    :: MonadThrow m
    => Config
    -> Text
    -> m Request
apiRequest Config{..} u = do
    req <- parseUrlThrow (toString (cfgZendesk <> path)) -- TODO(md): Get a list of exceptions that parseUrlThrow can throw
    return $ setRequestPath (encodeUtf8 path) $
             addRequestHeader "Content-Type" "application/json" $
             setRequestBasicAuth
                 (encodeUtf8 cfgEmail <> "/token")
                 (encodeUtf8 cfgToken) $ req
  where
    -- handlerList :: [Handler m Request]
    -- handlerList = [
    --                 handlerJSON
    --               , handlerHTTP
    --               ]
    -- handlerJSON :: Handler m Request
    -- handlerJSON = Handler $ \(ex :: JSONException) -> throwM ex
    -- handlerHTTP :: Handler m Request
    -- handlerHTTP = Handler $ \(ex :: HttpException) -> throwM ex
    path :: Text
    path = "/api/v2" <> u

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
addJsonBody
    :: forall m a. (ToJSON a, MonadThrow m)
    => a
    -> Request
    -> m Request
addJsonBody body req =
    return <$> setRequestBodyJSON body $ setRequestMethod "PUT" req


-- | Make an api call
apiCall
    :: forall m a. (MonadIO m, MonadThrow m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m a
apiCall parser req = do
    putTextLn $ show req
    v <- getResponseBody <$> httpJSON req
    either (throwEx v) return (parseEither parser v)
  where
    throwEx :: Value -> String -> m a
    throwEx v msg = throwM $ MkAPICallException . pack $
        "Exception occured when parsing " ++ (show v) ++ ": " ++ msg
    -- case parseEither parser v of
    --     Right o -> pure o
    --     Left e -> Left $ MkJSONParsingException $
    --         "couldn't parse response " <> toText e <> "\n" <> decodeUtf8 (encode v)

-- | Make a safe api call.
apiCallSafe
    :: forall m a. (MonadIO m, MonadThrow m, FromJSON a)
    => (Value -> Parser a)
    -> Request
    -> m a
apiCallSafe = apiCall
