module Http.Exceptions
    ( HttpNetworkLayerException (..)
    , statusCodeToException
    ) where

import           Universum

import           Network.HTTP.Simple (Request, defaultRequest)

import           Test.QuickCheck (Arbitrary (..), oneof)

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


