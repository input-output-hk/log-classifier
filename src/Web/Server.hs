{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Web.Server
    ( mainServer
    ) where

import           Prelude (Show (..))
import           Universum

import           Data.Aeson

import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant ((:<|>) (..), (:>), Capture, FromHttpApiData (..), Get, JSON, Post,
                          Proxy (..), ReqBody, err404)
import           Servant.Server (Application, Handler (..), Server, serve)

import           DataSource
import           Lib

------------------------------------------------------------
-- API types
------------------------------------------------------------

-- | So we can define the exposed types to the client differently.
-- If we ever want to change the API result types, we can create a
-- new wrapper V2, V3, ..., VN.
newtype V1 a = V1 a deriving (Eq, Ord)

instance Show a => Show (V1 a) where
    show (V1 a) = Prelude.show a

instance Enum a => Enum (V1 a) where
    toEnum x = V1 (toEnum x)
    fromEnum (V1 a) = fromEnum a

instance Bounded a => Bounded (V1 a) where
    minBound = V1 $ minBound @a
    maxBound = V1 $ maxBound @a

--instance (Read a) => FromHttpApiData (V1 a) where
--    parseUrlPiece = fmap V1 . readEither . toString

instance (FromHttpApiData a) => FromHttpApiData (V1 a) where
    parseUrlPiece = fmap V1 . parseUrlPiece

------------------------------------------------------------
-- Client types
------------------------------------------------------------

instance ToJSON (V1 TicketId) where
    toJSON (V1 (TicketId ticketId)) =
        object ["ticketId" .= ticketId]

instance FromJSON (V1 TicketId) where
    parseJSON = withObject "TicketId" $ \o -> V1 . TicketId <$> o .: "ticketId"

instance FromHttpApiData TicketId where
    parseUrlPiece cTicketId = TicketId <$> readEither ( toString cTicketId )

------------------------------------------------------------

-- A simple alias.
type CTicketId      = V1 TicketId
--type CTicketInfo    = V1 TicketInfo
--type CComment       = V1 Comment

------------------------------------------------------------
-- API
------------------------------------------------------------

type API
    =  "api" :> "v1" :> "tickets" :> Capture "ticketId" TicketId :> Get '[JSON] TicketInfo
  :<|> "api" :> "v1" :> "tickets" :> Capture "ticketId" TicketId :> "comments" :> Get '[JSON] [Comment]
  :<|> "api" :> "v1" :> "tickets" :> "analysis" :> ReqBody '[JSON] CTicketId :> Post '[JSON] ZendeskResponse


--type NT source target = forall a. source a -> target a
-- https://kseo.github.io/posts/2017-01-18-natural-transformations-in-servant.html
-- http://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
convert :: forall a. IO a -> Handler a
convert = Handler . ExceptT . try

-- | You provide me with the @Config@ and @DataLayer@ and I'll provide you with the @Server@
-- functionality.
server :: Config -> DataLayer App -> Server API
server config dataLayer =
            handlerGetTicket
    :<|>    handlerGetTicketComments
    :<|>    handlerPostTicketAnalysis
  where
    handlerGetTicket :: TicketId -> Handler TicketInfo
    handlerGetTicket ticketId = convert $ runApp fetchTicketEither config
      where
        fetchTicketEither :: App TicketInfo
        fetchTicketEither = do
            mTicketId   <- fetchTicket dataLayer ticketId

            case mTicketId of
                Nothing         -> throwM err404
                Just ticketId'  -> pure ticketId'

    handlerGetTicketComments :: TicketId -> Handler [Comment]
    handlerGetTicketComments ticketId = convert getTicketComments
      where
        getTicketComments :: IO [Comment]
        getTicketComments = runApp (fetchTicketComments dataLayer ticketId) config

    handlerPostTicketAnalysis :: CTicketId -> Handler ZendeskResponse
    handlerPostTicketAnalysis (V1 ticketId) = convert $ runApp (processTicket dataLayer ticketId) config

-- | Main function call.
mainServer :: IO ()
mainServer = do
    config      <- createConfig
    dataLayer   <- createBasicDataLayerIO config

    run 4000 (logStdoutDev (app config dataLayer))
  where
    app :: Config -> DataLayer App -> Application
    app config dataLayer = serve @API Proxy (server config dataLayer)

