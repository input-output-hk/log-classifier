{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Web.Server
    ( mainFn
    ) where

import           Universum

import           Data.Aeson

import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant (ReqBody, (:<|>) (..), (:>), Capture, FromHttpApiData (..), Get, JSON, Post,
                          Proxy (..))
import           Servant.Server (Application, Handler, Server, serve)

------------------------------------------------------------
-- Client types
------------------------------------------------------------

newtype CTicketId = CTicketId Int
    deriving (Eq, Show, Read)

instance ToJSON CTicketId where
    toJSON (CTicketId ticketId) =
        object ["ticketId" .= ticketId]

instance FromJSON CTicketId where
    parseJSON = withObject "TicketId" $ \o -> CTicketId <$> o .: "ticketId"

instance FromHttpApiData CTicketId where
    parseUrlPiece cTicketId = CTicketId <$> readEither ( toString cTicketId )

------------------------------------------------------------
-- API
------------------------------------------------------------

type API
    =  "api" :> "v1" :> "tickets" :> Capture "ticketId" CTicketId :> Get '[JSON] CTicketId
  :<|> "api" :> "v1" :> "tickets" :> "analysis" :> ReqBody '[JSON] CTicketId :> Post '[JSON] CTicketId

server :: Server API
server =
            handlerGetTicket
    :<|>    handlerPostTicketAnalysis
  where
    handlerGetTicket :: CTicketId -> Handler CTicketId
    handlerGetTicket = return

    handlerPostTicketAnalysis :: CTicketId -> Handler CTicketId
    handlerPostTicketAnalysis = return


mainFn :: IO ()
mainFn = run 4000 ( logStdoutDev app )
  where
    app :: Application
    app = serve @API Proxy server

