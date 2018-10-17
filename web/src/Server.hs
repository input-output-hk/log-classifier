{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server
    ( mainServer
    ) where

import           Prelude (Show (..))
import           Universum

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS

import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant ((:<|>) (..), (:>), BasicAuth, BasicAuthCheck (..), BasicAuthData (..),
                          BasicAuthResult (..), Capture, Context (..), FromHttpApiData (..), Get,
                          JSON, Post, Proxy (..), ReqBody, err404, serveWithContext)
import           Servant.Server (Handler (..), Server)

import           DataSource (App, Comment, Config, DataLayer, TicketId (..), TicketInfo,
                             ZendeskResponse, runApp)
import           Lib (createBasicDataLayerIO, createConfig, fetchTicket, fetchTicketComments,
                      processTicket, processTickets, fetchTickets)

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

-- A general instance with the wrapper.
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

-- A data type we use to store user credentials.
data ApplicationUser = ApplicationUser { _username :: Text, _password :: Text }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- A list of users we use.
newtype ApplicationUsers = ApplicationUsers [ApplicationUser]
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { _userName :: Text }
  deriving (Eq, Show)

------------------------------------------------------------
-- API
------------------------------------------------------------

type BasicAuthURL = BasicAuth "log-classifier" User

type API
    =  BasicAuthURL :> "api" :> "v1" :> "tickets" :> Capture "ticketId" TicketId :> Get '[JSON] TicketInfo
  :<|> BasicAuthURL :> "api" :> "v1" :> "tickets" :> Get '[JSON] [TicketInfo]
  :<|> BasicAuthURL :> "api" :> "v1" :> "tickets" :> Capture "ticketId" TicketId :> "comments" :> Get '[JSON] [Comment]
  :<|> BasicAuthURL :> "api" :> "v1" :> "tickets" :> "analysis" :> Post '[JSON] [ZendeskResponse]
  :<|> BasicAuthURL :> "api" :> "v1" :> "tickets" :> "analysis" :> ReqBody '[JSON] CTicketId :> Post '[JSON] ZendeskResponse

------------------------------------------------------------
-- Server
------------------------------------------------------------

-- type NT source target = forall a. source a -> target a
-- https://kseo.github.io/posts/2017-01-18-natural-transformations-in-servant.html
-- http://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
convert :: forall a. IO a -> Handler a
convert = Handler . ExceptT . try

-- | You provide me with the @Config@ and @DataLayer@ and I'll provide you with the @Server@
-- functionality.
server :: Config -> DataLayer App -> Server API
server config dataLayer =
            handlerGetTicket
    :<|>    handlerGetTickets
    :<|>    handlerGetTicketComments
    :<|>    handlerPostTicketsAnalysis
    :<|>    handlerPostTicketAnalysis
  where
    -- | A handler for fetching the ticket information.
    handlerGetTicket :: User -> TicketId -> Handler TicketInfo
    handlerGetTicket _ ticketId = convert $ runApp fetchTicketEither config
      where
        fetchTicketEither :: App TicketInfo
        fetchTicketEither = do
            mTicketId   <- fetchTicket dataLayer ticketId

            case mTicketId of
                Nothing        -> throwM err404
                Just ticketId' -> pure ticketId'

    handlerGetTickets :: User -> Handler [TicketInfo]
    handlerGetTickets _ = convert $ runApp (fetchTickets dataLayer) config

    -- | A handler for getting ticket comments.
    handlerGetTicketComments :: User -> TicketId -> Handler [Comment]
    handlerGetTicketComments _ ticketId = convert getTicketComments
      where
        getTicketComments :: IO [Comment]
        getTicketComments = runApp (fetchTicketComments dataLayer ticketId) config

    -- | A handler for the actual ticket analysis.
    handlerPostTicketAnalysis :: User -> CTicketId -> Handler ZendeskResponse
    handlerPostTicketAnalysis _ (V1 ticketId) = convert $ runApp (processTicket dataLayer ticketId) config

    handlerPostTicketsAnalysis :: User -> Handler [ZendeskResponse]
    handlerPostTicketsAnalysis _ = convert $ runApp (processTickets dataLayer) config


-- | Main function call.
mainServer :: IO ()
mainServer = do

    config      <- createConfig
    dataLayer   <- createBasicDataLayerIO config

    -- the location where we find our list of users and passwords.
    usersFile   <- BS.readFile "./app_users.json"

    let applicationUsers :: ApplicationUsers
        applicationUsers = fromMaybe (error "No app users!") (decode' usersFile)

    -- create the server
    let serverWithContext =  serveWithContext
                                    apiProxy
                                    (basicAuthServerContext applicationUsers)
                                    (server config dataLayer)

    -- Serve, Servant!
    run 4000 (logStdoutDev serverWithContext)
  where
    -- | The API proxy.
    apiProxy :: Proxy API
    apiProxy = Proxy

    -- | We need to supply our handlers with the right Context.
    basicAuthServerContext :: ApplicationUsers -> Context (BasicAuthCheck User ': '[])
    basicAuthServerContext applicationUsers = ( authCheck applicationUsers ) :. EmptyContext
      where
        -- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
        authCheck :: ApplicationUsers -> BasicAuthCheck User
        authCheck (ApplicationUsers applicationUsers') =

            let check :: BasicAuthData -> IO (BasicAuthResult User)
                check (BasicAuthData username password) =
                    if (ApplicationUser usernameText passwordText) `elem` applicationUsers'
                        then pure (Authorized (User usernameText))
                        else pure Unauthorized
                  where
                    usernameText = decodeUtf8 username
                    passwordText = decodeUtf8 password

            in BasicAuthCheck check

