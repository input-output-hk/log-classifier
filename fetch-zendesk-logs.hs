{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Monoid
import Data.Function ((&))
import System.Environment (getEnv)

import Control.Exception (throwIO)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)

data Config = Config
              { cfgZendesk :: Text
              , cfgToken :: Text
              , cfgEmail :: Text
              } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config "https://iohk.zendesk.com" "" "daedalus-bug-reports@iohk.io"

data Comment = Comment
  { commentBody :: Text
  , commentAttachments :: [Attachment]
  } deriving (Show, Eq)

data Attachment = Attachment
  { attachmentURL :: Text
  , attachmentContentType :: Text
  , attachmentSize :: Int
  } deriving (Show, Eq)

main :: IO ()
main = do
  token <- T.pack <$> getEnv "TOKEN"
  let cfg = defaultConfig { cfgToken = token }
  agentId <- getAgentId cfg
  ticketIds <- listTicketIds cfg agentId
  tickets <- mapM (getTicketComments cfg) (take 10 ticketIds)
  putStrLn (show tickets)

getAttachment :: Attachment -> IO BL.ByteString
getAttachment Attachment{..} = getResponseBody <$> httpLBS req
  where req = parseRequest_ (T.unpack attachmentURL)

tshow :: Show a => a -> Text
tshow = T.pack . show

newtype TicketId = TicketId Int deriving (Eq)

instance Show TicketId where
  show (TicketId tid) = show tid

listTicketIds :: Config -> Integer -> IO [TicketId]
listTicketIds cfg agentId = do
  let req = apiRequest cfg ("/users/" <> tshow agentId <> "/tickets/requested.json")
  r <- getResponseBody <$> httpJSON req
  case parseMaybe parseTickets r of
    Just id -> pure id
    Nothing -> error "couldn't parse ticket ids"

parseTickets :: Value -> Parser [TicketId]
parseTickets = withObject "tickets" $ \o -> o .: "tickets"

instance FromJSON TicketId where
  parseJSON = withObject "ticket" $ \o -> TicketId <$> (o .: "id")

parseComments :: Value -> Parser [Comment]
parseComments = withObject "comments" $ \o -> o .: "comments"

instance FromJSON Comment where
  parseJSON = withObject "comment" $ \o ->
    Comment <$> o .: "body" <*> o .: "attachments"

instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \o ->
    Attachment <$> o .: "content_url" <*> o .: "content_type" <*> o .: "size"

getTicketComments :: Config -> TicketId -> IO [Comment]
getTicketComments cfg (TicketId tid) = do
  let req = apiRequest cfg ("tickets/" <> tshow tid <> "/comments.json")
  r <- getResponseBody <$> httpJSON req
  case parseMaybe parseComments r of
    Just cs -> pure cs
    Nothing -> error "couldn't parse ticket comments"

apiRequest :: Config -> Text -> Request
apiRequest Config{..} u = setRequestPath (T.encodeUtf8 path) $
                          addRequestHeader "Content-Type" "application/json" $
                          setRequestBasicAuth (T.encodeUtf8 cfgEmail <> "/token") (T.encodeUtf8 cfgToken) $
                          parseRequest_ (T.unpack (cfgZendesk <> path))
  where
    path ="/api/v2/" <> u

parseAgentId :: Value -> Parser Integer
parseAgentId = withObject "user" $ \o -> (o .: "user") >>= (.: "id")

getAgentId :: Config -> IO Integer
getAgentId cfg = do
  let req = apiRequest cfg "users/me.json"
  r <- getResponseBody <$> httpJSON req
  case parseMaybe parseAgentId r of
    Just id -> pure id
    Nothing -> error "couldn't parse agent id"

{-

# download first attachment
curl -o logs.zip -L $(curl https://iohk.zendesk.com/api/v2/tickets/$TICKET_ID/comments.json -H "Content-Type: application/json" -u "$AUTH" | jq -r '.comments[].attachments[].content_url')

curl -o ll.zip -L  'https://iohk.zendesk.com/attachments/token/7adbjqvwvDondyNcW0iVPr9yG/?name=logs.zip' \
  -H "Content-Type: application/json" \
  -u 'daedalus-bug-reports@iohk.io/token:G1pTOWNDC1Tr5rNTOXHJPayK4uE7f15N5DluvHNP'
-}
