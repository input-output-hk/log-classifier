{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main
  ( Config
  , defaultConfig
  , Comment(..)
  , Attachment(..)
  , TicketId(..)
  , main
  ) where

import           Network.HTTP.Simple         (Request, getResponseBody, httpLBS, parseRequest_, httpJSON, setRequestPath, addRequestHeader, setRequestBasicAuth, setRequestMethod, setRequestBodyJSON)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import           Data.Monoid                 ( (<>) )
import           System.Environment          (getArgs)

import           Data.Aeson                  (FromJSON, Value, parseJSON, withObject, (.:), ToJSON, toJSON, object, (.=) )
import           Data.Aeson.Types (Parser, parseEither)
import           Classify                    (classifyZip, ErrorName, ErrorCode, postProcessError, ConfirmedError(..))
import           Data.Map.Strict      (Map)

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
  , commentPublic :: Bool
  } deriving (Show, Eq)

data Attachment = Attachment
  { attachmentURL :: Text
  , attachmentContentType :: Text
  , attachmentSize :: Int
  } deriving (Show, Eq)

newtype TicketId = TicketId Int deriving (Eq)

instance Show TicketId where
  show (TicketId tid) = show tid

main :: IO ()
main = do
  token <- B8.readFile "token"
  let cfg = defaultConfig { cfgToken = T.stripEnd $ T.decodeUtf8 token }
  agentId <- getAgentId cfg
  ticketIds <- listTicketIds cfg agentId
  --mapM_ (printTicketAndId cfg) (take 10 ticketIds)
  --printTicketAndId cfg $ TicketId 9248
  args <- getArgs
  print args
  case args of
    [ "getTicket", idNumber ] -> do
      printTicketAndId cfg $ TicketId $ read idNumber
    [ "printTickets" ] -> do
      ticketIds <- listAssignedTickets cfg agentId
      print $ "found: " <> (show $ length ticketIds) <> " tickets"
      print ticketIds
      mapM_ (printTicketAndId cfg) ticketIds

printTicketAndId :: Config -> TicketId -> IO ()
printTicketAndId cfg ticketId = do
  comments <- getTicketComments cfg ticketId
  let
    commentsWithAttachments :: [ Comment ]
    commentsWithAttachments = filter (\x -> length (commentAttachments x) > 0) comments
    attachments :: [ Attachment ]
    attachments = concat $ map commentAttachments commentsWithAttachments
    justLogs = filter (\x -> "application/zip" == attachmentContentType x) attachments
  mapM_ (inspectAttachment cfg ticketId) justLogs
  pure ()

inspectAttachment :: Config -> TicketId -> Attachment -> IO ()
inspectAttachment cfg ticketId att = do
  print ticketId
  rawlog <- getAttachment att
  results <- classifyZip rawlog
  case results of
    Left error -> do
      print "error parsing zip"
      print error
    Right result -> do
      case postProcessError result of
        Right (ConfirmedStaleLockFile msg) -> do
          print msg
          postTicketComment cfg ticketId (LT.toStrict msg) False
        Left result -> do
          print result

getAttachment :: Attachment -> IO BL.ByteString
getAttachment Attachment{..} = getResponseBody <$> httpLBS req
  where req = parseRequest_ (T.unpack attachmentURL)

tshow :: Show a => a -> Text
tshow = T.pack . show

apiCall :: FromJSON a => (Value -> Parser a) -> Request -> IO a
apiCall parser req = do
  v <- getResponseBody <$> httpJSON req
  case parseEither parser v of
    Right o -> pure o
    Left e -> error $ "couldn't parse response" ++ show e ++ "\n" ++ show v

listTicketIds :: Config -> Integer -> IO [TicketId]
listTicketIds cfg agentId = do
  let req = apiRequest cfg ("/users/" <> tshow agentId <> "/tickets/requested.json")
  apiCall parseTickets req

listAssignedTickets :: Config -> Integer -> IO [ TicketId ]
listAssignedTickets cfg agentId = do
  let
    req = apiRequest cfg ("/users/" <> tshow agentId <> "/tickets/assigned.json")
  apiCall parseTickets req

parseTickets :: Value -> Parser [TicketId]
parseTickets = withObject "tickets" $ \o -> o .: "tickets"

instance FromJSON TicketId where
  parseJSON = withObject "ticket" $ \o -> TicketId <$> (o .: "id")

parseComments :: Value -> Parser [Comment]
parseComments = withObject "comments" $ \o -> o .: "comments"

postTicketComment :: Config -> TicketId -> Text -> Bool -> IO ()
postTicketComment cfg (TicketId tid) body public = do
  let
    req1 = apiRequest cfg ("tickets/" <> tshow tid <> ".json")
    req2 = addJsonBody (Comment body [] False) req1
  _ <- apiCall (\x -> pure ()) req2
  pure ()

instance FromJSON Comment where
  parseJSON = withObject "comment" $ \o ->
    Comment <$> o .: "body" <*> o .: "attachments" <*> o .: "public"

instance ToJSON Comment where
  toJSON (Comment b as public) = object [ "comment" .= object [ "body" .= b, "attachments" .= as, "public" .= public ] ]

instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \o ->
    Attachment <$> o .: "content_url" <*> o .: "content_type" <*> o .: "size"

instance ToJSON Attachment where
  toJSON (Attachment url contenttype size) = object [ "content_url" .= url, "content_type" .= contenttype, "size" .= size]

getTicketComments :: Config -> TicketId -> IO [Comment]
getTicketComments cfg (TicketId tid) = do
  let req = apiRequest cfg ("tickets/" <> tshow tid <> "/comments.json")
  apiCall parseComments req

addJsonBody :: ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

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
  apiCall parseAgentId req

{-

# download first attachment
curl -o logs.zip -L $(curl https://iohk.zendesk.com/api/v2/tickets/$TICKET_ID/comments.json -H "Content-Type: application/json" -u "$AUTH" | jq -r '.comments[].attachments[].content_url')

curl -o ll.zip -L  'https://iohk.zendesk.com/attachments/token/7adbjqvwvDondyNcW0iVPr9yG/?name=logs.zip' \
  -H "Content-Type: application/json" \
  -u 'daedalus-bug-reports@iohk.io/token:TOKEN'
-}
