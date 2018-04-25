{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( Config
  , defaultConfig
  , Attachment(..)
  , TicketId(..)
  , main
  ) where

import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.IO              as LT
import           Network.HTTP.Simple            (Request, addRequestHeader,
                                                 getResponseBody, httpJSON,
                                                 httpLBS, parseRequest_,
                                                 setRequestBasicAuth,
                                                 setRequestBodyJSON,
                                                 setRequestMethod,
                                                 setRequestPath)
import           System.Environment             (getArgs)

import           Data.Aeson                     (FromJSON, ToJSON, Value,
                                                 encode, object, parseJSON,
                                                 toJSON, withObject, (.:), (.=))
import           Data.Aeson.Text                (encodeToLazyText)
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.Map.Strict                (Map)

import           LogAnalysis.Classifier         ( extractIssuesFromLogs
                                                , extractLogsFromZip
                                                , extractErrorCodes
                                                , prettyPrintAnalysis)
import           LogAnalysis.KnowledgeCSVParser (setupKnowledgebaseEnv)
import           LogAnalysis.Types

data Config = Config
              { cfgZendesk            :: Text
              , cfgToken              :: Text
              , cfgEmail              :: Text
              , cfgAssignTo           :: Integer
              , cfgKnowledgebase      :: [Knowledge]
              , cfgNumOfLogsToAnalyze :: Int
              } deriving (Show, Eq)

-- | This scirpt will look through tickets that are assigned by cfgEmail
defaultConfig :: Config
defaultConfig = Config "https://iohk.zendesk.com" "" "daedalus-bug-reports@iohk.io" 0 [] 5

-- | Path to knowledgebase
knowledgebasePath :: FilePath
knowledgebasePath = "./knowledgebase/knowledge.csv"

-- | Comments
data Comment = Comment
  { commentBody        :: Text
  , commentAttachments :: [Attachment]
  , commentPublic      :: Bool
  , commentAuthor      :: Integer
  } deriving (Show, Eq)

-- | Outer comment ??
data CommentOuter = CommentOuter {
    coComment :: Comment
  } deriving (Show, Eq)

-- | Attachment of the ticket
data Attachment = Attachment
  { attachmentURL         :: Text
  , attachmentContentType :: Text
  , attachmentSize        :: Int
  } deriving (Show, Eq)

-- | Zendexk ticket
data Ticket = Ticket {
    ticketComment  :: Comment
  , ticketAssignee :: Integer
  , ticketTag      :: [Text]
  } deriving (Show, Eq)

-- | List of zendesk ticket
data TicketList = TicketList {
    ticketListTickets :: [ TicketId ]
  , next_page         :: Maybe Text
  } deriving (Show, Eq)

newtype TicketId = TicketId Int deriving (Eq)

instance Show TicketId where
  show (TicketId tid) = show tid

data TicketStatus = AnalyzedByScript

instance Show TicketStatus where
  show AnalyzedByScript = "analyzed-by-script"

main :: IO ()
main = do
  token <- B8.readFile "token"        -- Zendesk token
  assignto <- B8.readFile "assign_to" -- Select assignee
  putStrLn "Reading knowledge base"
  knowledges <- setupKnowledgebaseEnv knowledgebasePath
  putStrLn "Knowledgebase setup complete"
  let cfg = defaultConfig { cfgToken = T.stripEnd $ T.decodeUtf8 token
                          , cfgAssignTo = read $ T.unpack $ T.decodeUtf8 assignto
                          , cfgKnowledgebase = knowledges
                          }
  agentId <- getAgentId cfg
  args <- getArgs
  case args of
    [ "processTicket", idNumber ] -> do
      putStrLn "Processing single ticket"
      processTicketAndId cfg agentId $ TicketId $ read idNumber
    [ "listAssigned" ] -> do
      T.putStrLn $  "The script is going to look through tickets assign to: " <> cfgEmail cfg
      tickets <- listAssignedTickets cfg agentId
      T.putStrLn $ "Done: there are currently" <> tshow (length tickets)
                   <> " tickets in the system assigned to " <> cfgEmail cfg
    [ "processTickets" ] -> do
      T.putStrLn $  "The script is going to process tickets assign to: " <> cfgEmail cfg
      ticketIds <- listAssignedTickets cfg agentId
      print $ "found: " <> (show $ length ticketIds) <> " tickets"
      print ticketIds
      mapM_ (processTicketAndId cfg agentId) ticketIds
    [ "raw_request", url ] -> do
      let
        req = apiRequest cfg (T.pack url)
      res <- apiCall (\x -> pure $ (encodeToLazyText x))  req
      LT.putStrLn res
    _  -> do
      let cmdItem = [ "processTicket <id> : Process single ticket of id"
                    , "listAssigned       : Print list of ticket Ids that agent has been assigned"
                    , "processTickets     : Process all the tickets i.e add comments, tags then assign to someone"
                    , "raw_request <url>  : Request raw request to the given url"]
      putStrLn "Invalid argument, please add following argument to run the command:"
      mapM_ putStrLn cmdItem
  putStrLn "Process finished!"

-- | Process specifig ticket id (can be used for testing) only inspects the one's with logs
processTicketAndId :: Config -> Integer -> TicketId -> IO ()
processTicketAndId cfg agentId ticketId = do
  comments <- getTicketComments cfg ticketId
  let
    -- Could implement comment inspection function (although I don't see it useful)
    commentsWithAttachments :: [ Comment ]
    commentsWithAttachments = filter (\x -> length (commentAttachments x) > 0) comments
    -- Filter out ticket without logs
    attachments :: [ Attachment ]
    attachments = concatMap commentAttachments commentsWithAttachments
    justLogs = filter (\x -> "application/zip" == attachmentContentType x) attachments
  mapM_ (inspectAttachment cfg agentId ticketId) justLogs
  pure ()

-- | Inspect the attachment i.e. archived zip file
inspectAttachment :: Config -> Integer -> TicketId -> Attachment -> IO ()
inspectAttachment cfg@Config{..} agentId ticketId att = do
  putStrLn $ "Analyzing ticket id: " <> show ticketId
  rawlog <- getAttachment att   -- Get attachment
  let results = extractLogsFromZip cfgNumOfLogsToAnalyze rawlog
  case results of
    Left error -> do
      print "error parsing zip"
      postTicketComment cfg agentId ticketId
        (LT.toStrict "Log file is corruputed")
        [toTag SentLogCorrupted]
        False
    Right result -> do
      let analysisEnv = setupAnalysis cfgKnowledgebase
          eitherAnalysisResult = extractIssuesFromLogs result analysisEnv
      case eitherAnalysisResult of
        Right analysisResult -> do -- do something!
          let errorCodes = extractErrorCodes analysisResult
          let commentRes = prettyPrintAnalysis analysisResult
          mapM_ print errorCodes
          postTicketComment cfg agentId ticketId
            (LT.toStrict commentRes) 
            errorCodes
            False
        Left result -> do
          putStrLn result
          postTicketComment cfg agentId ticketId 
            (LT.toStrict (LT.pack result)) 
            ["no-known-issue"]
            False

-- | Given attachmentUrl, return attachment in bytestring
getAttachment :: Attachment -> IO BL.ByteString
getAttachment Attachment{..} = getResponseBody <$> httpLBS req
  where req = parseRequest_ (T.unpack attachmentURL)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Make an api call
apiCall :: FromJSON a => (Value -> Parser a) -> Request -> IO a
apiCall parser req = do
  v <- getResponseBody <$> httpJSON req
  case parseEither parser v of
    Right o -> pure o
    Left e -> error $ "couldn't parse response " 
      <> e <> "\n" <> (T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode v)

-- | Return list of ticketIds that has been requested by config user (don't need..?)
listRequestedTicketIds :: Config -> Integer -> IO [TicketId]
listRequestedTicketIds cfg agentId = do
  let req = apiRequest cfg ("/users/" <> tshow agentId <> "/tickets/requested.json")
  (TicketList page0 next_page) <- apiCall parseTickets req
  pure page0

-- | Return list of ticketIds that has been assigned to config user
listAssignedTickets :: Config -> Integer ->  IO [ TicketId ]
listAssignedTickets cfg agentId = do
  let
    req = apiRequest cfg ("/users/" <> tshow agentId <> "/tickets/assigned.json")
    go :: [ TicketId ] -> Text -> IO [ TicketId ]
    go list next_page' = do
      let req' = apiRequestAbsolute cfg next_page'
      (TicketList pagen next_pagen) <- apiCall parseTickets req'
      case next_pagen of
        Just url -> go (list <> pagen) url
        Nothing  -> pure (list <> pagen)

  (TicketList page0 next_page) <- apiCall parseTickets req
  case next_page of
    Just url -> go page0 url
    Nothing  -> pure page0

-- | Parse tickets
parseTickets :: Value -> Parser TicketList
parseTickets = withObject "tickets" $ \o -> TicketList <$> o .: "tickets" <*> o .: "next_page"

instance FromJSON TicketId where
  parseJSON = withObject "ticket" $ \o -> TicketId <$> (o .: "id")

instance FromJSON TicketList where
  parseJSON = withObject "ticketList" $ \o -> TicketList <$> o .: "tickets" <*> o .: "next_page"

-- | Parse comments
parseComments :: Value -> Parser [ Comment ]
parseComments = withObject "comments" $ \o -> o .: "comments"

-- | Send API request to post comment
postTicketComment :: Config -> Integer -> TicketId -> Text -> [Text] -> Bool -> IO ()
postTicketComment cfg agentId (TicketId tid) body tags public = do
  let
    req1 = apiRequest cfg ("tickets/" <> tshow tid <> ".json")
    req2 = addJsonBody (Ticket 
             (Comment body [] False agentId) 
             (cfgAssignTo cfg)
             (tshow AnalyzedByScript:tags))
             req1
  v <- apiCall (\x -> pure $ (encodeToLazyText x)) req2
  pure ()

instance FromJSON Comment where
  parseJSON = withObject "comment" $ \o ->
    Comment <$> o .: "body" <*> o .: "attachments" <*> o .: "public" <*> o .: "author_id"

instance ToJSON Comment where -- Add tag and status
  toJSON (Comment b as public author)
    = object [ "body" .= b, "attachments" .= as,
      "public" .= public, "author_id" .= author]

instance ToJSON CommentOuter where
  toJSON (CommentOuter c) = object [ "comment" .= c ]

instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \o ->
    Attachment <$> o .: "content_url" <*> o .: "content_type" <*> o .: "size"

instance ToJSON Ticket where
  toJSON (Ticket comment assignee tags) = 
    object [ "ticket" .= object [ "comment" .= comment, "assignee_id" .= assignee, "tags" .= tags] ]

instance ToJSON Attachment where
  toJSON (Attachment url contenttype size) = 
    object [ "content_url" .= url, "content_type" .= contenttype, "size" .= size]

-- | Get ticket's comments
getTicketComments :: Config -> TicketId -> IO [ Comment ]
getTicketComments cfg (TicketId tid) = do
  let req = apiRequest cfg ("tickets/" <> tshow tid <> "/comments.json")
  apiCall parseComments req

-- | Request PUT
addJsonBody :: ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | General apiRequest function
apiRequest :: Config -> Text -> Request
apiRequest Config{..} u = setRequestPath (T.encodeUtf8 path) $
                          addRequestHeader "Content-Type" "application/json" $
                          setRequestBasicAuth (T.encodeUtf8 cfgEmail <> "/token") (T.encodeUtf8 cfgToken) $
                          parseRequest_ (T.unpack (cfgZendesk <> path))
  where
    path ="/api/v2/" <> u

apiRequestAbsolute :: Config -> Text -> Request
apiRequestAbsolute Config{..} u = addRequestHeader "Content-Type" "application/json" $
                                  setRequestBasicAuth (T.encodeUtf8 cfgEmail <> "/token") (T.encodeUtf8 cfgToken) $
                                  parseRequest_ (T.unpack u)

-- | Parse the apiRequest of getAgentId
parseAgentId :: Value -> Parser Integer
parseAgentId = withObject "user" $ \o -> (o .: "user") >>= (.: "id")

-- | Get agent id that has been set on Config
getAgentId :: Config -> IO Integer
getAgentId cfg = do
  let req = apiRequest cfg "users/me.json"
  apiCall parseAgentId req

