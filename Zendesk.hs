{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
    ( Config
    , defaultConfig
    , Attachment(..)
    , TicketId(..)
    , main
    ) where

import           Control.Monad                  (guard)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT, MonadReader)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Aeson                     (FromJSON, ToJSON, Value,
                                                 encode, object, parseJSON,
                                                 toJSON, withObject, (.:), (.=))
import           Data.Aeson.Text                (encodeToLazyText)
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.Attoparsec.Text.Lazy      (eitherResult, parse)
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Map.Strict                (Map)
import           Data.Monoid                    ((<>))
import           Data.List                      (group, sort)
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

import           LogAnalysis.Classifier         (extractErrorCodes,
                                                 extractIssuesFromLogs,
                                                 extractLogsFromZip,
                                                 prettyFormatAnalysis)
import           LogAnalysis.KnowledgeCSVParser (parseKnowLedgeBase)
import           LogAnalysis.Types              (ErrorCode (..), Knowledge,
                                                 setupAnalysis, toComment,
                                                 toTag)
import           Types                          (Attachment (..), Comment (..),
                                                 CommentOuter (..), Ticket (..),
                                                 TicketId, TicketInfo (..),
                                                 TicketList (..),
                                                 TicketStatus (..),
                                                 parseAgentId, parseComments,
                                                 parseTickets)
import           Util                           (tshow)

data Config = Config
    { cfgAgentId            :: Integer
    , cfgZendesk            :: Text
    , cfgToken              :: Text
    , cfgEmail              :: Text
    , cfgAssignTo           :: Integer
    , cfgKnowledgebase      :: [Knowledge]
    , cfgNumOfLogsToAnalyze :: Int
    } deriving (Show, Eq)

data RequestType =
      Requested
    | Assigned

newtype App a = App (ReaderT Config IO a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadReader Config
             , MonadIO)

runApp :: App a -> Config -> IO a
runApp (App a) = runReaderT a

-- | This scirpt will look through tickets that are assigned by cfgEmail
defaultConfig :: Config
defaultConfig = Config 0 "https://iohk.zendesk.com" "" "daedalus-bug-reports@iohk.io" 0 [] 5

-- | Path to knowledgebase
knowledgebasePath :: FilePath
knowledgebasePath = "./knowledgebase/knowledge.csv"

tokenPath :: FilePath
tokenPath = "token"

assignToPath :: FilePath
assignToPath = "assign_to"

main :: IO ()
main = do
  putStrLn "Welcome to Zendesk classifier!"
  token <- B8.readFile tokenPath        -- Zendesk token
  assignto <- B8.readFile assignToPath  -- Select assignee
  knowledges <- setupKnowledgebaseEnv knowledgebasePath
  let cfg' = defaultConfig { cfgToken = T.stripEnd $ T.decodeUtf8 token
                          , cfgAssignTo = read $ T.unpack $ T.decodeUtf8 assignto
                          , cfgKnowledgebase = knowledges
                          }
  agentId <- runApp getAgentId cfg'
  let cfg = cfg' { cfgAgentId = agentId }
  args <- getArgs
  case args of
    -- Process all the tikects that are requested by agent
    [ "extractEmailAddress" ] -> do
      T.putStrLn $  "Classifier is going to extract emails requested by: " <> cfgEmail cfg
      tickets <- runApp (listTickets Requested) cfg
      putStrLn $ "There are " <> show (length tickets) <> " tickets requested by this user."
      let ticketIds = foldr (\TicketInfo{..} acc -> ticketId : acc) [] tickets
      mapM_ (\tid -> runApp (extractEmailAddress tid) cfg) ticketIds
    -- Process given ticket
    [ "processTicket", ticketId ] -> do
      putStrLn "Processing single ticket"
      runApp (processTicketAndId $ read ticketId) cfg
      putStrLn "Process finished, please see the following url"
      putStrLn $ "https://iohk.zendesk.com/agent/tickets/" <> ticketId
    -- Process all the tickets (WARNING: This is really long process)
    [ "processTickets" ] -> do
      T.putStrLn $  "Classifier is going to process tickets assign to: " <> cfgEmail cfg
      printWarning
      tickets <- runApp (listTickets Assigned) cfg
      let filteredTicketIds = filterAnalyzedTickets tickets
      putStrLn $ "There are " <> show (length filteredTicketIds) <> " unanalyzed tickets."
      putStrLn "Processing tickets, this may take hours to finish."
      mapM_ (\tid -> runApp (processTicketAndId tid) cfg) filteredTicketIds
      putStrLn "All the tickets has been processed."
    -- Return raw request
    [ "raw_request", url ] -> do
      let
        req = apiRequest cfg (T.pack url)
      res <- apiCall (pure . encodeToLazyText)  req
      LT.putStrLn res
    -- Count assigned tickets
    [ "showStats" ] -> do
      T.putStrLn $  "Classifier is going to gather ticket information assigned to: " <> cfgEmail cfg
      printWarning
      tickets <- runApp (listTickets Assigned) cfg
      printTicketCountMessage tickets (cfgEmail cfg)
    _  -> do
      let cmdItem = [ "extractEmailAddress : Collect emails requested by single user"
                    , "processTicket <id>  : Process single ticket of id"
                    , "processTickets      : Process all the tickets i.e add comments, tags. WARNING:" <>
                      "This is really long process, please use with care"
                    , "raw_request <url>   : Request raw request to the given url"
                    , "showStats           : Print list of ticket Ids that agent has been assigned"]
      putStrLn "Invalid argument, please add following argument to run the command:"
      mapM_ putStrLn cmdItem

-- | Warning
printWarning :: IO ()
printWarning = putStrLn "Note that this process may take a while. Please do not kill the process"

-- | Print how many tickets are assinged, analyzed, and unanalyzed
printTicketCountMessage :: [ TicketInfo ] -> Text -> IO ()
printTicketCountMessage tickets email = do
  let ticketCount = length tickets
  putStrLn "Done!"
  T.putStrLn $ "There are currently " <> tshow ticketCount
               <> " tickets in the system assigned to " <> email
  let filteredTicketCount = length $ filterAnalyzedTickets tickets
  putStrLn $ show (ticketCount - filteredTicketCount) <> " tickets has been analyzed by the classifier."
  putStrLn $ show filteredTicketCount <> " tickets are not analyzed."
  putStrLn "Below are statistics:"
  let tagGroups = sortTickets tickets
  mapM_ (\(tag, count) -> T.putStrLn $ tag <> ": " <> tshow count) tagGroups

-- | Sort the ticket so we can see the statistics
sortTickets :: [ TicketInfo ] -> [ (Text, Int) ]
sortTickets ts =
    let extractedTags = foldr (\TicketInfo{..} acc -> ticketTags <> acc) [] ts   -- Extract tags from tickets
        filteredTags  = filter (`notElem` ["s3", "s2", "cannot-sync", "closed-by-merge", 
                                           "web_widget", "analyzed-by-script"]) extractedTags -- Filter tags
        groupByTags :: [ Text ] -> [(Text, Int)]
        groupByTags ts = map (\l@(x:xs) -> (x, length l)) (group $ sort ts)          -- Group them
    in  groupByTags filteredTags

-- | Read CSV file and setup knowledge base
setupKnowledgebaseEnv :: FilePath -> IO [ Knowledge ]
setupKnowledgebaseEnv path = do
    kfile <- LT.readFile path
    let kb = parse parseKnowLedgeBase kfile
    case eitherResult kb of
        Left e   -> error e
        Right ks -> return ks

-- | Collect email
extractEmailAddress :: TicketId -> App ()
extractEmailAddress ticketId = do
  comments <- getTicketComments ticketId
  let commentWithEmail = commentBody $ head comments
      emailAddress = head $ T.lines commentWithEmail
  liftIO $ guard ("@" `T.isInfixOf` emailAddress)
  liftIO $ T.appendFile "emailAddress.txt" (emailAddress <> "\n")
  liftIO $ T.putStrLn emailAddress

-- | Process specifig ticket id (can be used for testing) only inspects the one's with logs
processTicketAndId :: TicketId -> App ()
processTicketAndId ticketId = do
  comments <- getTicketComments ticketId
  let
    -- Filter tickets without logs
    -- Could analyze the comments but I don't see it useful..
    commentsWithAttachments :: [ Comment ]
    commentsWithAttachments = filter (\x -> length (commentAttachments x) > 0) comments
    -- Filter out ticket without logs
    attachments :: [ Attachment ]
    attachments = concatMap commentAttachments commentsWithAttachments
    justLogs = filter (\x -> "application/zip" == attachmentContentType x) attachments
  mapM_ (inspectAttachmentAndPostComment ticketId) justLogs
  pure ()

-- | Inspect attachment then post comment to the ticket
inspectAttachmentAndPostComment :: TicketId -> Attachment -> App ()
inspectAttachmentAndPostComment ticketId att = do
  Config{..} <- ask
  liftIO $ putStrLn $ "Analyzing ticket id: " <> show ticketId
  (comment, tags, isPublicComment) <- liftIO $ inspectAttachment
                                                 cfgNumOfLogsToAnalyze
                                                 cfgKnowledgebase
                                                 att
  postTicketComment ticketId comment tags isPublicComment

-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
--
-- The results are following:
--
-- __(comment, tags, bool of whether is should be public comment)__
inspectAttachment :: Int -> [ Knowledge ] -> Attachment -> IO (Text, [ Text ], Bool)
inspectAttachment num ks att = do
  rawlog <- getAttachment att   -- Get attachment
  let results = extractLogsFromZip num rawlog
  case results of
    Left error -> do
      putStrLn "Error parsing zip"
      return (toComment SentLogCorrupted , [toTag SentLogCorrupted], False)
    Right result -> do
      let analysisEnv = setupAnalysis ks
          eitherAnalysisResult = extractIssuesFromLogs result analysisEnv
      case eitherAnalysisResult of
        Right analysisResult -> do -- do something!
          let errorCodes = extractErrorCodes analysisResult
          let commentRes = prettyFormatAnalysis analysisResult
          mapM_ T.putStrLn errorCodes
          return (LT.toStrict commentRes, errorCodes, False)
        Left result -> do
          putStrLn result
          return (LT.toStrict (LT.pack result), [tshow NoKnownIssue], False)

-- | Filter analyzed tickets
filterAnalyzedTickets :: [ TicketInfo ] -> [ TicketId ]
filterAnalyzedTickets = foldr (\TicketInfo{..} acc ->
                                if analyzedIndicatorTag `elem` ticketTags
                                then acc
                                else ticketId : acc
                              ) []
  where analyzedIndicatorTag :: Text
        analyzedIndicatorTag = tshow AnalyzedByScript

-- | Return list of ticketIds that has been requested by config user (not used)
listTickets :: RequestType ->  App [ TicketInfo ]
listTickets request = do
  cfg <- ask
  let agentId = cfgAgentId cfg
      url = case request of
                Requested -> "/users/" <> tshow agentId <> "/tickets/requested.json"
                Assigned  -> "/users/" <> tshow agentId <> "/tickets/assigned.json"
      req = apiRequest cfg url
      go :: [ TicketInfo ] -> Text -> IO [ TicketInfo ]
      go list nextPage' = do
        let req' = apiRequestAbsolute cfg nextPage'
        (TicketList pagen nextPagen) <- apiCall parseTickets req'
        case nextPagen of
          Just url -> go (list <> pagen) url
          Nothing  -> pure (list <> pagen)

  (TicketList page0 nextPage) <- liftIO $ apiCall parseTickets req
  case nextPage of
    Just url -> liftIO $ go page0 url
    Nothing  -> pure page0

-- | Send API request to post comment
postTicketComment :: TicketId -> Text -> [ Text ] -> Bool -> App ()
postTicketComment tid body tags public = do
  cfg <- ask
  let
    req1 = apiRequest cfg ("tickets/" <> tshow tid <> ".json")
    req2 = addJsonBody
             (Ticket
              (Comment ("**Log classifier**\n\n" <> body) [] False (cfgAgentId cfg))
              (cfgAssignTo cfg)
              (tshow AnalyzedByScript:tags)
             )
             req1
  v <- liftIO $ apiCall (pure . encodeToLazyText) req2
  pure ()

-- | Get agent id that has been set on Config
getAgentId :: App Integer
getAgentId = do
  cfg <- ask
  let req = apiRequest cfg "users/me.json"
  liftIO $ apiCall parseAgentId req

-- | Given attachmentUrl, return attachment in bytestring
getAttachment :: Attachment -> IO BL.ByteString
getAttachment Attachment{..} = getResponseBody <$> httpLBS req
  where req = parseRequest_ (T.unpack attachmentURL)

-- | Get ticket's comments
getTicketComments :: TicketId -> App [ Comment ]
getTicketComments tid = do
  cfg <- ask
  let req = apiRequest cfg ("tickets/" <> tshow tid <> "/comments.json")
  liftIO $ apiCall parseComments req

-- | Request PUT
addJsonBody :: ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | Make an api call
apiCall :: FromJSON a => (Value -> Parser a) -> Request -> IO a
apiCall parser req = do
  v <- getResponseBody <$> httpJSON req
  case parseEither parser v of
    Right o -> pure o
    Left e -> error $ "couldn't parse response "
      <> e <> "\n" <> (T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode v)

-- | General api request function
apiRequest :: Config -> Text -> Request
apiRequest Config{..} u = setRequestPath (T.encodeUtf8 path) $
                          addRequestHeader "Content-Type" "application/json" $
                          setRequestBasicAuth
                            (T.encodeUtf8 cfgEmail <> "/token")
                            (T.encodeUtf8 cfgToken) $
                          parseRequest_ (T.unpack (cfgZendesk <> path))
  where
    path ="/api/v2/" <> u

-- | Api request but use absolute path
apiRequestAbsolute :: Config -> Text -> Request
apiRequestAbsolute Config{..} u = addRequestHeader "Content-Type" "application/json" $
                                  setRequestBasicAuth
                                    (T.encodeUtf8 cfgEmail <> "/token")
                                    (T.encodeUtf8 cfgToken) $
                                  parseRequest_ (T.unpack u)
